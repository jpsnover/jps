//! Expression evaluator for calculated properties
//!
//! Supports:
//! - Property references: `name`, `user.name`, `items[0]`
//! - PowerShell object notation: `$_.name`, `$this.name`, `$_.Name.ToUpper()`
//! - Arithmetic: `+`, `-`, `*`, `/`, `%`
//! - String concatenation: `firstName + " " + lastName`
//! - Comparisons: `==`, `!=`, `>`, `>=`, `<`, `<=`
//! - Functions: `upper(s)`, `lower(s)`, `len(s)`, `round(n)`, `floor(n)`, `ceil(n)`
//! - Method calls: `$_.Name.ToUpper()`, `$_.Handles.floor()`

use crate::debug;
use crate::value::Value;
use serde_json::Value as JsonValue;

/// Token types for expression parsing
#[derive(Debug, Clone, PartialEq)]
enum ExprToken {
    Identifier(String),
    String(String),
    Number(f64),
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Dot,
    LParen,
    RParen,
    LBracket,
    RBracket,
    Comma,
    Eq,
    Ne,
    Gt,
    Ge,
    Lt,
    Le,
    And,
    Or,
    Not,
    Eof,
}

/// Lexer for expressions
struct ExprLexer {
    input: Vec<char>,
    pos: usize,
}

impl ExprLexer {
    fn new(input: &str) -> Self {
        ExprLexer {
            input: input.chars().collect(),
            pos: 0,
        }
    }

    fn current(&self) -> Option<char> {
        self.input.get(self.pos).copied()
    }

    fn peek(&self) -> Option<char> {
        self.input.get(self.pos + 1).copied()
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.current() {
            if c.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn next_token(&mut self) -> Result<ExprToken, String> {
        self.skip_whitespace();

        match self.current() {
            None => Ok(ExprToken::Eof),
            Some('+') => {
                self.advance();
                Ok(ExprToken::Plus)
            }
            Some('-') => {
                self.advance();
                Ok(ExprToken::Minus)
            }
            Some('*') => {
                self.advance();
                Ok(ExprToken::Star)
            }
            Some('/') => {
                self.advance();
                Ok(ExprToken::Slash)
            }
            Some('%') => {
                self.advance();
                Ok(ExprToken::Percent)
            }
            Some('.') => {
                self.advance();
                Ok(ExprToken::Dot)
            }
            Some('(') => {
                self.advance();
                Ok(ExprToken::LParen)
            }
            Some(')') => {
                self.advance();
                Ok(ExprToken::RParen)
            }
            Some('[') => {
                self.advance();
                Ok(ExprToken::LBracket)
            }
            Some(']') => {
                self.advance();
                Ok(ExprToken::RBracket)
            }
            Some(',') => {
                self.advance();
                Ok(ExprToken::Comma)
            }
            Some('=') => {
                self.advance();
                if self.current() == Some('=') {
                    self.advance();
                    Ok(ExprToken::Eq)
                } else {
                    Err("Expected '==' for equality".to_string())
                }
            }
            Some('!') => {
                self.advance();
                if self.current() == Some('=') {
                    self.advance();
                    Ok(ExprToken::Ne)
                } else {
                    Ok(ExprToken::Not)
                }
            }
            Some('>') => {
                self.advance();
                if self.current() == Some('=') {
                    self.advance();
                    Ok(ExprToken::Ge)
                } else {
                    Ok(ExprToken::Gt)
                }
            }
            Some('<') => {
                self.advance();
                if self.current() == Some('=') {
                    self.advance();
                    Ok(ExprToken::Le)
                } else {
                    Ok(ExprToken::Lt)
                }
            }
            Some('&') => {
                self.advance();
                if self.current() == Some('&') {
                    self.advance();
                    Ok(ExprToken::And)
                } else {
                    Err("Expected '&&' for logical and".to_string())
                }
            }
            Some('|') => {
                self.advance();
                if self.current() == Some('|') {
                    self.advance();
                    Ok(ExprToken::Or)
                } else {
                    Err("Unexpected '|' in expression".to_string())
                }
            }
            Some('"') => self.read_double_string(),
            Some('\'') => self.read_single_string(),
            Some(c) if c.is_ascii_digit() => self.read_number(),
            Some(c) if c.is_alphabetic() || c == '_' || c == '$' => self.read_identifier(),
            Some(c) => Err(format!("Unexpected character in expression: '{}'", c)),
        }
    }

    fn read_double_string(&mut self) -> Result<ExprToken, String> {
        self.advance(); // skip opening quote
        let mut s = String::new();

        loop {
            match self.current() {
                None => return Err("Unterminated string".to_string()),
                Some('"') => {
                    self.advance();
                    break;
                }
                Some('\\') => {
                    self.advance();
                    match self.current() {
                        Some('n') => s.push('\n'),
                        Some('t') => s.push('\t'),
                        Some('\\') => s.push('\\'),
                        Some('"') => s.push('"'),
                        Some(c) => s.push(c),
                        None => return Err("Unterminated escape".to_string()),
                    }
                    self.advance();
                }
                Some(c) => {
                    s.push(c);
                    self.advance();
                }
            }
        }

        Ok(ExprToken::String(s))
    }

    fn read_single_string(&mut self) -> Result<ExprToken, String> {
        self.advance(); // skip opening quote
        let mut s = String::new();

        loop {
            match self.current() {
                None => return Err("Unterminated string".to_string()),
                Some('\'') => {
                    if self.peek() == Some('\'') {
                        s.push('\'');
                        self.advance();
                        self.advance();
                    } else {
                        self.advance();
                        break;
                    }
                }
                Some(c) => {
                    s.push(c);
                    self.advance();
                }
            }
        }

        Ok(ExprToken::String(s))
    }

    fn read_number(&mut self) -> Result<ExprToken, String> {
        let mut s = String::new();

        while let Some(c) = self.current() {
            if c.is_ascii_digit() || c == '.' {
                s.push(c);
                self.advance();
            } else {
                break;
            }
        }

        s.parse::<f64>()
            .map(ExprToken::Number)
            .map_err(|_| format!("Invalid number: {}", s))
    }

    fn read_identifier(&mut self) -> Result<ExprToken, String> {
        let mut s = String::new();

        // Handle $ prefix for variables like $_, $this, $varname
        if self.current() == Some('$') {
            s.push('$');
            self.advance();
        }

        while let Some(c) = self.current() {
            if c.is_alphanumeric() || c == '_' {
                s.push(c);
                self.advance();
            } else {
                break;
            }
        }

        Ok(ExprToken::Identifier(s))
    }

    fn tokenize(input: &str) -> Result<Vec<ExprToken>, String> {
        let mut lexer = ExprLexer::new(input);
        let mut tokens = Vec::new();

        loop {
            let token = lexer.next_token()?;
            if token == ExprToken::Eof {
                break;
            }
            tokens.push(token);
        }

        Ok(tokens)
    }
}

/// AST nodes for expressions
#[derive(Debug, Clone)]
enum Expr {
    Literal(JsonValue),
    /// Property path: ["user", "name"] - if first element is "$_" or "$this", starts from current object
    Property(Vec<String>),
    Index(Box<Expr>, Box<Expr>),
    BinaryOp(Box<Expr>, BinOp, Box<Expr>),
    UnaryOp(UnaryOp, Box<Expr>),
    FunctionCall(String, Vec<Expr>),
    /// Method call on an expression: expr.method(args)
    MethodCall(Box<Expr>, String, Vec<Expr>),
}

#[derive(Debug, Clone, Copy)]
enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Gt,
    Ge,
    Lt,
    Le,
    And,
    Or,
}

#[derive(Debug, Clone, Copy)]
enum UnaryOp {
    Neg,
    Not,
}

/// Parser for expressions
struct ExprParser {
    tokens: Vec<ExprToken>,
    pos: usize,
}

impl ExprParser {
    fn new(tokens: Vec<ExprToken>) -> Self {
        ExprParser { tokens, pos: 0 }
    }

    fn current(&self) -> &ExprToken {
        self.tokens.get(self.pos).unwrap_or(&ExprToken::Eof)
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn parse(&mut self) -> Result<Expr, String> {
        self.parse_or()
    }

    fn parse_or(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_and()?;

        while *self.current() == ExprToken::Or {
            self.advance();
            let right = self.parse_and()?;
            left = Expr::BinaryOp(Box::new(left), BinOp::Or, Box::new(right));
        }

        Ok(left)
    }

    fn parse_and(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_equality()?;

        while *self.current() == ExprToken::And {
            self.advance();
            let right = self.parse_equality()?;
            left = Expr::BinaryOp(Box::new(left), BinOp::And, Box::new(right));
        }

        Ok(left)
    }

    fn parse_equality(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_comparison()?;

        loop {
            let op = match self.current() {
                ExprToken::Eq => BinOp::Eq,
                ExprToken::Ne => BinOp::Ne,
                _ => break,
            };
            self.advance();
            let right = self.parse_comparison()?;
            left = Expr::BinaryOp(Box::new(left), op, Box::new(right));
        }

        Ok(left)
    }

    fn parse_comparison(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_additive()?;

        loop {
            let op = match self.current() {
                ExprToken::Gt => BinOp::Gt,
                ExprToken::Ge => BinOp::Ge,
                ExprToken::Lt => BinOp::Lt,
                ExprToken::Le => BinOp::Le,
                _ => break,
            };
            self.advance();
            let right = self.parse_additive()?;
            left = Expr::BinaryOp(Box::new(left), op, Box::new(right));
        }

        Ok(left)
    }

    fn parse_additive(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_multiplicative()?;

        loop {
            let op = match self.current() {
                ExprToken::Plus => BinOp::Add,
                ExprToken::Minus => BinOp::Sub,
                _ => break,
            };
            self.advance();
            let right = self.parse_multiplicative()?;
            left = Expr::BinaryOp(Box::new(left), op, Box::new(right));
        }

        Ok(left)
    }

    fn parse_multiplicative(&mut self) -> Result<Expr, String> {
        let mut left = self.parse_unary()?;

        loop {
            let op = match self.current() {
                ExprToken::Star => BinOp::Mul,
                ExprToken::Slash => BinOp::Div,
                ExprToken::Percent => BinOp::Mod,
                _ => break,
            };
            self.advance();
            let right = self.parse_unary()?;
            left = Expr::BinaryOp(Box::new(left), op, Box::new(right));
        }

        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<Expr, String> {
        match self.current() {
            ExprToken::Minus => {
                self.advance();
                let expr = self.parse_unary()?;
                Ok(Expr::UnaryOp(UnaryOp::Neg, Box::new(expr)))
            }
            ExprToken::Not => {
                self.advance();
                let expr = self.parse_unary()?;
                Ok(Expr::UnaryOp(UnaryOp::Not, Box::new(expr)))
            }
            _ => self.parse_postfix(),
        }
    }

    fn parse_postfix(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_primary()?;

        loop {
            match self.current() {
                ExprToken::Dot => {
                    self.advance();
                    if let ExprToken::Identifier(name) = self.current().clone() {
                        self.advance();
                        // Check if it's a method call (followed by `(`)
                        if *self.current() == ExprToken::LParen {
                            self.advance(); // consume `(`
                            let args = self.parse_args()?;
                            if *self.current() != ExprToken::RParen {
                                return Err("Expected ')' after method arguments".to_string());
                            }
                            self.advance(); // consume `)`
                            expr = Expr::MethodCall(Box::new(expr), name, args);
                        } else {
                            // Property access
                            match expr {
                                Expr::Property(mut path) => {
                                    path.push(name);
                                    expr = Expr::Property(path);
                                }
                                other => {
                                    // Property access on non-path expr
                                    // Treat as 0-arg method call (handles Length, Count, etc.)
                                    expr = Expr::MethodCall(Box::new(other), name, vec![]);
                                }
                            }
                        }
                    } else {
                        return Err("Expected property name after '.'".to_string());
                    }
                }
                ExprToken::LBracket => {
                    self.advance();
                    let index = self.parse()?;
                    if *self.current() != ExprToken::RBracket {
                        return Err("Expected ']'".to_string());
                    }
                    self.advance();
                    expr = Expr::Index(Box::new(expr), Box::new(index));
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, String> {
        match self.current().clone() {
            ExprToken::Number(n) => {
                self.advance();
                Ok(Expr::Literal(JsonValue::Number(
                    serde_json::Number::from_f64(n).unwrap_or_else(|| serde_json::Number::from(n as i64)),
                )))
            }
            ExprToken::String(s) => {
                self.advance();
                Ok(Expr::Literal(JsonValue::String(s)))
            }
            ExprToken::Identifier(name) => {
                self.advance();

                // $_ and $this refer to the current object (the whole thing)
                if name == "$_" || name == "$this" {
                    return Ok(Expr::Property(vec![name]));
                }

                // Other $variable references
                if name.starts_with('$') {
                    return Ok(Expr::Property(vec![name]));
                }

                // Check for function call
                if *self.current() == ExprToken::LParen {
                    self.advance();
                    let args = self.parse_args()?;
                    if *self.current() != ExprToken::RParen {
                        return Err("Expected ')' after function arguments".to_string());
                    }
                    self.advance();
                    Ok(Expr::FunctionCall(name, args))
                } else if name == "true" {
                    Ok(Expr::Literal(JsonValue::Bool(true)))
                } else if name == "false" {
                    Ok(Expr::Literal(JsonValue::Bool(false)))
                } else if name == "null" {
                    Ok(Expr::Literal(JsonValue::Null))
                } else {
                    // Property reference
                    Ok(Expr::Property(vec![name]))
                }
            }
            ExprToken::LParen => {
                self.advance();
                let expr = self.parse()?;
                if *self.current() != ExprToken::RParen {
                    return Err("Expected ')'".to_string());
                }
                self.advance();
                Ok(expr)
            }
            _ => Err(format!("Unexpected token: {:?}", self.current())),
        }
    }

    fn parse_args(&mut self) -> Result<Vec<Expr>, String> {
        let mut args = Vec::new();

        if *self.current() == ExprToken::RParen {
            return Ok(args);
        }

        args.push(self.parse()?);

        while *self.current() == ExprToken::Comma {
            self.advance();
            args.push(self.parse()?);
        }

        Ok(args)
    }
}

/// Evaluate an expression against an object
pub fn evaluate(expr_str: &str, obj: &Value) -> Result<JsonValue, String> {
    debug::log(&format!("Expression: Evaluating '{}'", expr_str));
    debug::indent();

    let tokens = ExprLexer::tokenize(expr_str)?;
    debug::log(&format!("Expression: {} tokens", tokens.len()));

    let mut parser = ExprParser::new(tokens);
    let expr = parser.parse()?;
    debug::log(&format!("Expression: Parsed AST: {:?}", expr));

    let result = eval_expr(&expr, obj);

    if let Ok(ref val) = result {
        debug::log(&format!("Expression: Result = {:?}", val));
    }

    debug::dedent();
    result
}

fn eval_expr(expr: &Expr, obj: &Value) -> Result<JsonValue, String> {
    match expr {
        Expr::Literal(v) => Ok(v.clone()),

        Expr::Property(path) => {
            // If path starts with $_ or $this, use the entire current object as base
            let (mut current, start_idx) = if path.first().map(|s| s == "$_" || s == "$this").unwrap_or(false) {
                (obj.inner().clone(), 1)
            } else if path.first().map(|s| s.starts_with('$')).unwrap_or(false) {
                // Other $variables - for now treat as null (no variable store)
                (JsonValue::Null, 1)
            } else {
                (obj.inner().clone(), 0)
            };

            for part in &path[start_idx..] {
                let part_lower = part.to_lowercase();
                // Handle virtual properties: Length and Count
                if part_lower == "length" || part_lower == "count" {
                    match &current {
                        JsonValue::String(s) => {
                            current = JsonValue::Number((s.chars().count() as i64).into());
                            continue;
                        }
                        JsonValue::Array(arr) => {
                            current = JsonValue::Number((arr.len() as i64).into());
                            continue;
                        }
                        _ => {}
                    }
                }
                current = get_property_case_insensitive(&current, part)
                    .unwrap_or(JsonValue::Null);
            }
            Ok(current)
        }

        Expr::Index(base, index) => {
            let base_val = eval_expr(base, obj)?;
            let index_val = eval_expr(index, obj)?;

            match (&base_val, &index_val) {
                (JsonValue::Array(arr), JsonValue::Number(n)) => {
                    let idx = n.as_u64().unwrap_or(0) as usize;
                    Ok(arr.get(idx).cloned().unwrap_or(JsonValue::Null))
                }
                (JsonValue::Object(_), JsonValue::String(key)) => {
                    Ok(get_property_case_insensitive(&base_val, key).unwrap_or(JsonValue::Null))
                }
                _ => Ok(JsonValue::Null),
            }
        }

        Expr::BinaryOp(left, op, right) => {
            let left_val = eval_expr(left, obj)?;
            let right_val = eval_expr(right, obj)?;
            eval_binary_op(&left_val, *op, &right_val)
        }

        Expr::UnaryOp(op, expr) => {
            let val = eval_expr(expr, obj)?;
            eval_unary_op(*op, &val)
        }

        Expr::FunctionCall(name, args) => {
            let arg_vals: Result<Vec<JsonValue>, String> =
                args.iter().map(|a| eval_expr(a, obj)).collect();
            eval_function(name, &arg_vals?)
        }

        Expr::MethodCall(base_expr, method, args) => {
            let base_val = eval_expr(base_expr, obj)?;
            let arg_vals: Result<Vec<JsonValue>, String> =
                args.iter().map(|a| eval_expr(a, obj)).collect();
            eval_method_call(&base_val, method, &arg_vals?)
        }
    }
}

fn get_property_case_insensitive(obj: &JsonValue, name: &str) -> Option<JsonValue> {
    if let JsonValue::Object(map) = obj {
        let name_lower = name.to_lowercase();
        for (k, v) in map {
            if k.to_lowercase() == name_lower {
                return Some(v.clone());
            }
        }
    }
    None
}

fn eval_binary_op(left: &JsonValue, op: BinOp, right: &JsonValue) -> Result<JsonValue, String> {
    match op {
        BinOp::Add => {
            // String concatenation or numeric addition
            match (left, right) {
                (JsonValue::String(a), JsonValue::String(b)) => {
                    Ok(JsonValue::String(format!("{}{}", a, b)))
                }
                (JsonValue::String(a), b) => {
                    Ok(JsonValue::String(format!("{}{}", a, json_to_string(b))))
                }
                (a, JsonValue::String(b)) => {
                    Ok(JsonValue::String(format!("{}{}", json_to_string(a), b)))
                }
                _ => {
                    let a = json_to_f64(left);
                    let b = json_to_f64(right);
                    Ok(f64_to_json(a + b))
                }
            }
        }
        BinOp::Sub => {
            let a = json_to_f64(left);
            let b = json_to_f64(right);
            Ok(f64_to_json(a - b))
        }
        BinOp::Mul => {
            let a = json_to_f64(left);
            let b = json_to_f64(right);
            Ok(f64_to_json(a * b))
        }
        BinOp::Div => {
            let a = json_to_f64(left);
            let b = json_to_f64(right);
            if b == 0.0 {
                Ok(JsonValue::Null)
            } else {
                Ok(f64_to_json(a / b))
            }
        }
        BinOp::Mod => {
            let a = json_to_f64(left);
            let b = json_to_f64(right);
            if b == 0.0 {
                Ok(JsonValue::Null)
            } else {
                Ok(f64_to_json(a % b))
            }
        }
        BinOp::Eq => Ok(JsonValue::Bool(json_equals(left, right))),
        BinOp::Ne => Ok(JsonValue::Bool(!json_equals(left, right))),
        BinOp::Gt => Ok(JsonValue::Bool(json_compare(left, right) > 0)),
        BinOp::Ge => Ok(JsonValue::Bool(json_compare(left, right) >= 0)),
        BinOp::Lt => Ok(JsonValue::Bool(json_compare(left, right) < 0)),
        BinOp::Le => Ok(JsonValue::Bool(json_compare(left, right) <= 0)),
        BinOp::And => {
            let a = json_to_bool(left);
            let b = json_to_bool(right);
            Ok(JsonValue::Bool(a && b))
        }
        BinOp::Or => {
            let a = json_to_bool(left);
            let b = json_to_bool(right);
            Ok(JsonValue::Bool(a || b))
        }
    }
}

fn eval_unary_op(op: UnaryOp, val: &JsonValue) -> Result<JsonValue, String> {
    match op {
        UnaryOp::Neg => Ok(f64_to_json(-json_to_f64(val))),
        UnaryOp::Not => Ok(JsonValue::Bool(!json_to_bool(val))),
    }
}

/// Evaluate a method call on a value (e.g., value.ToUpper(), value.len())
fn eval_method_call(base: &JsonValue, method: &str, args: &[JsonValue]) -> Result<JsonValue, String> {
    let method_lower = method.to_lowercase();

    match method_lower.as_str() {
        "toupper" | "upper" => {
            Ok(JsonValue::String(json_to_string(base).to_uppercase()))
        }
        "tolower" | "lower" => {
            Ok(JsonValue::String(json_to_string(base).to_lowercase()))
        }
        "trim" | "trimstart" => {
            Ok(JsonValue::String(json_to_string(base).trim().to_string()))
        }
        "trimend" => {
            Ok(JsonValue::String(json_to_string(base).trim_end().to_string()))
        }
        "length" | "count" | "len" => {
            match base {
                JsonValue::String(s) => Ok(JsonValue::Number((s.chars().count() as i64).into())),
                JsonValue::Array(arr) => Ok(JsonValue::Number((arr.len() as i64).into())),
                JsonValue::Object(map) => Ok(JsonValue::Number((map.len() as i64).into())),
                _ => Ok(JsonValue::Number(0.into())),
            }
        }
        "round" => {
            let n = json_to_f64(base);
            let decimals = args.first().map(json_to_f64).unwrap_or(0.0) as i32;
            let factor = 10_f64.powi(decimals);
            Ok(f64_to_json((n * factor).round() / factor))
        }
        "floor" => Ok(f64_to_json(json_to_f64(base).floor())),
        "ceiling" | "ceil" => Ok(f64_to_json(json_to_f64(base).ceil())),
        "abs" => Ok(f64_to_json(json_to_f64(base).abs())),
        "sqrt" => Ok(f64_to_json(json_to_f64(base).sqrt())),
        "substring" | "substr" => {
            let s = json_to_string(base);
            let start = args.first().map(json_to_f64).unwrap_or(0.0) as usize;
            let len = args.get(1).map(json_to_f64).map(|n| n as usize);
            let chars: Vec<char> = s.chars().collect();
            let end = len.map(|l| (start + l).min(chars.len())).unwrap_or(chars.len());
            let result: String = chars.get(start..end).unwrap_or(&[]).iter().collect();
            Ok(JsonValue::String(result))
        }
        "split" => {
            let s = json_to_string(base);
            let delim = args.first().map(json_to_string).unwrap_or_else(|| " ".to_string());
            let parts: Vec<JsonValue> = s.split(delim.as_str())
                .map(|p| JsonValue::String(p.to_string()))
                .collect();
            Ok(JsonValue::Array(parts))
        }
        "tostring" => Ok(JsonValue::String(json_to_string(base))),
        "startswith" => {
            let s = json_to_string(base);
            let prefix = args.first().map(json_to_string).unwrap_or_default();
            Ok(JsonValue::Bool(s.starts_with(prefix.as_str())))
        }
        "endswith" => {
            let s = json_to_string(base);
            let suffix = args.first().map(json_to_string).unwrap_or_default();
            Ok(JsonValue::Bool(s.ends_with(suffix.as_str())))
        }
        "contains" => {
            let s = json_to_string(base);
            let needle = args.first().map(json_to_string).unwrap_or_default();
            Ok(JsonValue::Bool(s.contains(needle.as_str())))
        }
        "replace" => {
            let s = json_to_string(base);
            let old = args.first().map(json_to_string).unwrap_or_default();
            let new_val = args.get(1).map(json_to_string).unwrap_or_default();
            Ok(JsonValue::String(s.replace(old.as_str(), new_val.as_str())))
        }
        "indexof" => {
            let s = json_to_string(base);
            let needle = args.first().map(json_to_string).unwrap_or_default();
            let idx = s.find(needle.as_str()).map(|i| i as i64).unwrap_or(-1);
            Ok(JsonValue::Number(idx.into()))
        }
        "padleft" => {
            let s = json_to_string(base);
            let width = args.first().map(json_to_f64).unwrap_or(0.0) as usize;
            let pad_char = args.get(1).map(json_to_string).and_then(|s| s.chars().next()).unwrap_or(' ');
            Ok(JsonValue::String(format!("{:>width$}", s, width = width).replace(' ', &pad_char.to_string())))
        }
        "padright" => {
            let s = json_to_string(base);
            let width = args.first().map(json_to_f64).unwrap_or(0.0) as usize;
            let pad_char = args.get(1).map(json_to_string).and_then(|s| s.chars().next()).unwrap_or(' ');
            Ok(JsonValue::String(format!("{:<width$}", s, width = width).replace(' ', &pad_char.to_string())))
        }
        _ => {
            // Unknown method - try as property access (handles cases like .Name on object)
            if args.is_empty() {
                match base {
                    JsonValue::Object(_) => {
                        Ok(get_property_case_insensitive(base, method).unwrap_or(JsonValue::Null))
                    }
                    _ => Err(format!("Unknown method '{}' on {:?}", method, base))
                }
            } else {
                Err(format!("Unknown method: {}", method))
            }
        }
    }
}

fn eval_function(name: &str, args: &[JsonValue]) -> Result<JsonValue, String> {
    let name_lower = name.to_lowercase();

    match name_lower.as_str() {
        "upper" | "toupper" => {
            if args.is_empty() {
                return Err("upper() requires 1 argument".to_string());
            }
            Ok(JsonValue::String(json_to_string(&args[0]).to_uppercase()))
        }
        "lower" | "tolower" => {
            if args.is_empty() {
                return Err("lower() requires 1 argument".to_string());
            }
            Ok(JsonValue::String(json_to_string(&args[0]).to_lowercase()))
        }
        "len" | "length" => {
            if args.is_empty() {
                return Err("len() requires 1 argument".to_string());
            }
            match &args[0] {
                JsonValue::String(s) => Ok(JsonValue::Number((s.len() as i64).into())),
                JsonValue::Array(arr) => Ok(JsonValue::Number((arr.len() as i64).into())),
                _ => Ok(JsonValue::Number(0.into())),
            }
        }
        "round" => {
            if args.is_empty() {
                return Err("round() requires 1 argument".to_string());
            }
            let n = json_to_f64(&args[0]);
            let decimals = args.get(1).map(json_to_f64).unwrap_or(0.0) as i32;
            let factor = 10_f64.powi(decimals);
            Ok(f64_to_json((n * factor).round() / factor))
        }
        "floor" => {
            if args.is_empty() {
                return Err("floor() requires 1 argument".to_string());
            }
            Ok(f64_to_json(json_to_f64(&args[0]).floor()))
        }
        "ceil" | "ceiling" => {
            if args.is_empty() {
                return Err("ceil() requires 1 argument".to_string());
            }
            Ok(f64_to_json(json_to_f64(&args[0]).ceil()))
        }
        "abs" => {
            if args.is_empty() {
                return Err("abs() requires 1 argument".to_string());
            }
            Ok(f64_to_json(json_to_f64(&args[0]).abs()))
        }
        "sqrt" => {
            if args.is_empty() {
                return Err("sqrt() requires 1 argument".to_string());
            }
            Ok(f64_to_json(json_to_f64(&args[0]).sqrt()))
        }
        "trim" => {
            if args.is_empty() {
                return Err("trim() requires 1 argument".to_string());
            }
            Ok(JsonValue::String(json_to_string(&args[0]).trim().to_string()))
        }
        "substring" | "substr" => {
            if args.is_empty() {
                return Err("substring() requires at least 1 argument".to_string());
            }
            let s = json_to_string(&args[0]);
            let start = args.get(1).map(json_to_f64).unwrap_or(0.0) as usize;
            let len = args.get(2).map(json_to_f64).map(|n| n as usize);

            let chars: Vec<char> = s.chars().collect();
            let end = len.map(|l| (start + l).min(chars.len())).unwrap_or(chars.len());
            let result: String = chars.get(start..end).unwrap_or(&[]).iter().collect();
            Ok(JsonValue::String(result))
        }
        "concat" => {
            let result: String = args.iter().map(json_to_string).collect();
            Ok(JsonValue::String(result))
        }
        "coalesce" | "ifnull" => {
            for arg in args {
                if !arg.is_null() {
                    return Ok(arg.clone());
                }
            }
            Ok(JsonValue::Null)
        }
        "if" | "iif" => {
            if args.len() < 3 {
                return Err("if() requires 3 arguments: if(condition, then, else)".to_string());
            }
            if json_to_bool(&args[0]) {
                Ok(args[1].clone())
            } else {
                Ok(args[2].clone())
            }
        }
        _ => Err(format!("Unknown function: {}", name)),
    }
}

pub fn json_to_f64(v: &JsonValue) -> f64 {
    match v {
        JsonValue::Number(n) => n.as_f64().unwrap_or(0.0),
        JsonValue::String(s) => s.parse().unwrap_or(0.0),
        JsonValue::Bool(b) => if *b { 1.0 } else { 0.0 },
        _ => 0.0,
    }
}

pub fn json_to_string(v: &JsonValue) -> String {
    match v {
        JsonValue::String(s) => s.clone(),
        JsonValue::Number(n) => n.to_string(),
        JsonValue::Bool(b) => b.to_string(),
        JsonValue::Null => String::new(),
        JsonValue::Array(_) | JsonValue::Object(_) => {
            serde_json::to_string(v).unwrap_or_default()
        }
    }
}

pub fn json_to_bool(v: &JsonValue) -> bool {
    match v {
        JsonValue::Bool(b) => *b,
        JsonValue::Number(n) => n.as_f64().unwrap_or(0.0) != 0.0,
        JsonValue::String(s) => !s.is_empty(),
        JsonValue::Null => false,
        JsonValue::Array(arr) => !arr.is_empty(),
        JsonValue::Object(obj) => !obj.is_empty(),
    }
}

fn json_equals(a: &JsonValue, b: &JsonValue) -> bool {
    match (a, b) {
        (JsonValue::String(a), JsonValue::String(b)) => a.to_lowercase() == b.to_lowercase(),
        (JsonValue::Number(a), JsonValue::Number(b)) => {
            a.as_f64().unwrap_or(0.0) == b.as_f64().unwrap_or(0.0)
        }
        _ => a == b,
    }
}

fn json_compare(a: &JsonValue, b: &JsonValue) -> i32 {
    match (a, b) {
        (JsonValue::Number(a), JsonValue::Number(b)) => {
            let af = a.as_f64().unwrap_or(0.0);
            let bf = b.as_f64().unwrap_or(0.0);
            if af < bf { -1 } else if af > bf { 1 } else { 0 }
        }
        (JsonValue::String(a), JsonValue::String(b)) => {
            a.to_lowercase().cmp(&b.to_lowercase()) as i32
        }
        _ => 0,
    }
}

pub fn f64_to_json(n: f64) -> JsonValue {
    if n.fract() == 0.0 && n.abs() < i64::MAX as f64 {
        JsonValue::Number((n as i64).into())
    } else {
        serde_json::Number::from_f64(n)
            .map(JsonValue::Number)
            .unwrap_or(JsonValue::Null)
    }
}

/// Represents a calculated property definition
#[derive(Debug, Clone)]
pub struct CalculatedProperty {
    pub name: Option<String>,
    pub expression: String,
    pub width: Option<usize>,
    pub alignment: Option<Alignment>,
    pub format_string: Option<String>,
    pub descending: Option<bool>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Alignment {
    Left,
    Center,
    Right,
}

impl CalculatedProperty {
    /// Evaluate this calculated property against an object
    pub fn evaluate(&self, obj: &Value) -> Result<JsonValue, String> {
        evaluate(&self.expression, obj)
    }

    /// Get the output name for this property
    pub fn output_name(&self) -> String {
        self.name.clone().unwrap_or_else(|| self.expression.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn eval(expr: &str, obj: serde_json::Value) -> JsonValue {
        evaluate(expr, &Value::new(obj)).unwrap()
    }

    #[test]
    fn test_property_access() {
        assert_eq!(eval("name", json!({"name": "alice"})), json!("alice"));
        assert_eq!(eval("age", json!({"age": 30})), json!(30));
    }

    #[test]
    fn test_nested_property() {
        assert_eq!(
            eval("user.name", json!({"user": {"name": "alice"}})),
            json!("alice")
        );
    }

    #[test]
    fn test_arithmetic() {
        assert_eq!(eval("price * quantity", json!({"price": 10, "quantity": 5})), json!(50));
        assert_eq!(eval("a + b", json!({"a": 10, "b": 20})), json!(30));
        assert_eq!(eval("a - b", json!({"a": 30, "b": 10})), json!(20));
        assert_eq!(eval("a / b", json!({"a": 20, "b": 4})), json!(5));
        assert_eq!(eval("a % b", json!({"a": 17, "b": 5})), json!(2));
    }

    #[test]
    fn test_string_concat() {
        assert_eq!(
            eval("firstName + ' ' + lastName", json!({"firstName": "John", "lastName": "Doe"})),
            json!("John Doe")
        );
    }

    #[test]
    fn test_functions() {
        assert_eq!(eval("upper(name)", json!({"name": "alice"})), json!("ALICE"));
        assert_eq!(eval("lower(name)", json!({"name": "ALICE"})), json!("alice"));
        assert_eq!(eval("len(name)", json!({"name": "alice"})), json!(5));
        assert_eq!(eval("round(3.7)", json!({})), json!(4));
        assert_eq!(eval("floor(3.7)", json!({})), json!(3));
        assert_eq!(eval("ceil(3.2)", json!({})), json!(4));
    }

    #[test]
    fn test_comparisons() {
        assert_eq!(eval("age > 25", json!({"age": 30})), json!(true));
        assert_eq!(eval("age < 25", json!({"age": 30})), json!(false));
        assert_eq!(eval("name == 'alice'", json!({"name": "alice"})), json!(true));
    }

    #[test]
    fn test_if_function() {
        assert_eq!(
            eval("if(age >= 18, 'adult', 'minor')", json!({"age": 25})),
            json!("adult")
        );
        assert_eq!(
            eval("if(age >= 18, 'adult', 'minor')", json!({"age": 15})),
            json!("minor")
        );
    }

    #[test]
    fn test_coalesce() {
        assert_eq!(eval("coalesce(nickname, name)", json!({"name": "alice"})), json!("alice"));
        assert_eq!(
            eval("coalesce(nickname, name)", json!({"nickname": "ally", "name": "alice"})),
            json!("ally")
        );
    }

    #[test]
    fn test_current_object_reference() {
        // $_ refers to the whole object
        let obj = json!({"name": "alice", "age": 30});
        // $_.name should return "alice"
        assert_eq!(eval("$_.name", obj.clone()), json!("alice"));
        // $this.age should return 30
        assert_eq!(eval("$this.age", obj.clone()), json!(30));
    }

    #[test]
    fn test_method_calls() {
        assert_eq!(eval("$_.name.ToUpper()", json!({"name": "alice"})), json!("ALICE"));
        assert_eq!(eval("$_.name.ToLower()", json!({"name": "ALICE"})), json!("alice"));
        assert_eq!(eval("$_.name.Trim()", json!({"name": "  alice  "})), json!("alice"));
        assert_eq!(eval("$_.name.Length", json!({"name": "alice"})), json!(5));
        assert_eq!(eval("$_.items.Count", json!({"items": [1, 2, 3]})), json!(3));
    }

    #[test]
    fn test_method_calls_on_value() {
        // Method calls work on property values too
        assert_eq!(eval("name.ToUpper()", json!({"name": "alice"})), json!("ALICE"));
    }

    #[test]
    fn test_arithmetic_with_dollar() {
        assert_eq!(eval("$_.price * $_.quantity", json!({"price": 10, "quantity": 5})), json!(50));
    }
}
