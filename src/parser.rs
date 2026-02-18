use crate::ast::{Argument, ArgumentValue, CalculatedPropertyDef, Command, Pipeline};
use crate::debug;
use crate::lexer::{Lexer, Token};

/// Parser for PowerShell-like script syntax
pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, pos: 0 }
    }

    fn current(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn advance(&mut self) -> Option<&Token> {
        let tok = self.tokens.get(self.pos);
        self.pos += 1;
        tok
    }

    #[allow(dead_code)]
    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos + 1)
    }

    /// Parse a complete script (may be wrapped in braces)
    pub fn parse(&mut self) -> Result<Pipeline, String> {
        // If the first token is a ScriptBlock (from `{ ... }` outer wrapper),
        // re-lex its content and parse that as the pipeline.
        if let Some(Token::ScriptBlock(content)) = self.current() {
            let content = content.clone();
            self.advance();
            let tokens = Lexer::tokenize(&content)?;
            let mut inner = Parser::new(tokens);
            return inner.parse_pipeline();
        }

        // Skip optional opening brace (legacy path)
        if matches!(self.current(), Some(Token::OpenBrace)) {
            self.advance();
        }

        let pipeline = self.parse_pipeline()?;

        // Skip optional closing brace (legacy path)
        if matches!(self.current(), Some(Token::CloseBrace)) {
            self.advance();
        }

        Ok(pipeline)
    }

    /// Parse a pipeline (sequence of commands separated by |)
    fn parse_pipeline(&mut self) -> Result<Pipeline, String> {
        let mut commands = Vec::new();

        // Parse first command
        if let Some(cmd) = self.parse_command()? {
            commands.push(cmd);
        }

        // Parse subsequent commands
        while matches!(self.current(), Some(Token::Pipe)) {
            self.advance(); // consume |
            if let Some(cmd) = self.parse_command()? {
                commands.push(cmd);
            }
        }

        Ok(Pipeline { commands })
    }

    /// Parse a single command with its arguments
    fn parse_command(&mut self) -> Result<Option<Command>, String> {
        // Get command name
        let name = match self.current() {
            Some(Token::Identifier(name)) => {
                let name = name.clone();
                self.advance();
                name
            }
            Some(Token::Pipe) | Some(Token::CloseBrace) | None => return Ok(None),
            Some(tok) => return Err(format!("Expected command name, found {:?}", tok)),
        };

        debug::log(&format!("Parser: Parsing command '{}'", name));
        debug::indent();

        // Parse arguments
        let arguments = self.parse_arguments()?;

        debug::log(&format!("Parser: Command '{}' has {} argument(s)", name, arguments.len()));
        for (i, arg) in arguments.iter().enumerate() {
            match arg {
                Argument::Parameter { name: pname, value } => {
                    debug::log(&format!(
                        "  Arg[{}]: Parameter '-{}' = {:?}",
                        i, pname, value
                    ));
                }
                Argument::Positional(val) => {
                    debug::log(&format!("  Arg[{}]: Positional {:?}", i, val));
                }
            }
        }
        debug::dedent();

        Ok(Some(Command { name, arguments }))
    }

    /// Parse command arguments
    fn parse_arguments(&mut self) -> Result<Vec<Argument>, String> {
        let mut args = Vec::new();

        loop {
            match self.current() {
                Some(Token::Pipe) | Some(Token::CloseBrace) | None => break,
                Some(Token::Parameter(name)) => {
                    let name = name.clone();
                    self.advance();

                    // Check if next token is a value for this parameter
                    let value = self.try_parse_argument_value()?;
                    args.push(Argument::Parameter { name, value });
                }
                _ => {
                    // Positional argument
                    if let Some(value) = self.try_parse_argument_value()? {
                        args.push(Argument::Positional(value));
                    } else {
                        break;
                    }
                }
            }
        }

        Ok(args)
    }

    /// Try to parse an argument value (returns None if next token isn't a value)
    fn try_parse_argument_value(&mut self) -> Result<Option<ArgumentValue>, String> {
        match self.current() {
            Some(Token::String(s)) => {
                let s = s.clone();
                self.advance();
                Ok(Some(self.maybe_parse_list(ArgumentValue::String(s))?))
            }
            Some(Token::Number(n)) => {
                let n = *n;
                self.advance();
                Ok(Some(self.maybe_parse_list(ArgumentValue::Number(n))?))
            }
            Some(Token::Boolean(b)) => {
                let b = *b;
                self.advance();
                Ok(Some(ArgumentValue::Boolean(b)))
            }
            Some(Token::Identifier(s)) => {
                let s = s.clone();
                self.advance();
                Ok(Some(self.maybe_parse_list(ArgumentValue::Identifier(s))?))
            }
            Some(Token::ScriptBlock(content)) => {
                let content = content.clone();
                self.advance();
                Ok(Some(ArgumentValue::ScriptBlock(content)))
            }
            Some(Token::OpenBrace) => {
                self.advance();
                let content = self.parse_script_block_content()?;
                Ok(Some(ArgumentValue::ScriptBlock(content)))
            }
            Some(Token::HashTable(pairs)) => {
                let pairs = pairs.clone();
                self.advance();
                let calc_prop = CalculatedPropertyDef::from_hashtable(&pairs)?;
                Ok(Some(self.maybe_parse_list(ArgumentValue::CalculatedProperty(calc_prop))?))
            }
            _ => Ok(None),
        }
    }

    /// Check if followed by comma and more values (build a list)
    fn maybe_parse_list(&mut self, first: ArgumentValue) -> Result<ArgumentValue, String> {
        if !matches!(self.current(), Some(Token::Comma)) {
            return Ok(first);
        }

        let mut items = vec![first];

        while matches!(self.current(), Some(Token::Comma)) {
            self.advance(); // consume comma

            match self.try_parse_single_value()? {
                Some(v) => items.push(v),
                None => return Err("Expected value after comma".to_string()),
            }
        }

        Ok(ArgumentValue::List(items))
    }

    /// Parse a single value (not a list)
    fn try_parse_single_value(&mut self) -> Result<Option<ArgumentValue>, String> {
        match self.current() {
            Some(Token::String(s)) => {
                let s = s.clone();
                self.advance();
                Ok(Some(ArgumentValue::String(s)))
            }
            Some(Token::Number(n)) => {
                let n = *n;
                self.advance();
                Ok(Some(ArgumentValue::Number(n)))
            }
            Some(Token::Boolean(b)) => {
                let b = *b;
                self.advance();
                Ok(Some(ArgumentValue::Boolean(b)))
            }
            Some(Token::Identifier(s)) => {
                let s = s.clone();
                self.advance();
                Ok(Some(ArgumentValue::Identifier(s)))
            }
            Some(Token::HashTable(pairs)) => {
                let pairs = pairs.clone();
                self.advance();
                let calc_prop = CalculatedPropertyDef::from_hashtable(&pairs)?;
                Ok(Some(ArgumentValue::CalculatedProperty(calc_prop)))
            }
            _ => Ok(None),
        }
    }

    /// Parse script block content (everything between { and })
    fn parse_script_block_content(&mut self) -> Result<String, String> {
        let mut content = String::new();
        let mut brace_depth = 1;

        while brace_depth > 0 {
            match self.current() {
                None => return Err("Unterminated script block".to_string()),
                Some(Token::OpenBrace) => {
                    brace_depth += 1;
                    content.push_str(" { ");
                    self.advance();
                }
                Some(Token::CloseBrace) => {
                    brace_depth -= 1;
                    if brace_depth > 0 {
                        content.push_str(" } ");
                    }
                    self.advance();
                }
                Some(tok) => {
                    content.push(' ');
                    content.push_str(&token_to_string(tok));
                    self.advance();
                }
            }
        }

        Ok(content.trim().to_string())
    }

    /// Parse a script string into a Pipeline
    pub fn parse_script(script: &str) -> Result<Pipeline, String> {
        debug::log("Parser: Starting script parsing");
        debug::indent();

        let tokens = Lexer::tokenize(script)?;
        debug::log(&format!("Parser: Received {} tokens", tokens.len()));

        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        debug::dedent();
        if let Ok(ref pipeline) = result {
            debug::log(&format!(
                "Parser: Successfully parsed {} command(s)",
                pipeline.commands.len()
            ));
        }

        result
    }
}

fn token_to_string(tok: &Token) -> String {
    match tok {
        Token::Pipe => "|".to_string(),
        Token::OpenBrace => "{".to_string(),
        Token::CloseBrace => "}".to_string(),
        Token::ScriptBlock(s) => format!("{{ {} }}", s),
        Token::Comma => ",".to_string(),
        Token::Semicolon => ";".to_string(),
        Token::Equals => "=".to_string(),
        Token::Parameter(s) => format!("-{}", s),
        Token::Identifier(s) => s.clone(),
        Token::String(s) => format!("'{}'", s),
        Token::Number(n) => n.to_string(),
        Token::Boolean(b) => format!("${}", b),
        Token::HashTable(_) => "@{...}".to_string(),
        Token::Eof => String::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_pipeline() {
        let pipeline = Parser::parse_script("where | select | table").unwrap();
        assert_eq!(pipeline.commands.len(), 3);
        assert_eq!(pipeline.commands[0].name, "where");
        assert_eq!(pipeline.commands[1].name, "select");
        assert_eq!(pipeline.commands[2].name, "table");
    }

    #[test]
    fn test_command_with_args() {
        let pipeline = Parser::parse_script("where name -eq 'alice'").unwrap();
        assert_eq!(pipeline.commands.len(), 1);
        let cmd = &pipeline.commands[0];
        assert_eq!(cmd.name, "where");
        // 2 args: positional "name", and parameter "-eq" with value "alice"
        assert_eq!(cmd.arguments.len(), 2);
    }

    #[test]
    fn test_parameter_with_value() {
        let pipeline = Parser::parse_script("select -Property name, age").unwrap();
        let cmd = &pipeline.commands[0];

        let prop = cmd.get_parameter("Property").unwrap();
        let props = prop.as_string_list();
        assert_eq!(props, vec!["name", "age"]);
    }

    #[test]
    fn test_switch_parameter() {
        let pipeline = Parser::parse_script("sort name -Descending").unwrap();
        let cmd = &pipeline.commands[0];

        assert!(cmd.has_switch("Descending"));
        assert!(cmd.has_switch("desc")); // abbreviation
    }

    #[test]
    fn test_braced_script() {
        let pipeline = Parser::parse_script("{ where name -eq 'test' | select name }").unwrap();
        assert_eq!(pipeline.commands.len(), 2);
    }

    #[test]
    fn test_numeric_comparison() {
        let pipeline = Parser::parse_script("where age -gt 30").unwrap();
        let cmd = &pipeline.commands[0];
        // 2 args: positional "age", and parameter "-gt" with value 30
        assert_eq!(cmd.arguments.len(), 2);
    }

    #[test]
    fn test_calculated_property() {
        let pipeline = Parser::parse_script("select name, @{Name='Total'; Expression='price * qty'}").unwrap();
        let cmd = &pipeline.commands[0];

        // Should have 2 arguments: "name" and the calculated property
        assert_eq!(cmd.arguments.len(), 1); // They're in a list

        let args = cmd.get_positional_args();
        assert_eq!(args.len(), 1);

        if let ArgumentValue::List(items) = args[0] {
            assert_eq!(items.len(), 2);
            if let ArgumentValue::CalculatedProperty(calc) = &items[1] {
                assert_eq!(calc.name, Some("Total".to_string()));
                assert_eq!(calc.expression, "price * qty");
            } else {
                panic!("Expected CalculatedProperty");
            }
        } else {
            panic!("Expected List");
        }
    }

    #[test]
    fn test_calculated_property_abbreviated() {
        let pipeline = Parser::parse_script("select @{n='Age'; e='years'}").unwrap();
        let cmd = &pipeline.commands[0];

        let args = cmd.get_positional_args();
        if let ArgumentValue::CalculatedProperty(calc) = args[0] {
            assert_eq!(calc.name, Some("Age".to_string()));
            assert_eq!(calc.expression, "years");
        } else {
            panic!("Expected CalculatedProperty");
        }
    }
}
