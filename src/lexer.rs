/// Lexer for PowerShell-like script syntax
use crate::debug;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    /// Pipe character |
    Pipe,
    /// Opening brace { (legacy, no longer emitted by tokenizer)
    OpenBrace,
    /// Closing brace } (legacy, no longer emitted by tokenizer)
    CloseBrace,
    /// Script block { ... } captured as raw string
    ScriptBlock(String),
    /// Comma separator
    Comma,
    /// Semicolon separator (for hashtables)
    Semicolon,
    /// Equals sign (for hashtable key=value)
    Equals,
    /// Parameter name (starts with -)
    Parameter(String),
    /// Identifier (cmdlet name, property name, etc.)
    Identifier(String),
    /// String literal (single or double quoted)
    String(String),
    /// Numeric literal
    Number(f64),
    /// Boolean ($true, $false)
    Boolean(bool),
    /// Hashtable literal @{...}
    HashTable(Vec<(String, String)>),
    /// End of input
    Eof,
}

pub struct Lexer {
    input: Vec<char>,
    pos: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Lexer {
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

    fn advance(&mut self) -> Option<char> {
        let c = self.current();
        self.pos += 1;
        c
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

    pub fn next_token(&mut self) -> Result<Token, String> {
        self.skip_whitespace();

        match self.current() {
            None => Ok(Token::Eof),
            Some('|') => {
                self.advance();
                Ok(Token::Pipe)
            }
            Some('{') => self.read_script_block(),
            Some('}') => {
                self.advance();
                Ok(Token::CloseBrace)
            }
            Some(',') => {
                self.advance();
                Ok(Token::Comma)
            }
            Some(';') => {
                self.advance();
                Ok(Token::Semicolon)
            }
            Some('=') => {
                self.advance();
                Ok(Token::Equals)
            }
            Some('@') => {
                // Check for hashtable @{...}
                if self.peek() == Some('{') {
                    self.read_hashtable()
                } else {
                    Err("Expected '{' after '@'".to_string())
                }
            }
            Some('-') => {
                // Could be a parameter or a negative number
                if let Some(next) = self.peek() {
                    if next.is_ascii_digit() {
                        // Negative number
                        self.read_number()
                    } else {
                        // Parameter
                        self.read_parameter()
                    }
                } else {
                    self.read_parameter()
                }
            }
            Some('$') => self.read_variable(),
            Some('"') => self.read_double_quoted_string(),
            Some('\'') => self.read_single_quoted_string(),
            Some(c) if c.is_ascii_digit() => self.read_number(),
            Some(c) if is_identifier_start(c) => self.read_identifier(),
            Some('.') => {
                // Standalone dot used as argument (e.g., -ArgumentList .)
                self.advance();
                Ok(Token::String(".".to_string()))
            }
            Some(c) => Err(format!("Unexpected character: '{}'", c)),
        }
    }

    fn read_parameter(&mut self) -> Result<Token, String> {
        // Skip the leading -
        self.advance();

        let mut name = String::new();
        while let Some(c) = self.current() {
            if is_identifier_char(c) {
                name.push(c);
                self.advance();
            } else {
                break;
            }
        }

        if name.is_empty() {
            return Err("Empty parameter name".to_string());
        }

        Ok(Token::Parameter(name))
    }

    fn read_variable(&mut self) -> Result<Token, String> {
        // Skip the $
        self.advance();

        let mut name = String::new();
        while let Some(c) = self.current() {
            if is_identifier_char(c) {
                name.push(c);
                self.advance();
            } else {
                break;
            }
        }

        // Check for $true and $false
        match name.to_lowercase().as_str() {
            "true" => Ok(Token::Boolean(true)),
            "false" => Ok(Token::Boolean(false)),
            "null" => Ok(Token::Identifier("$null".to_string())),
            _ => Ok(Token::Identifier(format!("${}", name))),
        }
    }

    fn read_double_quoted_string(&mut self) -> Result<Token, String> {
        // Skip opening quote
        self.advance();

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
                        Some('r') => s.push('\r'),
                        Some('"') => s.push('"'),
                        Some('\\') => s.push('\\'),
                        Some(c) => {
                            s.push('\\');
                            s.push(c);
                        }
                        None => return Err("Unterminated escape sequence".to_string()),
                    }
                    self.advance();
                }
                Some('`') => {
                    // PowerShell-style escape
                    self.advance();
                    match self.current() {
                        Some('n') => s.push('\n'),
                        Some('t') => s.push('\t'),
                        Some('r') => s.push('\r'),
                        Some('"') => s.push('"'),
                        Some('`') => s.push('`'),
                        Some(c) => s.push(c),
                        None => return Err("Unterminated escape sequence".to_string()),
                    }
                    self.advance();
                }
                Some(c) => {
                    s.push(c);
                    self.advance();
                }
            }
        }

        Ok(Token::String(s))
    }

    fn read_single_quoted_string(&mut self) -> Result<Token, String> {
        // Skip opening quote
        self.advance();

        let mut s = String::new();
        loop {
            match self.current() {
                None => return Err("Unterminated string".to_string()),
                Some('\'') => {
                    // Check for escaped single quote ''
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

        Ok(Token::String(s))
    }

    fn read_number(&mut self) -> Result<Token, String> {
        let mut s = String::new();

        // Handle negative sign
        if self.current() == Some('-') {
            s.push('-');
            self.advance();
        }

        // Read integer part
        while let Some(c) = self.current() {
            if c.is_ascii_digit() {
                s.push(c);
                self.advance();
            } else {
                break;
            }
        }

        // Read decimal part
        if self.current() == Some('.') {
            if let Some(next) = self.peek() {
                if next.is_ascii_digit() {
                    s.push('.');
                    self.advance();
                    while let Some(c) = self.current() {
                        if c.is_ascii_digit() {
                            s.push(c);
                            self.advance();
                        } else {
                            break;
                        }
                    }
                }
            }
        }

        s.parse::<f64>()
            .map(Token::Number)
            .map_err(|_| format!("Invalid number: {}", s))
    }

    fn read_identifier(&mut self) -> Result<Token, String> {
        let mut s = String::new();

        while let Some(c) = self.current() {
            if is_identifier_char(c) {
                s.push(c);
                self.advance();
            } else {
                break;
            }
        }

        Ok(Token::Identifier(s))
    }

    /// Read a script block { ... } as a raw string, handling nesting
    fn read_script_block(&mut self) -> Result<Token, String> {
        // Skip the opening {
        self.advance();

        let mut content = String::new();
        let mut depth = 1;

        loop {
            match self.current() {
                None => return Err("Unterminated script block".to_string()),
                Some('{') => {
                    depth += 1;
                    content.push('{');
                    self.advance();
                }
                Some('}') => {
                    depth -= 1;
                    if depth == 0 {
                        self.advance(); // consume closing }
                        break;
                    } else {
                        content.push('}');
                        self.advance();
                    }
                }
                Some(c) => {
                    content.push(c);
                    self.advance();
                }
            }
        }

        Ok(Token::ScriptBlock(content.trim().to_string()))
    }

    /// Read a hashtable @{key=value; key2=value2}
    fn read_hashtable(&mut self) -> Result<Token, String> {
        // Skip @
        self.advance();
        // Skip {
        self.advance();

        let mut pairs = Vec::new();

        loop {
            self.skip_whitespace();

            // Check for end of hashtable
            if self.current() == Some('}') {
                self.advance();
                break;
            }

            // Read key
            let key = self.read_hashtable_key()?;

            self.skip_whitespace();

            // Expect =
            if self.current() != Some('=') {
                return Err(format!("Expected '=' after hashtable key '{}'", key));
            }
            self.advance();

            self.skip_whitespace();

            // Read value
            let value = self.read_hashtable_value()?;

            pairs.push((key, value));

            self.skip_whitespace();

            // Check for separator (semicolon or comma) or end
            match self.current() {
                Some(';') | Some(',') => {
                    self.advance();
                }
                Some('}') => {
                    // Will be handled at start of loop
                }
                Some(c) => {
                    return Err(format!("Expected ';', ',', or '}}' in hashtable, found '{}'", c));
                }
                None => {
                    return Err("Unterminated hashtable".to_string());
                }
            }
        }

        Ok(Token::HashTable(pairs))
    }

    fn read_hashtable_key(&mut self) -> Result<String, String> {
        let mut key = String::new();

        while let Some(c) = self.current() {
            if c.is_alphanumeric() || c == '_' {
                key.push(c);
                self.advance();
            } else {
                break;
            }
        }

        if key.is_empty() {
            return Err("Empty hashtable key".to_string());
        }

        Ok(key)
    }

    fn read_hashtable_value(&mut self) -> Result<String, String> {
        match self.current() {
            Some('\'') => {
                // Single-quoted string
                self.advance();
                let mut s = String::new();
                loop {
                    match self.current() {
                        None => return Err("Unterminated string in hashtable".to_string()),
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
                Ok(s)
            }
            Some('"') => {
                // Double-quoted string
                self.advance();
                let mut s = String::new();
                loop {
                    match self.current() {
                        None => return Err("Unterminated string in hashtable".to_string()),
                        Some('"') => {
                            self.advance();
                            break;
                        }
                        Some('\\') => {
                            self.advance();
                            match self.current() {
                                Some('n') => s.push('\n'),
                                Some('t') => s.push('\t'),
                                Some('"') => s.push('"'),
                                Some('\\') => s.push('\\'),
                                Some(c) => s.push(c),
                                None => return Err("Unterminated escape in hashtable".to_string()),
                            }
                            self.advance();
                        }
                        Some(c) => {
                            s.push(c);
                            self.advance();
                        }
                    }
                }
                Ok(s)
            }
            _ => {
                // Unquoted value - read until ; or } or whitespace
                let mut s = String::new();
                while let Some(c) = self.current() {
                    if c == ';' || c == '}' || c == ',' || c.is_whitespace() {
                        break;
                    }
                    s.push(c);
                    self.advance();
                }
                Ok(s)
            }
        }
    }

    /// Tokenize the entire input
    pub fn tokenize(input: &str) -> Result<Vec<Token>, String> {
        debug::log("Lexer: Starting tokenization");
        debug::indent();

        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();

        loop {
            let token = lexer.next_token()?;
            if token == Token::Eof {
                break;
            }
            debug::log(&format!("Token: {:?}", token));
            tokens.push(token);
        }

        debug::dedent();
        debug::log(&format!("Lexer: Produced {} tokens", tokens.len()));
        Ok(tokens)
    }
}

fn is_identifier_start(c: char) -> bool {
    c.is_alphabetic() || c == '_' || c == '*' || c == '?'
}

fn is_identifier_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '-' || c == '*' || c == '?'
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_tokens() {
        let tokens = Lexer::tokenize("where | select").unwrap();
        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[0], Token::Identifier("where".to_string()));
        assert_eq!(tokens[1], Token::Pipe);
        assert_eq!(tokens[2], Token::Identifier("select".to_string()));
    }

    #[test]
    fn test_parameters() {
        let tokens = Lexer::tokenize("-Property name -Descending").unwrap();
        assert_eq!(tokens[0], Token::Parameter("Property".to_string()));
        assert_eq!(tokens[1], Token::Identifier("name".to_string()));
        assert_eq!(tokens[2], Token::Parameter("Descending".to_string()));
    }

    #[test]
    fn test_strings() {
        let tokens = Lexer::tokenize(r#""hello world" 'test'"#).unwrap();
        assert_eq!(tokens[0], Token::String("hello world".to_string()));
        assert_eq!(tokens[1], Token::String("test".to_string()));
    }

    #[test]
    fn test_numbers() {
        let tokens = Lexer::tokenize("42 3.14 -10").unwrap();
        assert_eq!(tokens[0], Token::Number(42.0));
        assert_eq!(tokens[1], Token::Number(3.14));
        assert_eq!(tokens[2], Token::Number(-10.0));
    }

    #[test]
    fn test_booleans() {
        let tokens = Lexer::tokenize("$true $false").unwrap();
        assert_eq!(tokens[0], Token::Boolean(true));
        assert_eq!(tokens[1], Token::Boolean(false));
    }

    #[test]
    fn test_wildcards() {
        let tokens = Lexer::tokenize("user* *name handle?").unwrap();
        assert_eq!(tokens[0], Token::Identifier("user*".to_string()));
        assert_eq!(tokens[1], Token::Identifier("*name".to_string()));
        assert_eq!(tokens[2], Token::Identifier("handle?".to_string()));
    }

    #[test]
    fn test_braces() {
        let tokens = Lexer::tokenize("{ where name -eq 'test' }").unwrap();
        assert_eq!(tokens.len(), 1);
        assert!(matches!(&tokens[0], Token::ScriptBlock(_)));
        if let Token::ScriptBlock(content) = &tokens[0] {
            assert!(content.contains("where"));
        }
    }

    #[test]
    fn test_hashtable() {
        let tokens = Lexer::tokenize("@{Name='Total'; Expression='price * qty'}").unwrap();
        assert_eq!(tokens.len(), 1);
        if let Token::HashTable(pairs) = &tokens[0] {
            assert_eq!(pairs.len(), 2);
            assert_eq!(pairs[0], ("Name".to_string(), "Total".to_string()));
            assert_eq!(pairs[1], ("Expression".to_string(), "price * qty".to_string()));
        } else {
            panic!("Expected HashTable token");
        }
    }

    #[test]
    fn test_hashtable_with_comma() {
        let tokens = Lexer::tokenize("@{n='Age', e='years'}").unwrap();
        assert_eq!(tokens.len(), 1);
        if let Token::HashTable(pairs) = &tokens[0] {
            assert_eq!(pairs.len(), 2);
        } else {
            panic!("Expected HashTable token");
        }
    }
}
