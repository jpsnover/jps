//! AST definitions for the PowerShell-like script parser

/// A complete pipeline: Command | Command | Command
#[derive(Debug, Clone)]
pub struct Pipeline {
    pub commands: Vec<Command>,
}

/// A single command with its name and arguments
#[derive(Debug, Clone)]
pub struct Command {
    pub name: String,
    pub arguments: Vec<Argument>,
}

/// An argument can be a parameter (with optional value) or a positional value
#[derive(Debug, Clone)]
pub enum Argument {
    /// Named parameter like -Property or -Descending
    Parameter {
        name: String,
        value: Option<ArgumentValue>,
    },
    /// Positional argument (value without parameter name)
    Positional(ArgumentValue),
}

/// The value of an argument
#[derive(Debug, Clone)]
pub enum ArgumentValue {
    /// Simple identifier (property name, etc.)
    Identifier(String),
    /// String literal (quoted)
    String(String),
    /// Numeric literal
    Number(f64),
    /// Boolean literal ($true, $false)
    Boolean(bool),
    /// List of values (comma-separated)
    List(Vec<ArgumentValue>),
    /// Script block { ... } - for calculated properties
    ScriptBlock(String),
    /// Calculated property @{Name='...'; Expression='...'}
    CalculatedProperty(CalculatedPropertyDef),
}

/// Definition of a calculated property from @{...} hashtable
#[derive(Debug, Clone)]
pub struct CalculatedPropertyDef {
    /// Output name (from Name or Label key)
    pub name: Option<String>,
    /// Expression to evaluate (from Expression or Expr or E key)
    pub expression: String,
    /// Column width (for Format-Table)
    pub width: Option<usize>,
    /// Alignment (for Format-Table): Left, Center, Right
    pub alignment: Option<String>,
    /// Format string (for formatting output)
    pub format_string: Option<String>,
    /// Sort direction (for Sort-Object)
    pub descending: Option<bool>,
}

impl CalculatedPropertyDef {
    /// Parse a hashtable into a CalculatedPropertyDef
    pub fn from_hashtable(pairs: &[(String, String)]) -> Result<Self, String> {
        let mut name = None;
        let mut expression = None;
        let mut width = None;
        let mut alignment = None;
        let mut format_string = None;
        let mut descending = None;

        for (key, value) in pairs {
            let key_lower = key.to_lowercase();

            // Match keys with abbreviation support
            if key_lower == "name" || key_lower == "label" || key_lower == "n" || key_lower == "l" {
                name = Some(value.clone());
            } else if key_lower == "expression" || key_lower == "expr" || key_lower == "e" {
                expression = Some(value.clone());
            } else if key_lower == "width" || key_lower == "w" {
                width = value.parse().ok();
            } else if key_lower == "alignment" || key_lower == "align" || key_lower == "a" {
                alignment = Some(value.clone());
            } else if key_lower == "formatstring" || key_lower == "format" || key_lower == "f" {
                format_string = Some(value.clone());
            } else if key_lower == "descending" || key_lower == "desc" || key_lower == "d" {
                descending = Some(value.to_lowercase() == "true" || value == "1" || value == "$true");
            } else if key_lower == "ascending" || key_lower == "asc" {
                descending = Some(!(value.to_lowercase() == "true" || value == "1" || value == "$true"));
            }
        }

        let expression = expression.ok_or_else(|| {
            "Calculated property requires an Expression".to_string()
        })?;

        Ok(CalculatedPropertyDef {
            name,
            expression,
            width,
            alignment,
            format_string,
            descending,
        })
    }

    /// Get the output name, defaulting to the expression if no name specified
    pub fn output_name(&self) -> String {
        self.name.clone().unwrap_or_else(|| self.expression.clone())
    }
}

impl ArgumentValue {
    /// Convert to string representation
    pub fn to_string_repr(&self) -> String {
        match self {
            ArgumentValue::Identifier(s) => s.clone(),
            ArgumentValue::String(s) => s.clone(),
            ArgumentValue::Number(n) => n.to_string(),
            ArgumentValue::Boolean(b) => b.to_string(),
            ArgumentValue::List(items) => {
                items.iter().map(|v| v.to_string_repr()).collect::<Vec<_>>().join(", ")
            }
            ArgumentValue::ScriptBlock(s) => format!("{{ {} }}", s),
            ArgumentValue::CalculatedProperty(calc) => {
                format!("@{{Name='{}'; Expression='{}'}}", calc.output_name(), calc.expression)
            }
        }
    }

    /// Try to get as a single string
    pub fn as_string(&self) -> Option<String> {
        match self {
            ArgumentValue::Identifier(s) | ArgumentValue::String(s) => Some(s.clone()),
            ArgumentValue::Number(n) => Some(n.to_string()),
            ArgumentValue::Boolean(b) => Some(b.to_string()),
            _ => None,
        }
    }

    /// Try to get as a list of strings (for property lists)
    pub fn as_string_list(&self) -> Vec<String> {
        match self {
            ArgumentValue::Identifier(s) | ArgumentValue::String(s) => vec![s.clone()],
            ArgumentValue::List(items) => {
                items.iter().filter_map(|v| v.as_string()).collect()
            }
            _ => vec![],
        }
    }

    /// Try to get as f64
    pub fn as_f64(&self) -> Option<f64> {
        match self {
            ArgumentValue::Number(n) => Some(*n),
            ArgumentValue::String(s) | ArgumentValue::Identifier(s) => s.parse().ok(),
            _ => None,
        }
    }

    /// Try to get as i64
    pub fn as_i64(&self) -> Option<i64> {
        match self {
            ArgumentValue::Number(n) => Some(*n as i64),
            ArgumentValue::String(s) | ArgumentValue::Identifier(s) => s.parse().ok(),
            _ => None,
        }
    }

    /// Try to get as bool
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            ArgumentValue::Boolean(b) => Some(*b),
            ArgumentValue::String(s) | ArgumentValue::Identifier(s) => {
                match s.to_lowercase().as_str() {
                    "true" | "1" | "yes" => Some(true),
                    "false" | "0" | "no" => Some(false),
                    _ => None,
                }
            }
            _ => None,
        }
    }
}

impl Command {
    /// Get a named parameter value (case-insensitive, supports abbreviation)
    /// Either the search name or param name can be an abbreviation of the other
    pub fn get_parameter(&self, name: &str) -> Option<&ArgumentValue> {
        let name_lower = name.to_lowercase();

        for arg in &self.arguments {
            if let Argument::Parameter { name: param_name, value: Some(v) } = arg {
                let param_lower = param_name.to_lowercase();
                // Support abbreviation in either direction:
                // - User typed "-Prop" and we search for "property" → name_lower.starts_with(param_lower)
                // - User typed "-Property" and we search for "prop" → param_lower.starts_with(name_lower)
                if param_lower == name_lower
                    || name_lower.starts_with(&param_lower)
                    || param_lower.starts_with(&name_lower)
                {
                    return Some(v);
                }
            }
        }
        None
    }

    /// Check if a switch parameter is present (case-insensitive)
    /// Either the search name or param name can be an abbreviation of the other
    pub fn has_switch(&self, name: &str) -> bool {
        let name_lower = name.to_lowercase();

        for arg in &self.arguments {
            if let Argument::Parameter { name: param_name, value: None } = arg {
                let param_lower = param_name.to_lowercase();
                // Support abbreviation in either direction
                if param_lower == name_lower
                    || name_lower.starts_with(&param_lower)
                    || param_lower.starts_with(&name_lower)
                {
                    return true;
                }
            }
        }
        false
    }

    /// Get positional arguments (non-parameter arguments)
    pub fn get_positional_args(&self) -> Vec<&ArgumentValue> {
        self.arguments
            .iter()
            .filter_map(|arg| {
                if let Argument::Positional(v) = arg {
                    Some(v)
                } else {
                    None
                }
            })
            .collect()
    }

    /// Find parameter by abbreviation prefix, returns the matching full parameter name
    /// Returns error if abbreviation is ambiguous
    pub fn find_parameter_match<'a>(
        &self,
        abbrev: &str,
        valid_params: &[&'a str],
    ) -> Result<Option<&'a str>, String> {
        let abbrev_lower = abbrev.to_lowercase();
        let matches: Vec<&str> = valid_params
            .iter()
            .filter(|p| p.to_lowercase().starts_with(&abbrev_lower))
            .copied()
            .collect();

        match matches.len() {
            0 => Ok(None),
            1 => Ok(Some(matches[0])),
            _ => Err(format!(
                "Ambiguous parameter '{}': could be {}",
                abbrev,
                matches.join(" or ")
            )),
        }
    }
}
