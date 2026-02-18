use crate::ast::{Argument, ArgumentValue, CalculatedPropertyDef, Command};
use crate::cmdlets::{Cmdlet, CmdletOutput, CmdletResult};
use crate::expression;
use crate::value::Value;

/// Format-List cmdlet for list-style output
pub struct FormatList;

impl Cmdlet for FormatList {
    fn name(&self) -> &'static str {
        "Format-List"
    }

    fn aliases(&self) -> &'static [&'static str] {
        &["fl", "list"]
    }

    fn execute(&self, input: Vec<Value>, cmd: &Command) -> CmdletResult<CmdletOutput> {
        if input.is_empty() {
            return Ok(CmdletOutput::Text(String::new()));
        }

        let properties = get_display_properties(cmd, &input)?;
        let output = format_list(&input, &properties);
        Ok(CmdletOutput::Text(output))
    }
}

/// A property specification for format-list
#[derive(Debug, Clone)]
enum PropertySpec {
    Simple(String),
    Calculated {
        label: String,
        expression: String,
        format_string: Option<String>,
    },
}

impl PropertySpec {
    fn label(&self) -> &str {
        match self {
            PropertySpec::Simple(name) => name.as_str(),
            PropertySpec::Calculated { label, .. } => label.as_str(),
        }
    }

    fn get_value(&self, obj: &Value) -> String {
        match self {
            PropertySpec::Simple(name) => obj
                .get_property(name)
                .map(|v| v.to_string_value())
                .unwrap_or_default(),
            PropertySpec::Calculated { expression, format_string, .. } => {
                match expression::evaluate(expression, obj) {
                    Ok(val) => {
                        let raw = expression::json_to_string(&val);
                        if let Some(fmt) = format_string {
                            apply_format_string(&raw, fmt, &val)
                        } else {
                            raw
                        }
                    }
                    Err(_) => String::new(),
                }
            }
        }
    }
}

fn apply_format_string(raw: &str, fmt: &str, val: &serde_json::Value) -> String {
    let fmt_upper = fmt.to_uppercase();
    if let Some(n) = val.as_f64() {
        if fmt_upper.starts_with('N') {
            let decimals = fmt[1..].parse::<usize>().unwrap_or(2);
            return format!("{:.prec$}", n, prec = decimals);
        } else if fmt_upper.starts_with('F') {
            let decimals = fmt[1..].parse::<usize>().unwrap_or(2);
            return format!("{:.prec$}", n, prec = decimals);
        } else if fmt_upper.starts_with('P') {
            let decimals = fmt[1..].parse::<usize>().unwrap_or(2);
            return format!("{:.prec$}%", n * 100.0, prec = decimals);
        }
    }
    raw.to_string()
}

fn from_calculated_def(calc: &CalculatedPropertyDef) -> PropertySpec {
    PropertySpec::Calculated {
        label: calc.output_name(),
        expression: calc.expression.clone(),
        format_string: calc.format_string.clone(),
    }
}

fn get_display_properties(cmd: &Command, input: &[Value]) -> CmdletResult<Vec<PropertySpec>> {
    // Check -Property parameter
    if let Some(prop_val) = cmd.get_parameter("property") {
        return extract_properties(prop_val, input);
    }

    // Check positional arguments
    let mut properties = Vec::new();
    for arg in &cmd.arguments {
        if let Argument::Positional(val) = arg {
            let mut extracted = extract_properties(val, input)?;
            properties.append(&mut extracted);
        }
    }

    if !properties.is_empty() {
        return Ok(properties);
    }

    // Default: use all properties from first object
    if let Some(first) = input.first() {
        Ok(first
            .property_names()
            .into_iter()
            .map(PropertySpec::Simple)
            .collect())
    } else {
        Ok(Vec::new())
    }
}

fn extract_properties(val: &ArgumentValue, input: &[Value]) -> CmdletResult<Vec<PropertySpec>> {
    let mut props = Vec::new();
    extract_properties_into(val, input, &mut props);
    Ok(props)
}

fn extract_properties_into(val: &ArgumentValue, input: &[Value], props: &mut Vec<PropertySpec>) {
    match val {
        ArgumentValue::Identifier(s) | ArgumentValue::String(s) => {
            if s.contains('*') || s.contains('?') {
                if let Some(first) = input.first() {
                    for (name, _) in first.get_properties_wildcard(s) {
                        if !props.iter().any(|p| p.label().to_lowercase() == name.to_lowercase()) {
                            props.push(PropertySpec::Simple(name));
                        }
                    }
                }
            } else {
                props.push(PropertySpec::Simple(s.clone()));
            }
        }
        ArgumentValue::CalculatedProperty(calc) => {
            props.push(from_calculated_def(calc));
        }
        ArgumentValue::List(items) => {
            for item in items {
                extract_properties_into(item, input, props);
            }
        }
        _ => {}
    }
}

fn format_list(input: &[Value], properties: &[PropertySpec]) -> String {
    let mut output = String::new();

    // Find max label length for alignment
    let max_label_len = properties.iter().map(|p| p.label().len()).max().unwrap_or(0);

    for (i, obj) in input.iter().enumerate() {
        if i > 0 {
            output.push('\n');
        }

        for prop in properties {
            let label = prop.label();
            let value = prop.get_value(obj);

            // Right-align labels
            let padding = " ".repeat(max_label_len - label.len());
            output.push_str(&format!("{}{} : {}\n", padding, label, value));
        }
    }

    output
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;
    use serde_json::json;

    fn run_format(script: &str, input: Vec<serde_json::Value>) -> String {
        let pipeline = Parser::parse_script(script).unwrap();
        let cmd = &pipeline.commands[0];
        let input_values: Vec<Value> = input.into_iter().map(Value::new).collect();

        match FormatList.execute(input_values, cmd).unwrap() {
            CmdletOutput::Text(s) => s,
            _ => panic!("Expected text output"),
        }
    }

    #[test]
    fn test_basic_list() {
        let input = vec![json!({"name": "alice", "age": 30})];
        let result = run_format("list", input);

        assert!(result.contains("name : alice"));
        assert!(result.contains("age : 30"));
    }

    #[test]
    fn test_list_selected_properties() {
        let input = vec![json!({"name": "alice", "age": 30, "city": "NYC"})];
        let result = run_format("list name, age", input);

        assert!(result.contains("name : alice"));
        assert!(result.contains("age : 30"));
        assert!(!result.contains("city"));
    }

    #[test]
    fn test_multiple_objects() {
        let input = vec![
            json!({"name": "alice"}),
            json!({"name": "bob"}),
        ];
        let result = run_format("list", input);

        assert!(result.contains("alice"));
        assert!(result.contains("bob"));
    }

    #[test]
    fn test_calculated_property() {
        let input = vec![json!({"price": 10, "qty": 5})];
        let result = run_format("list @{name='Total'; expression='price * qty'}", input);

        assert!(result.contains("Total"));
        assert!(result.contains("50"));
    }
}
