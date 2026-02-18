use crate::ast::Command;
use crate::cmdlets::{Cmdlet, CmdletOutput, CmdletResult};
use crate::value::Value;

/// ConvertTo-Csv cmdlet for CSV output
pub struct ConvertToCsv;

impl Cmdlet for ConvertToCsv {
    fn name(&self) -> &'static str {
        "ConvertTo-Csv"
    }

    fn aliases(&self) -> &'static [&'static str] {
        &["csv"]
    }

    fn execute(&self, input: Vec<Value>, cmd: &Command) -> CmdletResult<CmdletOutput> {
        if input.is_empty() {
            return Ok(CmdletOutput::Text(String::new()));
        }

        let delimiter = cmd
            .get_parameter("delimiter")
            .and_then(|v| v.as_string())
            .and_then(|s| s.chars().next())
            .unwrap_or(',');

        let no_header = cmd.has_switch("notypeinfo")
            || cmd.has_switch("noheader")
            || cmd.has_switch("notypeinformation");

        // Get all unique property names from all objects
        let headers = collect_headers(&input);

        let mut output = String::new();

        // Header row
        if !no_header {
            output.push_str(&format_csv_row(&headers, delimiter));
            output.push('\n');
        }

        // Data rows
        for obj in &input {
            let row: Vec<String> = headers
                .iter()
                .map(|h| {
                    obj.get_property(h)
                        .map(|v| v.to_string_value())
                        .unwrap_or_default()
                })
                .collect();
            output.push_str(&format_csv_row(&row, delimiter));
            output.push('\n');
        }

        Ok(CmdletOutput::Text(output))
    }
}

fn collect_headers(input: &[Value]) -> Vec<String> {
    let mut headers = Vec::new();

    for obj in input {
        for name in obj.property_names() {
            if !headers.iter().any(|h: &String| h.to_lowercase() == name.to_lowercase()) {
                headers.push(name);
            }
        }
    }

    headers
}

fn format_csv_row(values: &[String], delimiter: char) -> String {
    values
        .iter()
        .map(|v| escape_csv_field(v, delimiter))
        .collect::<Vec<_>>()
        .join(&delimiter.to_string())
}

fn escape_csv_field(field: &str, delimiter: char) -> String {
    let needs_quotes = field.contains(delimiter)
        || field.contains('"')
        || field.contains('\n')
        || field.contains('\r');

    if needs_quotes {
        format!("\"{}\"", field.replace('"', "\"\""))
    } else {
        field.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;
    use serde_json::json;

    fn run_convert(script: &str, input: Vec<serde_json::Value>) -> String {
        let pipeline = Parser::parse_script(script).unwrap();
        let cmd = &pipeline.commands[0];
        let input_values: Vec<Value> = input.into_iter().map(Value::new).collect();

        match ConvertToCsv.execute(input_values, cmd).unwrap() {
            CmdletOutput::Text(s) => s,
            _ => panic!("Expected text output"),
        }
    }

    #[test]
    fn test_basic_csv() {
        let input = vec![
            json!({"name": "alice", "age": 30}),
            json!({"name": "bob", "age": 25}),
        ];
        let result = run_convert("csv", input);

        let lines: Vec<&str> = result.lines().collect();
        assert_eq!(lines.len(), 3); // header + 2 data rows
        assert!(lines[0].contains("name"));
        assert!(lines[1].contains("alice"));
        assert!(lines[2].contains("bob"));
    }

    #[test]
    fn test_no_header() {
        let input = vec![json!({"name": "alice"})];
        let result = run_convert("csv -NoTypeInformation", input);

        let lines: Vec<&str> = result.lines().collect();
        assert_eq!(lines.len(), 1);
        assert!(lines[0].contains("alice"));
    }

    #[test]
    fn test_custom_delimiter() {
        let input = vec![json!({"name": "alice", "age": 30})];
        let result = run_convert("csv -Delimiter ';'", input);

        assert!(result.contains(';'));
    }

    #[test]
    fn test_escape_quotes() {
        let input = vec![json!({"name": "O\"Brien"})];
        let result = run_convert("csv", input);

        assert!(result.contains("\"O\"\"Brien\""));
    }

    #[test]
    fn test_escape_comma() {
        let input = vec![json!({"name": "Doe, John"})];
        let result = run_convert("csv", input);

        assert!(result.contains("\"Doe, John\""));
    }
}
