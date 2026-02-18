use crate::ast::Command;
use crate::cmdlets::{Cmdlet, CmdletError, CmdletOutput, CmdletResult};
use crate::value::Value;
use serde_json::Value as JsonValue;

/// ConvertTo-Json cmdlet for JSON output
pub struct ConvertToJson;

impl Cmdlet for ConvertToJson {
    fn name(&self) -> &'static str {
        "ConvertTo-Json"
    }

    fn aliases(&self) -> &'static [&'static str] {
        &["json"]
    }

    fn execute(&self, input: Vec<Value>, cmd: &Command) -> CmdletResult<CmdletOutput> {
        let depth = cmd
            .get_parameter("depth")
            .and_then(|v| v.as_i64())
            .map(|n| n.max(1) as usize)
            .unwrap_or(100);

        let compress = cmd.has_switch("compress");

        // Convert to JSON array if multiple items, single object if one
        let json_val: JsonValue = if input.len() == 1 {
            input.into_iter().next().unwrap().into_inner()
        } else {
            JsonValue::Array(input.into_iter().map(|v| v.into_inner()).collect())
        };

        // Apply depth limit
        let limited = limit_depth(&json_val, depth);

        // Format output
        let output = if compress {
            serde_json::to_string(&limited)
                .map_err(|e| CmdletError(format!("JSON serialization error: {}", e)))?
        } else {
            serde_json::to_string_pretty(&limited)
                .map_err(|e| CmdletError(format!("JSON serialization error: {}", e)))?
        };

        Ok(CmdletOutput::Text(output))
    }
}

fn limit_depth(value: &JsonValue, depth: usize) -> JsonValue {
    if depth == 0 {
        return match value {
            JsonValue::Object(_) => JsonValue::String("[object]".to_string()),
            JsonValue::Array(_) => JsonValue::String("[array]".to_string()),
            _ => value.clone(),
        };
    }

    match value {
        JsonValue::Object(map) => {
            let limited: serde_json::Map<String, JsonValue> = map
                .iter()
                .map(|(k, v)| (k.clone(), limit_depth(v, depth - 1)))
                .collect();
            JsonValue::Object(limited)
        }
        JsonValue::Array(arr) => {
            let limited: Vec<JsonValue> = arr.iter().map(|v| limit_depth(v, depth - 1)).collect();
            JsonValue::Array(limited)
        }
        _ => value.clone(),
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

        match ConvertToJson.execute(input_values, cmd).unwrap() {
            CmdletOutput::Text(s) => s,
            _ => panic!("Expected text output"),
        }
    }

    #[test]
    fn test_single_object() {
        let input = vec![json!({"name": "alice", "age": 30})];
        let result = run_convert("json", input);

        let parsed: serde_json::Value = serde_json::from_str(&result).unwrap();
        assert_eq!(parsed["name"], "alice");
        assert_eq!(parsed["age"], 30);
    }

    #[test]
    fn test_multiple_objects() {
        let input = vec![
            json!({"name": "alice"}),
            json!({"name": "bob"}),
        ];
        let result = run_convert("json", input);

        let parsed: serde_json::Value = serde_json::from_str(&result).unwrap();
        assert!(parsed.is_array());
        assert_eq!(parsed.as_array().unwrap().len(), 2);
    }

    #[test]
    fn test_compress() {
        let input = vec![json!({"name": "alice"})];
        let result = run_convert("json -Compress", input);

        assert!(!result.contains('\n'));
    }

    #[test]
    fn test_depth_limit() {
        let input = vec![json!({"nested": {"deep": {"value": 1}}})];
        let result = run_convert("json -Depth 2", input);

        let parsed: serde_json::Value = serde_json::from_str(&result).unwrap();
        assert_eq!(parsed["nested"]["deep"], "[object]");
    }
}
