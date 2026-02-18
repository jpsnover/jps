use crate::ast::{Argument, ArgumentValue, Command};
use crate::cmdlets::{Cmdlet, CmdletError, CmdletOutput, CmdletResult};
use crate::debug;
use crate::operators::ComparisonOp;
use crate::value::Value;

/// Where-Object cmdlet for filtering objects
pub struct WhereObject;

impl Cmdlet for WhereObject {
    fn name(&self) -> &'static str {
        "Where-Object"
    }

    fn aliases(&self) -> &'static [&'static str] {
        &["where", "?"]
    }

    fn execute(&self, input: Vec<Value>, cmd: &Command) -> CmdletResult<CmdletOutput> {
        let filter = parse_filter(cmd)?;

        debug::log(&format!(
            "Where-Object: property='{}', operator={:?}",
            filter.property, filter.operator
        ));

        let input_count = input.len();
        let filtered: Vec<Value> = input
            .into_iter()
            .filter(|v| filter.matches(v))
            .collect();

        debug::log(&format!(
            "Where-Object: {} of {} objects passed filter",
            filtered.len(),
            input_count
        ));

        Ok(CmdletOutput::Objects(filtered))
    }
}

/// Parsed filter condition
struct Filter {
    property: String,
    operator: ComparisonOp,
    value: Value,
}

impl Filter {
    fn matches(&self, obj: &Value) -> bool {
        // Get property value (supports wildcards)
        let prop_values = if self.property.contains('*') || self.property.contains('?') {
            obj.get_properties_wildcard(&self.property)
                .into_iter()
                .map(|(_, v)| v)
                .collect()
        } else {
            match obj.get_property(&self.property) {
                Some(v) => vec![v],
                None => vec![],
            }
        };

        // If property doesn't exist, filter returns false
        if prop_values.is_empty() {
            return false;
        }

        // Check if any matching property satisfies the condition
        prop_values.iter().any(|pv| self.operator.evaluate(pv, &self.value))
    }
}

fn parse_filter(cmd: &Command) -> CmdletResult<Filter> {
    let args = &cmd.arguments;

    // Expected pattern: Property Operator Value
    // or: -Property prop -Operator Value (less common)

    // First, try positional form: property -operator value
    let positional: Vec<&ArgumentValue> = args
        .iter()
        .filter_map(|a| match a {
            Argument::Positional(v) => Some(v),
            _ => None,
        })
        .collect();

    let params: Vec<(&String, Option<&ArgumentValue>)> = args
        .iter()
        .filter_map(|a| match a {
            Argument::Parameter { name, value } => Some((name, value.as_ref())),
            _ => None,
        })
        .collect();

    // Check for -FilterScript or -Property parameter
    if let Some(prop_param) = params.iter().find(|(n, _)| {
        let nl = n.to_lowercase();
        "property".starts_with(&nl) || "filterscript".starts_with(&nl)
    }) {
        if let Some(val) = prop_param.1 {
            let property = val.as_string().ok_or_else(|| {
                CmdletError("Invalid property value".to_string())
            })?;

            // Find operator and value in remaining params
            let (operator, value) = find_operator_and_value(&params, &positional)?;
            return Ok(Filter { property, operator, value });
        }
    }

    // Positional form: property -operator value
    // OR: property operator value (where operator looks like -eq)
    if !positional.is_empty() {
        let property = positional[0].as_string().ok_or_else(|| {
            CmdletError("Property name must be a string".to_string())
        })?;

        let (operator, value) = find_operator_and_value(&params, &positional[1..])?;
        return Ok(Filter { property, operator, value });
    }

    Err(CmdletError(
        "Where-Object requires: Property -Operator Value".to_string(),
    ))
}

fn find_operator_and_value(
    params: &[(&String, Option<&ArgumentValue>)],
    positional: &[&ArgumentValue],
) -> CmdletResult<(ComparisonOp, Value)> {
    // Look for operator in parameters (like -eq, -gt, etc.)
    for (name, value) in params {
        if let Some(op) = ComparisonOp::parse(name) {
            // The parameter value is our comparison value
            let val = value
                .ok_or_else(|| CmdletError(format!("Operator -{} requires a value", name)))?;
            return Ok((op, argument_to_value(val)?));
        }
    }

    // Check positional arguments for operator
    if positional.len() >= 2 {
        if let ArgumentValue::Identifier(op_str) = &positional[0] {
            if let Some(op) = ComparisonOp::parse(op_str) {
                return Ok((op, argument_to_value(positional[1])?));
            }
        }
    }

    Err(CmdletError(
        "Missing comparison operator (e.g., -eq, -gt, -like)".to_string(),
    ))
}

fn argument_to_value(arg: &ArgumentValue) -> CmdletResult<Value> {
    let json_val = match arg {
        ArgumentValue::String(s) => serde_json::Value::String(s.clone()),
        ArgumentValue::Number(n) => {
            serde_json::Value::Number(serde_json::Number::from_f64(*n).unwrap_or_else(|| {
                serde_json::Number::from(*n as i64)
            }))
        }
        ArgumentValue::Boolean(b) => serde_json::Value::Bool(*b),
        ArgumentValue::Identifier(s) => {
            // Try to parse as number, otherwise treat as string
            if let Ok(n) = s.parse::<f64>() {
                serde_json::Value::Number(serde_json::Number::from_f64(n).unwrap_or_else(|| {
                    serde_json::Number::from(n as i64)
                }))
            } else if s.to_lowercase() == "true" {
                serde_json::Value::Bool(true)
            } else if s.to_lowercase() == "false" {
                serde_json::Value::Bool(false)
            } else if s == "$null" {
                serde_json::Value::Null
            } else {
                serde_json::Value::String(s.clone())
            }
        }
        ArgumentValue::List(items) => {
            let arr: Result<Vec<serde_json::Value>, _> = items
                .iter()
                .map(|i| argument_to_value(i).map(|v| v.into_inner()))
                .collect();
            serde_json::Value::Array(arr?)
        }
        ArgumentValue::ScriptBlock(_) => {
            return Err(CmdletError("Script blocks not supported in comparisons".to_string()));
        }
        ArgumentValue::CalculatedProperty(_) => {
            return Err(CmdletError("Calculated properties not supported in comparisons".to_string()));
        }
    };

    Ok(Value::new(json_val))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;
    use serde_json::json;

    fn run_where(script: &str, input: Vec<serde_json::Value>) -> Vec<Value> {
        let pipeline = Parser::parse_script(script).unwrap();
        let cmd = &pipeline.commands[0];
        let input_values: Vec<Value> = input.into_iter().map(Value::new).collect();

        match WhereObject.execute(input_values, cmd).unwrap() {
            CmdletOutput::Objects(v) => v,
            _ => panic!("Expected objects output"),
        }
    }

    #[test]
    fn test_eq_filter() {
        let input = vec![
            json!({"name": "alice", "age": 30}),
            json!({"name": "bob", "age": 25}),
        ];
        let result = run_where("where name -eq alice", input);
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].get_property("name").unwrap().as_str(), Some("alice"));
    }

    #[test]
    fn test_gt_filter() {
        let input = vec![
            json!({"name": "alice", "age": 30}),
            json!({"name": "bob", "age": 25}),
        ];
        let result = run_where("where age -gt 26", input);
        assert_eq!(result.len(), 1);
        assert_eq!(result[0].get_property("name").unwrap().as_str(), Some("alice"));
    }

    #[test]
    fn test_like_filter() {
        let input = vec![
            json!({"name": "alice", "age": 30}),
            json!({"name": "bob", "age": 25}),
        ];
        let result = run_where("where name -like 'a*'", input);
        assert_eq!(result.len(), 1);
    }

    #[test]
    fn test_case_insensitive() {
        let input = vec![
            json!({"NAME": "ALICE"}),
        ];
        let result = run_where("where name -eq alice", input);
        assert_eq!(result.len(), 1);
    }
}
