use crate::ast::{Argument, ArgumentValue, CalculatedPropertyDef, Command};
use crate::cmdlets::{Cmdlet, CmdletError, CmdletOutput, CmdletResult};
use crate::debug;
use crate::expression;
use crate::value::Value;
use serde_json::{Map, Value as JsonValue};

/// Select-Object cmdlet for property selection and object limiting
pub struct SelectObject;

impl Cmdlet for SelectObject {
    fn name(&self) -> &'static str {
        "Select-Object"
    }

    fn aliases(&self) -> &'static [&'static str] {
        &["select"]
    }

    fn execute(&self, input: Vec<Value>, cmd: &Command) -> CmdletResult<CmdletOutput> {
        // Get limiting parameters
        let first = get_int_param(cmd, "first");
        let last = get_int_param(cmd, "last");
        let skip = get_int_param(cmd, "skip").unwrap_or(0);

        // Get -ExpandProperty parameter
        let expand_property = get_string_param(cmd, "expandproperty");

        debug::log(&format!(
            "Select-Object: first={:?}, last={:?}, skip={}, expand={:?}",
            first, last, skip, expand_property
        ));

        // Get property selection (including calculated properties)
        let properties = get_property_specs(cmd)?;

        for (i, prop) in properties.iter().enumerate() {
            match prop {
                PropertySpec::Simple(name) => {
                    debug::log(&format!("Select-Object: Property[{}] = '{}'", i, name));
                }
                PropertySpec::Calculated(calc) => {
                    debug::log(&format!(
                        "Select-Object: Property[{}] = Calculated(name='{}', expr='{}')",
                        i,
                        calc.output_name(),
                        calc.expression
                    ));
                }
            }
        }

        // Apply skip/first/last
        let objects = apply_limits(input, skip, first, last);
        debug::log(&format!(
            "Select-Object: After limits, {} object(s)",
            objects.len()
        ));

        // Handle -ExpandProperty
        if let Some(ref expand_prop) = expand_property {
            let result = expand_property_values(&objects, expand_prop, &properties)?;
            debug::log(&format!(
                "Select-Object: After expand, {} object(s)",
                result.len()
            ));
            return Ok(CmdletOutput::Objects(result));
        }

        // Apply property selection
        let result = if properties.is_empty() {
            objects
        } else {
            objects
                .into_iter()
                .map(|obj| select_properties(&obj, &properties))
                .collect::<Result<Vec<_>, _>>()?
        };

        Ok(CmdletOutput::Objects(result))
    }
}

/// Represents either a simple property name or a calculated property
#[derive(Debug, Clone)]
enum PropertySpec {
    Simple(String),
    Calculated(CalculatedPropertyDef),
}

fn get_int_param(cmd: &Command, name: &str) -> Option<usize> {
    cmd.get_parameter(name)
        .and_then(|v| v.as_i64())
        .map(|n| n.max(0) as usize)
}

fn get_string_param(cmd: &Command, name: &str) -> Option<String> {
    cmd.get_parameter(name).and_then(|v| v.as_string())
}

fn get_property_specs(cmd: &Command) -> CmdletResult<Vec<PropertySpec>> {
    let mut specs = Vec::new();

    // Check -Property parameter
    if let Some(prop_val) = cmd.get_parameter("property") {
        extract_property_specs(prop_val, &mut specs)?;
        return Ok(specs);
    }

    // Check positional arguments
    for arg in &cmd.arguments {
        if let Argument::Positional(val) = arg {
            extract_property_specs(val, &mut specs)?;
        }
    }

    Ok(specs)
}

fn extract_property_specs(val: &ArgumentValue, specs: &mut Vec<PropertySpec>) -> CmdletResult<()> {
    match val {
        ArgumentValue::Identifier(s) | ArgumentValue::String(s) => {
            specs.push(PropertySpec::Simple(s.clone()));
        }
        ArgumentValue::CalculatedProperty(calc) => {
            specs.push(PropertySpec::Calculated(calc.clone()));
        }
        ArgumentValue::List(items) => {
            for item in items {
                extract_property_specs(item, specs)?;
            }
        }
        _ => {}
    }
    Ok(())
}

fn apply_limits(input: Vec<Value>, skip: usize, first: Option<usize>, last: Option<usize>) -> Vec<Value> {
    let len = input.len();

    // Apply skip
    let start = skip.min(len);

    // Apply first/last
    let (start, end) = if let Some(n) = last {
        // Last N items (after skip)
        let remaining = len.saturating_sub(skip);
        let take = n.min(remaining);
        (len.saturating_sub(take), len)
    } else if let Some(n) = first {
        // First N items (after skip)
        (start, (start + n).min(len))
    } else {
        (start, len)
    };

    input.into_iter().skip(start).take(end - start).collect()
}

fn select_properties(obj: &Value, properties: &[PropertySpec]) -> CmdletResult<Value> {
    let mut result = Map::new();

    for prop in properties {
        match prop {
            PropertySpec::Simple(name) => {
                // Handle wildcard properties
                if name.contains('*') || name.contains('?') {
                    for (prop_name, val) in obj.get_properties_wildcard(name) {
                        result.insert(prop_name, val.into_inner());
                    }
                } else {
                    // Regular property lookup
                    if let Some(val) = obj.get_property(name) {
                        // Use the original property name from the object if possible
                        let actual_name = find_actual_property_name(obj, name)
                            .unwrap_or_else(|| name.clone());
                        result.insert(actual_name, val.into_inner());
                    }
                }
            }
            PropertySpec::Calculated(calc) => {
                // Evaluate the expression
                let value = expression::evaluate(&calc.expression, obj)
                    .map_err(|e| CmdletError(format!("Expression error: {}", e)))?;
                let output_name = calc.output_name();
                result.insert(output_name, value);
            }
        }
    }

    Ok(Value::new(JsonValue::Object(result)))
}

fn find_actual_property_name(obj: &Value, prop: &str) -> Option<String> {
    let prop_lower = prop.to_lowercase();
    obj.property_names()
        .into_iter()
        .find(|name| name.to_lowercase() == prop_lower)
}

/// Expand a property value into multiple output objects
/// If the property is an array, each element becomes an output
/// If the property is an object, it becomes the output (with additional properties merged)
fn expand_property_values(
    objects: &[Value],
    expand_prop: &str,
    additional_properties: &[PropertySpec],
) -> CmdletResult<Vec<Value>> {
    let mut result = Vec::new();

    for obj in objects {
        // Get the property to expand
        let prop_value = obj.get_property(expand_prop).ok_or_else(|| {
            CmdletError(format!(
                "Property '{}' not found on input object",
                expand_prop
            ))
        })?;

        debug::log(&format!(
            "Select-Object: Expanding property '{}' of type {:?}",
            expand_prop,
            get_json_type(&prop_value)
        ));

        // Get additional properties to merge (if -Property was specified)
        let extra_props = if additional_properties.is_empty() {
            Map::new()
        } else {
            let selected = select_properties(obj, additional_properties)?;
            match selected.into_inner() {
                JsonValue::Object(map) => map,
                _ => Map::new(),
            }
        };

        match prop_value.into_inner() {
            JsonValue::Array(arr) => {
                // Each array element becomes an output object
                for item in arr {
                    let expanded = expand_single_value(item, &extra_props)?;
                    result.push(expanded);
                }
            }
            JsonValue::Object(map) => {
                // Object properties are expanded, with extra props merged
                let expanded = expand_object_value(map, &extra_props)?;
                result.push(expanded);
            }
            other => {
                // Scalar value - just wrap it with extra props
                let expanded = expand_scalar_value(other, &extra_props);
                result.push(expanded);
            }
        }
    }

    Ok(result)
}

fn expand_single_value(value: JsonValue, extra_props: &Map<String, JsonValue>) -> CmdletResult<Value> {
    match value {
        JsonValue::Object(mut map) => {
            merge_with_rename(&mut map, extra_props);
            Ok(Value::new(JsonValue::Object(map)))
        }
        other => {
            // Scalar value in array - wrap with extra props
            Ok(expand_scalar_value(other, extra_props))
        }
    }
}

fn expand_object_value(mut map: Map<String, JsonValue>, extra_props: &Map<String, JsonValue>) -> CmdletResult<Value> {
    merge_with_rename(&mut map, extra_props);
    Ok(Value::new(JsonValue::Object(map)))
}

/// Merge extra_props into map, renaming colliding keys with "O" prefix (e.g., Name → OName)
fn merge_with_rename(map: &mut Map<String, JsonValue>, extra_props: &Map<String, JsonValue>) {
    for (key, val) in extra_props {
        // Find any existing key with same name (case-insensitive)
        let existing_key = map
            .keys()
            .find(|k| k.to_lowercase() == key.to_lowercase())
            .cloned();

        if let Some(existing) = existing_key {
            // Rename the existing property by prepending "O"
            let renamed = format!("O{}", existing);
            let existing_val = map.remove(&existing).unwrap();
            map.insert(renamed, existing_val);
        }

        map.insert(key.clone(), val.clone());
    }
}

fn expand_scalar_value(value: JsonValue, extra_props: &Map<String, JsonValue>) -> Value {
    let mut map = extra_props.clone();
    // For scalar values, we add them as a "Value" property if no extra props,
    // or just return the extra props if there are some
    if map.is_empty() {
        map.insert("Value".to_string(), value);
    }
    Value::new(JsonValue::Object(map))
}

fn get_json_type(value: &Value) -> &'static str {
    match value.inner() {
        JsonValue::Null => "Null",
        JsonValue::Bool(_) => "Boolean",
        JsonValue::Number(_) => "Number",
        JsonValue::String(_) => "String",
        JsonValue::Array(_) => "Array",
        JsonValue::Object(_) => "Object",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;
    use serde_json::json;

    fn run_select(script: &str, input: Vec<serde_json::Value>) -> Vec<Value> {
        let pipeline = Parser::parse_script(script).unwrap();
        let cmd = &pipeline.commands[0];
        let input_values: Vec<Value> = input.into_iter().map(Value::new).collect();

        match SelectObject.execute(input_values, cmd).unwrap() {
            CmdletOutput::Objects(v) => v,
            _ => panic!("Expected objects output"),
        }
    }

    #[test]
    fn test_select_properties() {
        let input = vec![json!({"name": "alice", "age": 30, "city": "NYC"})];
        let result = run_select("select name, age", input);

        assert_eq!(result.len(), 1);
        assert!(result[0].get_property("name").is_some());
        assert!(result[0].get_property("age").is_some());
        assert!(result[0].get_property("city").is_none());
    }

    #[test]
    fn test_select_wildcard() {
        let input = vec![json!({"UserName": "alice", "UserID": 123, "Email": "test@test.com"})];
        let result = run_select("select user*", input);

        assert_eq!(result.len(), 1);
        assert!(result[0].get_property("UserName").is_some());
        assert!(result[0].get_property("UserID").is_some());
        assert!(result[0].get_property("Email").is_none());
    }

    #[test]
    fn test_select_first() {
        let input = vec![
            json!({"id": 1}),
            json!({"id": 2}),
            json!({"id": 3}),
        ];
        let result = run_select("select -First 2", input);

        assert_eq!(result.len(), 2);
        assert_eq!(result[0].get_property("id").unwrap().as_i64(), Some(1));
        assert_eq!(result[1].get_property("id").unwrap().as_i64(), Some(2));
    }

    #[test]
    fn test_select_last() {
        let input = vec![
            json!({"id": 1}),
            json!({"id": 2}),
            json!({"id": 3}),
        ];
        let result = run_select("select -Last 2", input);

        assert_eq!(result.len(), 2);
        assert_eq!(result[0].get_property("id").unwrap().as_i64(), Some(2));
        assert_eq!(result[1].get_property("id").unwrap().as_i64(), Some(3));
    }

    #[test]
    fn test_select_skip() {
        let input = vec![
            json!({"id": 1}),
            json!({"id": 2}),
            json!({"id": 3}),
        ];
        let result = run_select("select -Skip 1", input);

        assert_eq!(result.len(), 2);
        assert_eq!(result[0].get_property("id").unwrap().as_i64(), Some(2));
    }

    #[test]
    fn test_case_insensitive_select() {
        let input = vec![json!({"NAME": "alice", "AGE": 30})];
        let result = run_select("select name, age", input);

        assert_eq!(result.len(), 1);
        // Should preserve original case
        assert!(result[0].get_property("NAME").is_some());
    }

    #[test]
    fn test_calculated_property() {
        let input = vec![json!({"price": 10, "quantity": 5})];
        let result = run_select("select @{Name='Total'; Expression='price * quantity'}", input);

        assert_eq!(result.len(), 1);
        assert_eq!(result[0].get_property("Total").unwrap().as_i64(), Some(50));
    }

    #[test]
    fn test_calculated_property_with_regular() {
        let input = vec![json!({"name": "Widget", "price": 10, "quantity": 5})];
        let result = run_select("select name, @{n='Total'; e='price * quantity'}", input);

        assert_eq!(result.len(), 1);
        assert_eq!(result[0].get_property("name").unwrap().as_str(), Some("Widget"));
        assert_eq!(result[0].get_property("Total").unwrap().as_i64(), Some(50));
    }

    #[test]
    fn test_calculated_property_string_concat() {
        let input = vec![json!({"firstName": "John", "lastName": "Doe"})];
        let result = run_select("select @{Name='FullName'; Expression=\"firstName + ' ' + lastName\"}", input);

        assert_eq!(result.len(), 1);
        assert_eq!(result[0].get_property("FullName").unwrap().as_str(), Some("John Doe"));
    }

    #[test]
    fn test_calculated_property_function() {
        let input = vec![json!({"name": "alice"})];
        let result = run_select("select @{n='UpperName'; e='upper(name)'}", input);

        assert_eq!(result.len(), 1);
        assert_eq!(result[0].get_property("UpperName").unwrap().as_str(), Some("ALICE"));
    }

    #[test]
    fn test_expand_property_array() {
        let input = vec![json!({"name": "alice", "tags": ["admin", "user", "dev"]})];
        let result = run_select("select -ExpandProperty tags", input);

        // Each array element becomes a separate output
        assert_eq!(result.len(), 3);
    }

    #[test]
    fn test_expand_property_object() {
        let input = vec![json!({
            "name": "alice",
            "address": {"city": "NYC", "zip": "10001"}
        })];
        let result = run_select("select -ExpandProperty address", input);

        assert_eq!(result.len(), 1);
        assert_eq!(result[0].get_property("city").unwrap().as_str(), Some("NYC"));
        assert_eq!(result[0].get_property("zip").unwrap().as_str(), Some("10001"));
    }

    #[test]
    fn test_expand_property_with_additional_props() {
        let input = vec![json!({
            "name": "alice",
            "address": {"city": "NYC", "zip": "10001"}
        })];
        let result = run_select("select -ExpandProperty address -Property name", input);

        assert_eq!(result.len(), 1);
        // Should have expanded address properties plus name
        assert_eq!(result[0].get_property("city").unwrap().as_str(), Some("NYC"));
        assert_eq!(result[0].get_property("name").unwrap().as_str(), Some("alice"));
    }

    #[test]
    fn test_expand_property_array_of_objects() {
        let input = vec![json!({
            "user": "alice",
            "orders": [
                {"id": 1, "amount": 100},
                {"id": 2, "amount": 200}
            ]
        })];
        let result = run_select("select -ExpandProperty orders -Property user", input);

        assert_eq!(result.len(), 2);
        // First order
        assert_eq!(result[0].get_property("id").unwrap().as_i64(), Some(1));
        assert_eq!(result[0].get_property("user").unwrap().as_str(), Some("alice"));
        // Second order
        assert_eq!(result[1].get_property("id").unwrap().as_i64(), Some(2));
        assert_eq!(result[1].get_property("user").unwrap().as_str(), Some("alice"));
    }
}
