use crate::ast::{Argument, Command};
use crate::cmdlets::{Cmdlet, CmdletOutput, CmdletResult};
use crate::value::Value;
use std::cmp::Ordering;

/// Sort-Object cmdlet for sorting objects by property
pub struct SortObject;

impl Cmdlet for SortObject {
    fn name(&self) -> &'static str {
        "Sort-Object"
    }

    fn aliases(&self) -> &'static [&'static str] {
        &["sort"]
    }

    fn execute(&self, mut input: Vec<Value>, cmd: &Command) -> CmdletResult<CmdletOutput> {
        let properties = get_sort_properties(cmd)?;
        let descending = cmd.has_switch("descending") || cmd.has_switch("desc");
        let case_sensitive = cmd.has_switch("casesensitive");
        let unique = cmd.has_switch("unique");

        // Sort by properties
        input.sort_by(|a, b| {
            let mut result = Ordering::Equal;

            for prop in &properties {
                let a_val = a.get_property(prop);
                let b_val = b.get_property(prop);

                result = match (a_val, b_val) {
                    (Some(av), Some(bv)) => av.compare(&bv, case_sensitive),
                    (Some(_), None) => Ordering::Greater,
                    (None, Some(_)) => Ordering::Less,
                    (None, None) => Ordering::Equal,
                };

                if result != Ordering::Equal {
                    break;
                }
            }

            if descending {
                result.reverse()
            } else {
                result
            }
        });

        // Handle unique
        if unique && !properties.is_empty() {
            input = remove_duplicates(input, &properties, case_sensitive);
        }

        Ok(CmdletOutput::Objects(input))
    }
}

fn get_sort_properties(cmd: &Command) -> CmdletResult<Vec<String>> {
    // Check -Property parameter
    if let Some(prop_val) = cmd.get_parameter("property") {
        return Ok(prop_val.as_string_list());
    }

    // Check positional arguments
    let mut properties = Vec::new();
    for arg in &cmd.arguments {
        if let Argument::Positional(val) = arg {
            properties.extend(val.as_string_list());
        }
    }

    // If no properties specified, sort by string representation
    Ok(properties)
}

fn remove_duplicates(input: Vec<Value>, properties: &[String], case_sensitive: bool) -> Vec<Value> {
    let mut seen: Vec<Vec<String>> = Vec::new();
    let mut result = Vec::new();

    for obj in input {
        let key: Vec<String> = properties
            .iter()
            .map(|p| {
                obj.get_property(p)
                    .map(|v| {
                        let s = v.to_string_value();
                        if case_sensitive {
                            s
                        } else {
                            s.to_lowercase()
                        }
                    })
                    .unwrap_or_default()
            })
            .collect();

        if !seen.contains(&key) {
            seen.push(key);
            result.push(obj);
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;
    use serde_json::json;

    fn run_sort(script: &str, input: Vec<serde_json::Value>) -> Vec<Value> {
        let pipeline = Parser::parse_script(script).unwrap();
        let cmd = &pipeline.commands[0];
        let input_values: Vec<Value> = input.into_iter().map(Value::new).collect();

        match SortObject.execute(input_values, cmd).unwrap() {
            CmdletOutput::Objects(v) => v,
            _ => panic!("Expected objects output"),
        }
    }

    #[test]
    fn test_sort_ascending() {
        let input = vec![
            json!({"name": "charlie", "age": 35}),
            json!({"name": "alice", "age": 30}),
            json!({"name": "bob", "age": 25}),
        ];
        let result = run_sort("sort name", input);

        assert_eq!(result[0].get_property("name").unwrap().as_str(), Some("alice"));
        assert_eq!(result[1].get_property("name").unwrap().as_str(), Some("bob"));
        assert_eq!(result[2].get_property("name").unwrap().as_str(), Some("charlie"));
    }

    #[test]
    fn test_sort_descending() {
        let input = vec![
            json!({"name": "alice", "age": 30}),
            json!({"name": "bob", "age": 25}),
            json!({"name": "charlie", "age": 35}),
        ];
        let result = run_sort("sort age -Descending", input);

        assert_eq!(result[0].get_property("age").unwrap().as_i64(), Some(35));
        assert_eq!(result[1].get_property("age").unwrap().as_i64(), Some(30));
        assert_eq!(result[2].get_property("age").unwrap().as_i64(), Some(25));
    }

    #[test]
    fn test_sort_numeric() {
        let input = vec![
            json!({"val": 10}),
            json!({"val": 2}),
            json!({"val": 100}),
        ];
        let result = run_sort("sort val", input);

        assert_eq!(result[0].get_property("val").unwrap().as_i64(), Some(2));
        assert_eq!(result[1].get_property("val").unwrap().as_i64(), Some(10));
        assert_eq!(result[2].get_property("val").unwrap().as_i64(), Some(100));
    }

    #[test]
    fn test_sort_unique() {
        let input = vec![
            json!({"name": "alice"}),
            json!({"name": "bob"}),
            json!({"name": "alice"}),
        ];
        let result = run_sort("sort name -Unique", input);

        assert_eq!(result.len(), 2);
    }

    #[test]
    fn test_sort_desc_abbreviation() {
        let input = vec![
            json!({"age": 10}),
            json!({"age": 30}),
            json!({"age": 20}),
        ];
        let result = run_sort("sort age -desc", input);

        assert_eq!(result[0].get_property("age").unwrap().as_i64(), Some(30));
    }
}
