use crate::ast::{Argument, Command};
use crate::cmdlets::{Cmdlet, CmdletOutput, CmdletResult};
use crate::value::Value;
use serde_json::{json, Map, Value as JsonValue};

/// Measure-Object cmdlet for statistics
pub struct MeasureObject;

impl Cmdlet for MeasureObject {
    fn name(&self) -> &'static str {
        "Measure-Object"
    }

    fn aliases(&self) -> &'static [&'static str] {
        &["measure"]
    }

    fn execute(&self, input: Vec<Value>, cmd: &Command) -> CmdletResult<CmdletOutput> {
        let property = get_property(cmd);
        let sum = cmd.has_switch("sum");
        let average = cmd.has_switch("average") || cmd.has_switch("avg");
        let minimum = cmd.has_switch("minimum") || cmd.has_switch("min");
        let maximum = cmd.has_switch("maximum") || cmd.has_switch("max");

        // If no specific stats requested, default to count only
        let include_stats = sum || average || minimum || maximum;

        let stats = calculate_stats(&input, &property, include_stats);
        let result = build_result(&property, &stats, sum, average, minimum, maximum);

        Ok(CmdletOutput::Objects(vec![Value::new(result)]))
    }
}

fn get_property(cmd: &Command) -> Option<String> {
    // Check -Property parameter
    if let Some(prop_val) = cmd.get_parameter("property") {
        return prop_val.as_string();
    }

    // Check positional arguments
    for arg in &cmd.arguments {
        if let Argument::Positional(val) = arg {
            if let Some(s) = val.as_string() {
                return Some(s);
            }
        }
    }

    None
}

struct Stats {
    count: usize,
    sum: f64,
    min: Option<f64>,
    max: Option<f64>,
}

fn calculate_stats(input: &[Value], property: &Option<String>, include_numeric: bool) -> Stats {
    let mut stats = Stats {
        count: input.len(),
        sum: 0.0,
        min: None,
        max: None,
    };

    if !include_numeric {
        return stats;
    }

    let values: Vec<f64> = if let Some(prop) = property {
        input
            .iter()
            .filter_map(|obj| obj.get_property(prop))
            .filter_map(|v| v.as_f64())
            .collect()
    } else {
        input
            .iter()
            .filter_map(|v| v.as_f64())
            .collect()
    };

    for val in &values {
        stats.sum += val;

        stats.min = Some(match stats.min {
            None => *val,
            Some(m) => m.min(*val),
        });

        stats.max = Some(match stats.max {
            None => *val,
            Some(m) => m.max(*val),
        });
    }

    stats
}

fn build_result(
    property: &Option<String>,
    stats: &Stats,
    sum: bool,
    average: bool,
    minimum: bool,
    maximum: bool,
) -> JsonValue {
    let mut result = Map::new();

    result.insert("Count".to_string(), json!(stats.count));

    if let Some(prop) = property {
        result.insert("Property".to_string(), json!(prop));
    }

    if sum {
        result.insert("Sum".to_string(), json!(stats.sum));
    }

    if average && stats.count > 0 {
        let avg = stats.sum / stats.count as f64;
        result.insert("Average".to_string(), json!(avg));
    }

    if minimum {
        result.insert(
            "Minimum".to_string(),
            stats.min.map(|v| json!(v)).unwrap_or(JsonValue::Null),
        );
    }

    if maximum {
        result.insert(
            "Maximum".to_string(),
            stats.max.map(|v| json!(v)).unwrap_or(JsonValue::Null),
        );
    }

    JsonValue::Object(result)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;
    use serde_json::json;

    fn run_measure(script: &str, input: Vec<serde_json::Value>) -> Value {
        let pipeline = Parser::parse_script(script).unwrap();
        let cmd = &pipeline.commands[0];
        let input_values: Vec<Value> = input.into_iter().map(Value::new).collect();

        match MeasureObject.execute(input_values, cmd).unwrap() {
            CmdletOutput::Objects(v) => v.into_iter().next().unwrap(),
            _ => panic!("Expected objects output"),
        }
    }

    #[test]
    fn test_count() {
        let input = vec![
            json!({"name": "alice"}),
            json!({"name": "bob"}),
            json!({"name": "charlie"}),
        ];
        let result = run_measure("measure", input);

        assert_eq!(result.get_property("Count").unwrap().as_i64(), Some(3));
    }

    #[test]
    fn test_sum() {
        let input = vec![
            json!({"value": 10}),
            json!({"value": 20}),
            json!({"value": 30}),
        ];
        let result = run_measure("measure value -Sum", input);

        assert_eq!(result.get_property("Sum").unwrap().as_f64(), Some(60.0));
    }

    #[test]
    fn test_average() {
        let input = vec![
            json!({"value": 10}),
            json!({"value": 20}),
            json!({"value": 30}),
        ];
        let result = run_measure("measure value -Average", input);

        assert_eq!(result.get_property("Average").unwrap().as_f64(), Some(20.0));
    }

    #[test]
    fn test_min_max() {
        let input = vec![
            json!({"value": 10}),
            json!({"value": 5}),
            json!({"value": 30}),
        ];
        let result = run_measure("measure value -Minimum -Maximum", input);

        assert_eq!(result.get_property("Minimum").unwrap().as_f64(), Some(5.0));
        assert_eq!(result.get_property("Maximum").unwrap().as_f64(), Some(30.0));
    }

    #[test]
    fn test_all_stats() {
        let input = vec![
            json!({"value": 10}),
            json!({"value": 20}),
        ];
        let result = run_measure("measure value -Sum -Average -Min -Max", input);

        assert_eq!(result.get_property("Count").unwrap().as_i64(), Some(2));
        assert_eq!(result.get_property("Sum").unwrap().as_f64(), Some(30.0));
        assert_eq!(result.get_property("Average").unwrap().as_f64(), Some(15.0));
        assert_eq!(result.get_property("Minimum").unwrap().as_f64(), Some(10.0));
        assert_eq!(result.get_property("Maximum").unwrap().as_f64(), Some(20.0));
    }
}
