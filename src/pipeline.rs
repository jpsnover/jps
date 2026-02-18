use crate::ast::Pipeline;
use crate::cmdlets::{CmdletOutput, CmdletRegistry};
use crate::debug;
use crate::value::Value;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum PipelineError {
    #[error("Unknown command: {0}")]
    UnknownCommand(String),

    #[error("Command '{0}' failed: {1}")]
    CommandFailed(String, String),

    #[error("Parse error: {0}")]
    ParseError(String),
}

/// Execute a pipeline of commands
pub struct PipelineExecutor {
    registry: CmdletRegistry,
}

impl PipelineExecutor {
    pub fn new() -> Self {
        PipelineExecutor {
            registry: CmdletRegistry::new(),
        }
    }

    /// Execute a pipeline with given input
    pub fn execute(&self, pipeline: &Pipeline, input: Vec<Value>) -> Result<String, PipelineError> {
        debug::log("Pipeline: Starting execution");
        debug::indent();
        debug::log(&format!("Pipeline: Input contains {} object(s)", input.len()));

        let mut current = input;
        let mut final_output: Option<String> = None;

        for (i, cmd) in pipeline.commands.iter().enumerate() {
            debug::log(&format!(
                "Pipeline: Step {}/{} - Command '{}'",
                i + 1,
                pipeline.commands.len(),
                cmd.name
            ));
            debug::indent();

            let cmdlet = self
                .registry
                .get(&cmd.name)
                .ok_or_else(|| PipelineError::UnknownCommand(cmd.name.clone()))?;

            debug::log(&format!(
                "Pipeline: Resolved to cmdlet '{}'",
                cmdlet.name()
            ));
            debug::log(&format!("Pipeline: Input objects: {}", current.len()));

            match cmdlet.execute(current, cmd) {
                Ok(CmdletOutput::Objects(objects)) => {
                    debug::log(&format!("Pipeline: Output objects: {}", objects.len()));
                    current = objects;
                    // If this is the last command and it produced objects,
                    // we'll need to format them as JSON for output
                    if i == pipeline.commands.len() - 1 {
                        final_output = Some(objects_to_default_output(&current));
                    }
                }
                Ok(CmdletOutput::Text(text)) => {
                    debug::log(&format!(
                        "Pipeline: Text output ({} bytes)",
                        text.len()
                    ));
                    // Text output terminates the pipeline
                    final_output = Some(text);
                    debug::dedent();
                    break;
                }
                Err(e) => {
                    debug::log(&format!("Pipeline: Error - {}", e.0));
                    debug::dedent();
                    debug::dedent();
                    return Err(PipelineError::CommandFailed(cmd.name.clone(), e.0));
                }
            }
            debug::dedent();
        }

        debug::dedent();
        debug::log("Pipeline: Execution complete");

        Ok(final_output.unwrap_or_default())
    }
}

impl Default for PipelineExecutor {
    fn default() -> Self {
        Self::new()
    }
}

/// Convert objects to default output (JSON array or single object)
fn objects_to_default_output(objects: &[Value]) -> String {
    if objects.is_empty() {
        return String::new();
    }

    let json_vals: Vec<serde_json::Value> = objects
        .iter()
        .map(|v| v.inner().clone())
        .collect();

    if json_vals.len() == 1 {
        serde_json::to_string_pretty(&json_vals[0]).unwrap_or_default()
    } else {
        serde_json::to_string_pretty(&json_vals).unwrap_or_default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;
    use serde_json::json;

    fn execute(script: &str, input: Vec<serde_json::Value>) -> String {
        let pipeline = Parser::parse_script(script).unwrap();
        let input_values: Vec<Value> = input.into_iter().map(Value::new).collect();
        let executor = PipelineExecutor::new();
        executor.execute(&pipeline, input_values).unwrap()
    }

    #[test]
    fn test_simple_pipeline() {
        let input = vec![
            json!({"name": "alice", "age": 30}),
            json!({"name": "bob", "age": 25}),
        ];
        let result = execute("where age -gt 26", input);

        let parsed: serde_json::Value = serde_json::from_str(&result).unwrap();
        assert_eq!(parsed["name"], "alice");
    }

    #[test]
    fn test_chained_pipeline() {
        let input = vec![
            json!({"name": "alice", "age": 30, "city": "NYC"}),
            json!({"name": "bob", "age": 25, "city": "LA"}),
            json!({"name": "charlie", "age": 35, "city": "NYC"}),
        ];
        let result = execute("where age -gt 26 | select name, age | sort age -Descending", input);

        let parsed: Vec<serde_json::Value> = serde_json::from_str(&result).unwrap();
        assert_eq!(parsed.len(), 2);
        assert_eq!(parsed[0]["name"], "charlie");
        assert_eq!(parsed[1]["name"], "alice");
    }

    #[test]
    fn test_format_table_output() {
        let input = vec![
            json!({"name": "alice", "age": 30}),
            json!({"name": "bob", "age": 25}),
        ];
        let result = execute("table", input);

        assert!(result.contains("name"));
        assert!(result.contains("alice"));
        assert!(result.contains("bob"));
    }

    #[test]
    fn test_cmdlet_shortcuts() {
        let input = vec![json!({"value": 10}), json!({"value": 20})];

        // Test "where" instead of "Where-Object"
        let r1 = execute("where value -gt 15", input.clone());
        assert!(r1.contains("20"));

        // Test "select" instead of "Select-Object"
        let r2 = execute("select value", input.clone());
        assert!(r2.contains("value"));

        // Test "sort" instead of "Sort-Object"
        let r3 = execute("sort value -desc", input.clone());
        assert!(r3.contains("20"));
    }
}
