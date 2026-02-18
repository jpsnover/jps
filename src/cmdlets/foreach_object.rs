use crate::ast::{Argument, ArgumentValue, Command};
use crate::cmdlets::{Cmdlet, CmdletError, CmdletOutput, CmdletResult};
use crate::debug;
use crate::expression;
use crate::value::Value;
use serde_json::Value as JsonValue;

/// ForEach-Object cmdlet - performs operations on each item in a collection
pub struct ForEachObject;

impl Cmdlet for ForEachObject {
    fn name(&self) -> &'static str {
        "ForEach-Object"
    }

    fn aliases(&self) -> &'static [&'static str] {
        &["foreach", "%"]
    }

    fn execute(&self, input: Vec<Value>, cmd: &Command) -> CmdletResult<CmdletOutput> {
        // Check for -MemberName parameter (property/method access shorthand)
        if let Some(member_name) = get_member_name(cmd) {
            return execute_member_access(input, &member_name, cmd);
        }

        // Gather script blocks
        let (begin_blocks, process_blocks, end_blocks) = collect_script_blocks(cmd)?;

        debug::log(&format!(
            "ForEach-Object: begin={}, process={}, end={} blocks",
            begin_blocks.len(),
            process_blocks.len(),
            end_blocks.len()
        ));

        let mut results: Vec<Value> = Vec::new();

        // Execute Begin blocks (once, before any input)
        for block in &begin_blocks {
            debug::log(&format!("ForEach-Object: Running begin block: {}", block));
            if let Some(val) = eval_script_block(block, &Value::new(JsonValue::Null))? {
                if !val.is_null() {
                    results.push(Value::new(val));
                }
            }
        }

        // Execute Process blocks for each input object
        for obj in &input {
            for block in &process_blocks {
                if let Some(val) = eval_script_block(block, obj)? {
                    if !val.is_null() {
                        results.push(Value::new(val));
                    }
                }
            }
        }

        // Execute End blocks (once, after all input)
        for block in &end_blocks {
            debug::log(&format!("ForEach-Object: Running end block: {}", block));
            if let Some(val) = eval_script_block(block, &Value::new(JsonValue::Null))? {
                if !val.is_null() {
                    results.push(Value::new(val));
                }
            }
        }

        Ok(CmdletOutput::Objects(results))
    }
}

/// Evaluate a script block string as an expression against an object
fn eval_script_block(block: &str, obj: &Value) -> CmdletResult<Option<JsonValue>> {
    let trimmed = block.trim();
    if trimmed.is_empty() {
        return Ok(None);
    }
    match expression::evaluate(trimmed, obj) {
        Ok(val) => Ok(Some(val)),
        Err(e) => {
            debug::log(&format!("ForEach-Object: block error '{}': {}", trimmed, e));
            Ok(None)
        }
    }
}

/// Collect Begin, Process, and End script blocks from command arguments
fn collect_script_blocks(cmd: &Command) -> CmdletResult<(Vec<String>, Vec<String>, Vec<String>)> {
    let mut begin_blocks: Vec<String> = Vec::new();
    let mut process_blocks: Vec<String> = Vec::new();
    let mut end_blocks: Vec<String> = Vec::new();

    let has_named = has_named_block_params(cmd);

    if has_named {
        if let Some(b) = get_script_block_param(cmd, "begin").or_else(|| get_script_block_param(cmd, "b")) {
            begin_blocks.push(b);
        }
        for block in get_script_block_param_all(cmd, "process") {
            process_blocks.push(block);
        }
        if let Some(e) = get_script_block_param(cmd, "end").or_else(|| get_script_block_param(cmd, "e")) {
            end_blocks.push(e);
        }
        // Also pick up positional script blocks as additional process blocks
        if process_blocks.is_empty() {
            process_blocks.extend(get_positional_script_blocks(cmd));
        }
    } else {
        // Positional-only style - map by count
        let positional: Vec<String> = get_positional_script_blocks(cmd);
        match positional.len() {
            0 => {}
            1 => process_blocks.push(positional.into_iter().next().unwrap()),
            2 => {
                let mut it = positional.into_iter();
                begin_blocks.push(it.next().unwrap());
                process_blocks.push(it.next().unwrap());
            }
            _ => {
                let mut it = positional.into_iter();
                begin_blocks.push(it.next().unwrap());
                let collected: Vec<String> = it.collect();
                let last_idx = collected.len() - 1;
                for (idx, block) in collected.into_iter().enumerate() {
                    if idx == last_idx {
                        end_blocks.push(block);
                    } else {
                        process_blocks.push(block);
                    }
                }
            }
        }
    }

    Ok((begin_blocks, process_blocks, end_blocks))
}

fn has_named_block_params(cmd: &Command) -> bool {
    for arg in &cmd.arguments {
        if let Argument::Parameter { name, .. } = arg {
            let lower = name.to_lowercase();
            if lower.starts_with("begin") || lower == "b"
                || lower.starts_with("process") || lower == "p"
                || lower.starts_with("end") || lower == "e"
            {
                return true;
            }
        }
    }
    false
}

fn get_script_block_param(cmd: &Command, name: &str) -> Option<String> {
    let name_lower = name.to_lowercase();
    for arg in &cmd.arguments {
        if let Argument::Parameter { name: param_name, value: Some(val) } = arg {
            let param_lower = param_name.to_lowercase();
            if param_lower == name_lower
                || name_lower.starts_with(&param_lower)
                || param_lower.starts_with(&name_lower)
            {
                if let ArgumentValue::ScriptBlock(content) = val {
                    return Some(content.clone());
                }
            }
        }
    }
    None
}

fn get_script_block_param_all(cmd: &Command, name: &str) -> Vec<String> {
    let name_lower = name.to_lowercase();
    let mut blocks = Vec::new();
    for arg in &cmd.arguments {
        if let Argument::Parameter { name: param_name, value: Some(val) } = arg {
            let param_lower = param_name.to_lowercase();
            if param_lower == name_lower
                || name_lower.starts_with(&param_lower)
                || param_lower.starts_with(&name_lower)
            {
                extract_script_blocks(val, &mut blocks);
            }
        }
    }
    blocks
}

fn get_positional_script_blocks(cmd: &Command) -> Vec<String> {
    let mut blocks = Vec::new();
    for arg in &cmd.arguments {
        if let Argument::Positional(val) = arg {
            extract_script_blocks(val, &mut blocks);
        }
    }
    blocks
}

fn extract_script_blocks(val: &ArgumentValue, blocks: &mut Vec<String>) {
    match val {
        ArgumentValue::ScriptBlock(content) => blocks.push(content.clone()),
        ArgumentValue::List(items) => {
            for item in items {
                extract_script_blocks(item, blocks);
            }
        }
        _ => {}
    }
}

/// Get the -MemberName parameter value, or infer it from positional args (no script blocks)
fn get_member_name(cmd: &Command) -> Option<String> {
    // Check -MemberName or abbreviations
    if let Some(val) = cmd.get_parameter("membername").or_else(|| cmd.get_parameter("member")) {
        return val.as_string();
    }

    // Shorthand: `foreach PropertyName` where there are no script blocks
    let positional = cmd.get_positional_args();
    if !positional.is_empty() {
        let has_blocks = positional.iter().any(|v| matches!(v, ArgumentValue::ScriptBlock(_)));
        if !has_blocks {
            if let Some(ArgumentValue::Identifier(name)) = positional.first() {
                return Some(name.clone());
            }
        }
    }

    None
}

fn get_argument_list(cmd: &Command) -> Vec<JsonValue> {
    let mut args = Vec::new();
    if let Some(val) = cmd.get_parameter("argumentlist").or_else(|| cmd.get_parameter("args")) {
        collect_json_values(val, &mut args);
    }
    // Also check positional args after the first (for shorthand `foreach Split "."`)
    let positionals = cmd.get_positional_args();
    if positionals.len() > 1 {
        for val in positionals.iter().skip(1) {
            collect_json_values(val, &mut args);
        }
    }
    args
}

fn collect_json_values(val: &ArgumentValue, args: &mut Vec<JsonValue>) {
    match val {
        ArgumentValue::String(s) => args.push(JsonValue::String(s.clone())),
        ArgumentValue::Identifier(s) => args.push(JsonValue::String(s.clone())),
        ArgumentValue::Number(n) => {
            args.push(
                serde_json::Number::from_f64(*n)
                    .map(JsonValue::Number)
                    .unwrap_or(JsonValue::Null),
            );
        }
        ArgumentValue::Boolean(b) => args.push(JsonValue::Bool(*b)),
        ArgumentValue::List(items) => {
            for item in items {
                collect_json_values(item, args);
            }
        }
        _ => {}
    }
}

fn execute_member_access(input: Vec<Value>, member_name: &str, cmd: &Command) -> CmdletResult<CmdletOutput> {
    let arg_list = get_argument_list(cmd);
    let mut results = Vec::new();

    for obj in &input {
        if !arg_list.is_empty() {
            // Method call with arguments
            let args_str: Vec<String> = arg_list
                .iter()
                .map(|v| match v {
                    JsonValue::String(s) => format!("\"{}\"", s.replace('"', "\\\"")),
                    JsonValue::Number(n) => n.to_string(),
                    JsonValue::Bool(b) => b.to_string(),
                    _ => String::new(),
                })
                .collect();
            let expr_str = format!("$_.{}({})", member_name, args_str.join(", "));
            match expression::evaluate(&expr_str, obj) {
                Ok(val) => push_value_or_array(&mut results, val),
                Err(e) => {
                    return Err(CmdletError(format!(
                        "Member '{}' call error: {}",
                        member_name, e
                    )));
                }
            }
        } else {
            // Property access
            if let Some(val) = obj.get_property(member_name) {
                let inner = val.into_inner();
                push_value_or_array(&mut results, inner);
            } else {
                // Not found as property - try as method via expression evaluator
                let expr_str = format!("$_.{}", member_name);
                if let Ok(val) = expression::evaluate(&expr_str, obj) {
                    if !val.is_null() {
                        push_value_or_array(&mut results, val);
                    }
                }
                // If method not found, just skip (no output for this object)
            }
        }
    }

    Ok(CmdletOutput::Objects(results))
}

/// Push a value to results; if it's an array, push each element individually
fn push_value_or_array(results: &mut Vec<Value>, val: JsonValue) {
    match val {
        JsonValue::Array(arr) => {
            for item in arr {
                results.push(Value::new(item));
            }
        }
        other => results.push(Value::new(other)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;
    use serde_json::json;

    fn run_foreach(script: &str, input: Vec<serde_json::Value>) -> Vec<Value> {
        let pipeline = Parser::parse_script(script).unwrap();
        let cmd = &pipeline.commands[0];
        let input_values: Vec<Value> = input.into_iter().map(Value::new).collect();

        match ForEachObject.execute(input_values, cmd).unwrap() {
            CmdletOutput::Objects(v) => v,
            CmdletOutput::Text(t) => panic!("Expected objects, got text: {}", t),
        }
    }

    #[test]
    fn test_simple_expression() {
        let input = vec![json!(30000), json!(56798), json!(12432)];
        let result = run_foreach("foreach { $_ / 1024 }", input);
        assert_eq!(result.len(), 3);
    }

    #[test]
    fn test_member_name() {
        let input = vec![
            json!({"name": "alice", "age": 30}),
            json!({"name": "bob", "age": 25}),
        ];
        let result = run_foreach("foreach -MemberName name", input);
        assert_eq!(result.len(), 2);
        assert_eq!(result[0].as_str(), Some("alice"));
        assert_eq!(result[1].as_str(), Some("bob"));
    }

    #[test]
    fn test_process_block() {
        let input = vec![json!({"value": 10}), json!({"value": 20})];
        let result = run_foreach("foreach { $_.value * 2 }", input);
        assert_eq!(result.len(), 2);
        assert_eq!(result[0].as_i64(), Some(20));
        assert_eq!(result[1].as_i64(), Some(40));
    }

    #[test]
    fn test_named_process_block() {
        let input = vec![json!({"x": 5}), json!({"x": 10})];
        let result = run_foreach("foreach -Process { $_.x * 3 }", input);
        assert_eq!(result.len(), 2);
        assert_eq!(result[0].as_i64(), Some(15));
        assert_eq!(result[1].as_i64(), Some(30));
    }

    #[test]
    fn test_method_call_member() {
        let input = vec![
            json!({"name": "alice"}),
            json!({"name": "bob"}),
        ];
        let result = run_foreach("foreach { $_.name.ToUpper() }", input);
        assert_eq!(result.len(), 2);
        assert_eq!(result[0].as_str(), Some("ALICE"));
        assert_eq!(result[1].as_str(), Some("BOB"));
    }

    #[test]
    fn test_split_member_with_args() {
        let input = vec![json!("Microsoft.PowerShell.Core")];
        let result = run_foreach("foreach -MemberName Split -ArgumentList .", input);
        // Should split into parts
        assert!(result.len() >= 1);
    }

    #[test]
    fn test_two_positional_blocks() {
        // 2 blocks: begin + process
        let input = vec![json!({"x": 5})];
        let result = run_foreach("foreach { 'start' } { $_.x }", input);
        // begin produces "start", process produces 5
        assert_eq!(result.len(), 2);
    }
}
