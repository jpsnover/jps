use crate::ast::{Argument, ArgumentValue, CalculatedPropertyDef, Command};
use crate::cmdlets::{Cmdlet, CmdletOutput, CmdletResult};
use crate::expression;
use crate::expression::Alignment;
use crate::value::Value;
use unicode_width::UnicodeWidthStr;

/// Format-Table cmdlet for tabular output
pub struct FormatTable;

impl Cmdlet for FormatTable {
    fn name(&self) -> &'static str {
        "Format-Table"
    }

    fn aliases(&self) -> &'static [&'static str] {
        &["ft", "table"]
    }

    fn execute(&self, input: Vec<Value>, cmd: &Command) -> CmdletResult<CmdletOutput> {
        if input.is_empty() {
            return Ok(CmdletOutput::Text(String::new()));
        }

        let columns = get_display_columns(cmd, &input)?;
        let auto_size = cmd.has_switch("autosize") || cmd.has_switch("auto");
        let hide_headers = cmd.has_switch("hidetableheaders");
        let wrap = cmd.has_switch("wrap");

        let output = format_table(&input, &columns, auto_size, hide_headers, wrap);
        Ok(CmdletOutput::Text(output))
    }
}

/// A column specification for format-table
#[derive(Debug, Clone)]
pub enum ColumnSpec {
    /// Simple property name lookup
    Simple(String),
    /// Calculated column from a hashtable expression
    Calculated {
        header: String,
        expression: String,
        width: Option<usize>,
        alignment: Option<Alignment>,
        format_string: Option<String>,
    },
}

impl ColumnSpec {
    pub fn header(&self) -> &str {
        match self {
            ColumnSpec::Simple(name) => name.as_str(),
            ColumnSpec::Calculated { header, .. } => header.as_str(),
        }
    }

    pub fn fixed_width(&self) -> Option<usize> {
        match self {
            ColumnSpec::Simple(_) => None,
            ColumnSpec::Calculated { width, .. } => *width,
        }
    }

    pub fn alignment(&self) -> Option<Alignment> {
        match self {
            ColumnSpec::Simple(_) => None,
            ColumnSpec::Calculated { alignment, .. } => *alignment,
        }
    }

    pub fn format_string(&self) -> Option<&str> {
        match self {
            ColumnSpec::Simple(_) => None,
            ColumnSpec::Calculated { format_string, .. } => format_string.as_deref(),
        }
    }

    pub fn get_value(&self, obj: &Value) -> String {
        match self {
            ColumnSpec::Simple(name) => obj
                .get_property(name)
                .map(|v| v.to_string_value())
                .unwrap_or_default(),
            ColumnSpec::Calculated { expression, format_string, .. } => {
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

/// Apply a simple format string to a value
fn apply_format_string(raw: &str, fmt: &str, val: &serde_json::Value) -> String {
    // Support basic .NET-style format strings like "N2", "D", "X", "P"
    let fmt_upper = fmt.to_uppercase();
    if let Some(n) = val.as_f64() {
        if fmt_upper.starts_with('N') {
            let decimals = fmt[1..].parse::<usize>().unwrap_or(2);
            return format!("{:.prec$}", n, prec = decimals);
        } else if fmt_upper == "D" {
            return format!("{}", n as i64);
        } else if fmt_upper.starts_with('F') {
            let decimals = fmt[1..].parse::<usize>().unwrap_or(2);
            return format!("{:.prec$}", n, prec = decimals);
        } else if fmt_upper.starts_with('P') {
            let decimals = fmt[1..].parse::<usize>().unwrap_or(2);
            return format!("{:.prec$}%", n * 100.0, prec = decimals);
        }
    }
    // Fallback: return raw string
    raw.to_string()
}

fn from_calculated_def(calc: &CalculatedPropertyDef) -> ColumnSpec {
    let header = calc.output_name();
    let alignment = calc.alignment.as_deref().and_then(|a| match a.to_lowercase().as_str() {
        "left" | "l" => Some(Alignment::Left),
        "center" | "centre" | "c" => Some(Alignment::Center),
        "right" | "r" => Some(Alignment::Right),
        _ => None,
    });
    ColumnSpec::Calculated {
        header,
        expression: calc.expression.clone(),
        width: calc.width,
        alignment,
        format_string: calc.format_string.clone(),
    }
}

fn get_display_columns(cmd: &Command, input: &[Value]) -> CmdletResult<Vec<ColumnSpec>> {
    // Check -Property parameter
    if let Some(prop_val) = cmd.get_parameter("property") {
        return extract_columns(prop_val, input);
    }

    // Check positional arguments
    let mut columns = Vec::new();
    for arg in &cmd.arguments {
        if let Argument::Positional(val) = arg {
            let mut extracted = extract_columns(val, input)?;
            columns.append(&mut extracted);
        }
    }

    if !columns.is_empty() {
        return Ok(columns);
    }

    // Default: use all properties from first object
    if let Some(first) = input.first() {
        Ok(first
            .property_names()
            .into_iter()
            .map(ColumnSpec::Simple)
            .collect())
    } else {
        Ok(Vec::new())
    }
}

fn extract_columns(val: &ArgumentValue, input: &[Value]) -> CmdletResult<Vec<ColumnSpec>> {
    let mut cols = Vec::new();
    extract_columns_into(val, input, &mut cols);
    Ok(cols)
}

fn extract_columns_into(val: &ArgumentValue, input: &[Value], cols: &mut Vec<ColumnSpec>) {
    match val {
        ArgumentValue::Identifier(s) | ArgumentValue::String(s) => {
            // Expand wildcards
            if s.contains('*') || s.contains('?') {
                if let Some(first) = input.first() {
                    for (name, _) in first.get_properties_wildcard(s) {
                        if !cols.iter().any(|c| c.header().to_lowercase() == name.to_lowercase()) {
                            cols.push(ColumnSpec::Simple(name));
                        }
                    }
                }
            } else {
                cols.push(ColumnSpec::Simple(s.clone()));
            }
        }
        ArgumentValue::CalculatedProperty(calc) => {
            cols.push(from_calculated_def(calc));
        }
        ArgumentValue::List(items) => {
            for item in items {
                extract_columns_into(item, input, cols);
            }
        }
        _ => {}
    }
}

fn format_table(
    input: &[Value],
    columns: &[ColumnSpec],
    auto_size: bool,
    hide_headers: bool,
    wrap: bool,
) -> String {
    if columns.is_empty() {
        return String::new();
    }

    // Initialize widths from column headers
    let mut widths: Vec<usize> = columns
        .iter()
        .map(|c| {
            // If column has a fixed width, use that as minimum
            c.fixed_width()
                .unwrap_or_else(|| UnicodeWidthStr::width(c.header()))
        })
        .collect();

    // Collect cell values
    let mut rows: Vec<Vec<String>> = Vec::new();
    for obj in input {
        let row: Vec<String> = columns.iter().map(|col| col.get_value(obj)).collect();

        // Update widths
        for (i, cell) in row.iter().enumerate() {
            let cell_width = UnicodeWidthStr::width(cell.as_str());
            if i < widths.len() {
                // Fixed-width columns don't grow
                if columns[i].fixed_width().is_none() {
                    widths[i] = widths[i].max(cell_width);
                }
            }
        }

        rows.push(row);
    }

    // Apply auto-size or default max width
    if !auto_size {
        let max_width = 30;
        for (i, w) in widths.iter_mut().enumerate() {
            // Don't cap fixed-width columns
            if columns[i].fixed_width().is_none() {
                *w = (*w).min(max_width);
            }
        }
    }

    // Build output
    let mut output = String::new();

    // Header
    if !hide_headers {
        let header: Vec<String> = columns
            .iter()
            .zip(&widths)
            .map(|(col, w)| pad_or_truncate(col.header(), *w, wrap, None))
            .collect();
        output.push_str(&header.join(" "));
        output.push('\n');

        // Separator
        let sep: Vec<String> = widths.iter().map(|w| "-".repeat(*w)).collect();
        output.push_str(&sep.join(" "));
        output.push('\n');
    }

    // Data rows
    for row in rows {
        let formatted: Vec<String> = row
            .iter()
            .zip(columns.iter())
            .zip(&widths)
            .map(|((cell, col), w)| pad_or_truncate(cell, *w, wrap, col.alignment()))
            .collect();
        output.push_str(&formatted.join(" "));
        output.push('\n');
    }

    output
}

fn pad_or_truncate(s: &str, width: usize, _wrap: bool, alignment: Option<Alignment>) -> String {
    let s_width = UnicodeWidthStr::width(s);

    if s_width <= width {
        let padding = width - s_width;
        match alignment.unwrap_or(Alignment::Left) {
            Alignment::Left => format!("{}{}", s, " ".repeat(padding)),
            Alignment::Right => format!("{}{}", " ".repeat(padding), s),
            Alignment::Center => {
                let left_pad = padding / 2;
                let right_pad = padding - left_pad;
                format!("{}{}{}", " ".repeat(left_pad), s, " ".repeat(right_pad))
            }
        }
    } else {
        // Truncate with ellipsis
        let mut result = String::new();
        let mut current_width = 0;
        let target = width.saturating_sub(3);

        for c in s.chars() {
            let c_width = unicode_width::UnicodeWidthChar::width(c).unwrap_or(0);
            if current_width + c_width > target {
                break;
            }
            result.push(c);
            current_width += c_width;
        }

        if width >= 3 {
            result.push_str("...");
            let remaining = width - current_width - 3;
            result.push_str(&" ".repeat(remaining));
        }

        result
    }
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

        match FormatTable.execute(input_values, cmd).unwrap() {
            CmdletOutput::Text(s) => s,
            _ => panic!("Expected text output"),
        }
    }

    #[test]
    fn test_basic_table() {
        let input = vec![
            json!({"name": "alice", "age": 30}),
            json!({"name": "bob", "age": 25}),
        ];
        let result = run_format("table", input);

        assert!(result.contains("name"));
        assert!(result.contains("age"));
        assert!(result.contains("alice"));
        assert!(result.contains("bob"));
    }

    #[test]
    fn test_table_selected_properties() {
        let input = vec![
            json!({"name": "alice", "age": 30, "city": "NYC"}),
        ];
        let result = run_format("table name, age", input);

        assert!(result.contains("name"));
        assert!(result.contains("age"));
        assert!(!result.contains("city"));
    }

    #[test]
    fn test_hide_headers() {
        let input = vec![json!({"name": "alice"})];
        let result = run_format("table -HideTableHeaders", input);

        assert!(!result.contains("name"));
        assert!(result.contains("alice"));
    }

    #[test]
    fn test_calculated_property() {
        let input = vec![json!({"handles": 1500, "name": "proc1"})];
        let result = run_format("table -property name, @{name='TEST'; expression='handles'; width=15}", input);

        assert!(result.contains("TEST"));
        assert!(result.contains("1500"));
    }

    #[test]
    fn test_calculated_property_expression() {
        let input = vec![json!({"price": 10, "qty": 5})];
        let result = run_format("table @{name='Total'; expression='price * qty'}", input);

        assert!(result.contains("Total"));
        assert!(result.contains("50"));
    }
}
