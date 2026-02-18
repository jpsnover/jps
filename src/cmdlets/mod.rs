pub mod convert_csv;
pub mod convert_json;
pub mod foreach_object;
pub mod format_list;
pub mod format_table;
pub mod measure_object;
pub mod select_object;
pub mod sort_object;
pub mod where_object;

use crate::ast::Command;
use crate::debug;
use crate::value::Value;
use std::collections::HashMap;

/// Error type for cmdlet execution
#[derive(Debug)]
pub struct CmdletError(pub String);

impl std::fmt::Display for CmdletError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for CmdletError {}

/// Result type for cmdlet execution
pub type CmdletResult<T> = Result<T, CmdletError>;

/// Output from a cmdlet - either objects to pass to next cmdlet, or formatted text for display
#[derive(Debug)]
pub enum CmdletOutput {
    /// Objects to pass to next cmdlet in pipeline
    Objects(Vec<Value>),
    /// Formatted text output (from Format-* cmdlets)
    Text(String),
}

/// Trait that all cmdlets must implement
pub trait Cmdlet: Send + Sync {
    /// Primary cmdlet name (e.g., "Where-Object")
    fn name(&self) -> &'static str;

    /// Aliases for this cmdlet (e.g., ["where", "?"])
    fn aliases(&self) -> &'static [&'static str];

    /// Execute the cmdlet with given input and arguments
    fn execute(&self, input: Vec<Value>, cmd: &Command) -> CmdletResult<CmdletOutput>;
}

/// Registry of all available cmdlets
pub struct CmdletRegistry {
    cmdlets: Vec<Box<dyn Cmdlet>>,
    name_map: HashMap<String, usize>,
}

impl CmdletRegistry {
    /// Create a new registry with all built-in cmdlets
    pub fn new() -> Self {
        let mut registry = CmdletRegistry {
            cmdlets: Vec::new(),
            name_map: HashMap::new(),
        };

        // Register all cmdlets
        registry.register(Box::new(where_object::WhereObject));
        registry.register(Box::new(select_object::SelectObject));
        registry.register(Box::new(sort_object::SortObject));
        registry.register(Box::new(format_table::FormatTable));
        registry.register(Box::new(format_list::FormatList));
        registry.register(Box::new(measure_object::MeasureObject));
        registry.register(Box::new(convert_json::ConvertToJson));
        registry.register(Box::new(convert_csv::ConvertToCsv));
        registry.register(Box::new(foreach_object::ForEachObject));

        registry
    }

    fn register(&mut self, cmdlet: Box<dyn Cmdlet>) {
        let idx = self.cmdlets.len();

        // Add primary name
        self.name_map.insert(cmdlet.name().to_lowercase(), idx);

        // Add aliases
        for alias in cmdlet.aliases() {
            self.name_map.insert(alias.to_lowercase(), idx);
        }

        // Add shortcut forms
        let name = cmdlet.name();
        if let Some(stripped) = name.strip_suffix("-Object") {
            self.name_map.insert(stripped.to_lowercase(), idx);
        }
        if let Some(stripped) = name.strip_prefix("Format-") {
            self.name_map.insert(stripped.to_lowercase(), idx);
        }
        if let Some(stripped) = name.strip_prefix("ConvertTo-") {
            self.name_map.insert(stripped.to_lowercase(), idx);
        }

        self.cmdlets.push(cmdlet);
    }

    /// Look up a cmdlet by name (case-insensitive)
    pub fn get(&self, name: &str) -> Option<&dyn Cmdlet> {
        let name_lower = name.to_lowercase();
        let result = self.name_map.get(&name_lower).map(|&idx| self.cmdlets[idx].as_ref());

        if let Some(cmdlet) = &result {
            debug::log(&format!(
                "Registry: '{}' resolved to '{}'",
                name,
                cmdlet.name()
            ));
        } else {
            debug::log(&format!("Registry: '{}' not found", name));
        }

        result
    }
}

impl Default for CmdletRegistry {
    fn default() -> Self {
        Self::new()
    }
}
