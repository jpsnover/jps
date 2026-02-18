//! Debug output module for jps
//!
//! Provides debug logging that can be directed to stderr or a file.

use std::fs::{File, OpenOptions};
use std::io::{self, Write};
use std::sync::{Mutex, OnceLock};

/// Global debug state
static DEBUG_STATE: OnceLock<Mutex<DebugState>> = OnceLock::new();

struct DebugState {
    enabled: bool,
    output: DebugOutput,
    indent_level: usize,
}

enum DebugOutput {
    Stderr,
    File(File),
}


/// Initialize the debug system
pub fn init(enabled: bool, file_path: Option<&str>) -> io::Result<()> {
    let output = if let Some(path) = file_path {
        let file = OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(path)?;
        DebugOutput::File(file)
    } else {
        DebugOutput::Stderr
    };

    let state = DebugState {
        enabled,
        output,
        indent_level: 0,
    };

    let _ = DEBUG_STATE.set(Mutex::new(state));
    Ok(())
}

/// Check if debug mode is enabled
pub fn is_enabled() -> bool {
    DEBUG_STATE
        .get()
        .map(|s| s.lock().unwrap().enabled)
        .unwrap_or(false)
}

/// Write a debug message
pub fn log(message: &str) {
    if let Some(state_mutex) = DEBUG_STATE.get() {
        let mut state = state_mutex.lock().unwrap();
        if !state.enabled {
            return;
        }

        let indent = "  ".repeat(state.indent_level);
        let formatted = format!("[DEBUG] {}{}\n", indent, message);

        match &mut state.output {
            DebugOutput::Stderr => {
                let _ = io::stderr().write_all(formatted.as_bytes());
            }
            DebugOutput::File(file) => {
                let _ = file.write_all(formatted.as_bytes());
            }
        }
    }
}

/// Write a debug message with formatting
#[macro_export]
macro_rules! debug_log {
    ($($arg:tt)*) => {
        if $crate::debug::is_enabled() {
            $crate::debug::log(&format!($($arg)*));
        }
    };
}

/// Increase indent level for nested operations
pub fn indent() {
    if let Some(state_mutex) = DEBUG_STATE.get() {
        let mut state = state_mutex.lock().unwrap();
        state.indent_level += 1;
    }
}

/// Decrease indent level
pub fn dedent() {
    if let Some(state_mutex) = DEBUG_STATE.get() {
        let mut state = state_mutex.lock().unwrap();
        if state.indent_level > 0 {
            state.indent_level -= 1;
        }
    }
}

/// Log entry into a section with automatic indentation
pub fn section<F, T>(name: &str, f: F) -> T
where
    F: FnOnce() -> T,
{
    log(&format!(">>> {}", name));
    indent();
    let result = f();
    dedent();
    log(&format!("<<< {}", name));
    result
}

/// Debug helper for logging values
pub fn log_value(name: &str, value: &impl std::fmt::Debug) {
    log(&format!("{}: {:?}", name, value));
}

/// Debug helper for logging JSON values
pub fn log_json(name: &str, value: &serde_json::Value) {
    if !is_enabled() {
        return;
    }

    match serde_json::to_string_pretty(value) {
        Ok(json) => {
            log(&format!("{}:", name));
            for line in json.lines() {
                log(&format!("  {}", line));
            }
        }
        Err(_) => log(&format!("{}: {:?}", name, value)),
    }
}
