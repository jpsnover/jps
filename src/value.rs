use serde_json::Value as JsonValue;
use std::cmp::Ordering;

/// Wrapper around serde_json::Value providing case-insensitive property access
/// and wildcard matching support.
#[derive(Debug, Clone)]
pub struct Value(pub JsonValue);

impl Value {
    pub fn new(v: JsonValue) -> Self {
        Value(v)
    }

    pub fn inner(&self) -> &JsonValue {
        &self.0
    }

    pub fn into_inner(self) -> JsonValue {
        self.0
    }

    /// Get a property by exact name (case-insensitive)
    pub fn get_property(&self, name: &str) -> Option<Value> {
        match &self.0 {
            JsonValue::Object(map) => {
                let name_lower = name.to_lowercase();
                for (k, v) in map {
                    if k.to_lowercase() == name_lower {
                        return Some(Value::new(v.clone()));
                    }
                }
                None
            }
            _ => None,
        }
    }

    /// Get properties matching a wildcard pattern (case-insensitive)
    /// Returns vec of (property_name, value) pairs
    pub fn get_properties_wildcard(&self, pattern: &str) -> Vec<(String, Value)> {
        match &self.0 {
            JsonValue::Object(map) => {
                let mut results = Vec::new();
                for (k, v) in map {
                    if wildcard_match(&k.to_lowercase(), &pattern.to_lowercase()) {
                        results.push((k.clone(), Value::new(v.clone())));
                    }
                }
                results
            }
            _ => Vec::new(),
        }
    }

    /// Get all property names
    pub fn property_names(&self) -> Vec<String> {
        match &self.0 {
            JsonValue::Object(map) => map.keys().cloned().collect(),
            _ => Vec::new(),
        }
    }

    /// Check if value is null
    pub fn is_null(&self) -> bool {
        self.0.is_null()
    }

    /// Check if value is an object
    pub fn is_object(&self) -> bool {
        self.0.is_object()
    }

    /// Check if value is an array
    pub fn is_array(&self) -> bool {
        self.0.is_array()
    }

    /// Get as f64 if numeric
    pub fn as_f64(&self) -> Option<f64> {
        self.0.as_f64()
    }

    /// Get as i64 if integer
    pub fn as_i64(&self) -> Option<i64> {
        self.0.as_i64()
    }

    /// Get as bool
    pub fn as_bool(&self) -> Option<bool> {
        self.0.as_bool()
    }

    /// Get as string
    pub fn as_str(&self) -> Option<&str> {
        self.0.as_str()
    }

    /// Convert to string representation for display/comparison
    pub fn to_string_value(&self) -> String {
        match &self.0 {
            JsonValue::Null => String::new(),
            JsonValue::Bool(b) => b.to_string(),
            JsonValue::Number(n) => n.to_string(),
            JsonValue::String(s) => s.clone(),
            JsonValue::Array(arr) => {
                let items: Vec<String> = arr.iter()
                    .map(|v| Value::new(v.clone()).to_string_value())
                    .collect();
                items.join(", ")
            }
            JsonValue::Object(_) => "[object]".to_string(),
        }
    }

    /// Compare two values for ordering (used by Sort-Object)
    pub fn compare(&self, other: &Value, case_sensitive: bool) -> Ordering {
        match (&self.0, &other.0) {
            (JsonValue::Number(a), JsonValue::Number(b)) => {
                let a_f = a.as_f64().unwrap_or(0.0);
                let b_f = b.as_f64().unwrap_or(0.0);
                a_f.partial_cmp(&b_f).unwrap_or(Ordering::Equal)
            }
            (JsonValue::String(a), JsonValue::String(b)) => {
                if case_sensitive {
                    a.cmp(b)
                } else {
                    a.to_lowercase().cmp(&b.to_lowercase())
                }
            }
            (JsonValue::Bool(a), JsonValue::Bool(b)) => a.cmp(b),
            (JsonValue::Null, JsonValue::Null) => Ordering::Equal,
            // Null is less than everything
            (JsonValue::Null, _) => Ordering::Less,
            (_, JsonValue::Null) => Ordering::Greater,
            // Mixed types: compare string representations
            _ => {
                let a_str = self.to_string_value();
                let b_str = other.to_string_value();
                if case_sensitive {
                    a_str.cmp(&b_str)
                } else {
                    a_str.to_lowercase().cmp(&b_str.to_lowercase())
                }
            }
        }
    }
}

impl From<JsonValue> for Value {
    fn from(v: JsonValue) -> Self {
        Value::new(v)
    }
}

impl From<Value> for JsonValue {
    fn from(v: Value) -> Self {
        v.0
    }
}

/// Simple wildcard matching (supports * and ?)
fn wildcard_match(text: &str, pattern: &str) -> bool {
    let text_chars: Vec<char> = text.chars().collect();
    let pattern_chars: Vec<char> = pattern.chars().collect();

    wildcard_match_helper(&text_chars, &pattern_chars)
}

fn wildcard_match_helper(text: &[char], pattern: &[char]) -> bool {
    if pattern.is_empty() {
        return text.is_empty();
    }

    if pattern[0] == '*' {
        // Try matching zero or more characters
        for i in 0..=text.len() {
            if wildcard_match_helper(&text[i..], &pattern[1..]) {
                return true;
            }
        }
        false
    } else if text.is_empty() {
        false
    } else if pattern[0] == '?' || pattern[0] == text[0] {
        wildcard_match_helper(&text[1..], &pattern[1..])
    } else {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_case_insensitive_property_access() {
        let v = Value::new(json!({"Name": "Alice", "AGE": 30}));
        assert_eq!(v.get_property("name").unwrap().as_str(), Some("Alice"));
        assert_eq!(v.get_property("NAME").unwrap().as_str(), Some("Alice"));
        assert_eq!(v.get_property("age").unwrap().as_i64(), Some(30));
    }

    #[test]
    fn test_wildcard_matching() {
        assert!(wildcard_match("username", "user*"));
        assert!(wildcard_match("username", "*name"));
        assert!(wildcard_match("username", "u*e"));
        assert!(wildcard_match("username", "????????"));
        assert!(!wildcard_match("username", "admin*"));
    }

    #[test]
    fn test_wildcard_property_access() {
        let v = Value::new(json!({"UserName": "alice", "UserID": 123, "Email": "test@test.com"}));
        let matches = v.get_properties_wildcard("user*");
        assert_eq!(matches.len(), 2);
    }
}
