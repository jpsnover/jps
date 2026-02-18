use crate::value::Value;
use regex::Regex;
use std::cmp::Ordering;

/// Comparison operators for Where-Object
#[derive(Debug, Clone, PartialEq)]
pub enum ComparisonOp {
    // Equality
    Eq,   // -eq (case-insensitive)
    CEq,  // -ceq (case-sensitive)
    Ne,   // -ne
    CNe,  // -cne

    // Numeric/string comparison
    Gt,   // -gt
    CGt,  // -cgt
    Ge,   // -ge
    CGe,  // -cge
    Lt,   // -lt
    CLt,  // -clt
    Le,   // -le
    CLe,  // -cle

    // Pattern matching
    Like,      // -like (wildcard)
    CLike,     // -clike
    NotLike,   // -notlike
    CNotLike,  // -cnotlike
    Match,     // -match (regex)
    CMatch,    // -cmatch
    NotMatch,  // -notmatch
    CNotMatch, // -cnotmatch

    // Containment
    Contains,     // -contains (collection contains value)
    CContains,    // -ccontains
    NotContains,  // -notcontains
    CNotContains, // -cnotcontains
    In,           // -in (value in collection)
    CIn,          // -cin
    NotIn,        // -notin
    CNotIn,       // -cnotin
}

impl ComparisonOp {
    /// Parse an operator string (case-insensitive)
    pub fn parse(s: &str) -> Option<Self> {
        let s = s.to_lowercase();
        let s = s.strip_prefix('-').unwrap_or(&s);

        match s {
            "eq" => Some(ComparisonOp::Eq),
            "ceq" => Some(ComparisonOp::CEq),
            "ne" => Some(ComparisonOp::Ne),
            "cne" => Some(ComparisonOp::CNe),
            "gt" => Some(ComparisonOp::Gt),
            "cgt" => Some(ComparisonOp::CGt),
            "ge" => Some(ComparisonOp::Ge),
            "cge" => Some(ComparisonOp::CGe),
            "lt" => Some(ComparisonOp::Lt),
            "clt" => Some(ComparisonOp::CLt),
            "le" => Some(ComparisonOp::Le),
            "cle" => Some(ComparisonOp::CLe),
            "like" => Some(ComparisonOp::Like),
            "clike" => Some(ComparisonOp::CLike),
            "notlike" => Some(ComparisonOp::NotLike),
            "cnotlike" => Some(ComparisonOp::CNotLike),
            "match" => Some(ComparisonOp::Match),
            "cmatch" => Some(ComparisonOp::CMatch),
            "notmatch" => Some(ComparisonOp::NotMatch),
            "cnotmatch" => Some(ComparisonOp::CNotMatch),
            "contains" => Some(ComparisonOp::Contains),
            "ccontains" => Some(ComparisonOp::CContains),
            "notcontains" => Some(ComparisonOp::NotContains),
            "cnotcontains" => Some(ComparisonOp::CNotContains),
            "in" => Some(ComparisonOp::In),
            "cin" => Some(ComparisonOp::CIn),
            "notin" => Some(ComparisonOp::NotIn),
            "cnotin" => Some(ComparisonOp::CNotIn),
            _ => None,
        }
    }

    /// Check if this operator is case-sensitive
    pub fn is_case_sensitive(&self) -> bool {
        matches!(
            self,
            ComparisonOp::CEq
                | ComparisonOp::CNe
                | ComparisonOp::CGt
                | ComparisonOp::CGe
                | ComparisonOp::CLt
                | ComparisonOp::CLe
                | ComparisonOp::CLike
                | ComparisonOp::CNotLike
                | ComparisonOp::CMatch
                | ComparisonOp::CNotMatch
                | ComparisonOp::CContains
                | ComparisonOp::CNotContains
                | ComparisonOp::CIn
                | ComparisonOp::CNotIn
        )
    }

    /// Evaluate the comparison
    pub fn evaluate(&self, left: &Value, right: &Value) -> bool {
        let case_sensitive = self.is_case_sensitive();

        match self {
            ComparisonOp::Eq | ComparisonOp::CEq => values_equal(left, right, case_sensitive),
            ComparisonOp::Ne | ComparisonOp::CNe => !values_equal(left, right, case_sensitive),

            ComparisonOp::Gt | ComparisonOp::CGt => {
                left.compare(right, case_sensitive) == Ordering::Greater
            }
            ComparisonOp::Ge | ComparisonOp::CGe => {
                matches!(
                    left.compare(right, case_sensitive),
                    Ordering::Greater | Ordering::Equal
                )
            }
            ComparisonOp::Lt | ComparisonOp::CLt => {
                left.compare(right, case_sensitive) == Ordering::Less
            }
            ComparisonOp::Le | ComparisonOp::CLe => {
                matches!(
                    left.compare(right, case_sensitive),
                    Ordering::Less | Ordering::Equal
                )
            }

            ComparisonOp::Like | ComparisonOp::CLike => {
                wildcard_match(&left.to_string_value(), &right.to_string_value(), case_sensitive)
            }
            ComparisonOp::NotLike | ComparisonOp::CNotLike => {
                !wildcard_match(&left.to_string_value(), &right.to_string_value(), case_sensitive)
            }

            ComparisonOp::Match | ComparisonOp::CMatch => {
                regex_match(&left.to_string_value(), &right.to_string_value(), case_sensitive)
            }
            ComparisonOp::NotMatch | ComparisonOp::CNotMatch => {
                !regex_match(&left.to_string_value(), &right.to_string_value(), case_sensitive)
            }

            ComparisonOp::Contains | ComparisonOp::CContains => {
                collection_contains(left, right, case_sensitive)
            }
            ComparisonOp::NotContains | ComparisonOp::CNotContains => {
                !collection_contains(left, right, case_sensitive)
            }

            ComparisonOp::In | ComparisonOp::CIn => collection_contains(right, left, case_sensitive),
            ComparisonOp::NotIn | ComparisonOp::CNotIn => {
                !collection_contains(right, left, case_sensitive)
            }
        }
    }
}

/// Check if two values are equal
fn values_equal(left: &Value, right: &Value, case_sensitive: bool) -> bool {
    use serde_json::Value as JV;

    match (left.inner(), right.inner()) {
        (JV::Null, JV::Null) => true,
        (JV::Bool(a), JV::Bool(b)) => a == b,
        (JV::Number(a), JV::Number(b)) => {
            a.as_f64().unwrap_or(0.0) == b.as_f64().unwrap_or(0.0)
        }
        (JV::String(a), JV::String(b)) => {
            if case_sensitive {
                a == b
            } else {
                a.to_lowercase() == b.to_lowercase()
            }
        }
        // Compare number to string
        (JV::Number(n), JV::String(s)) | (JV::String(s), JV::Number(n)) => {
            if let Ok(parsed) = s.parse::<f64>() {
                n.as_f64().unwrap_or(0.0) == parsed
            } else {
                false
            }
        }
        _ => false,
    }
}

/// Wildcard pattern matching (supports * and ?)
fn wildcard_match(text: &str, pattern: &str, case_sensitive: bool) -> bool {
    let text = if case_sensitive {
        text.to_string()
    } else {
        text.to_lowercase()
    };
    let pattern = if case_sensitive {
        pattern.to_string()
    } else {
        pattern.to_lowercase()
    };

    let text_chars: Vec<char> = text.chars().collect();
    let pattern_chars: Vec<char> = pattern.chars().collect();

    wildcard_match_helper(&text_chars, &pattern_chars)
}

fn wildcard_match_helper(text: &[char], pattern: &[char]) -> bool {
    if pattern.is_empty() {
        return text.is_empty();
    }

    if pattern[0] == '*' {
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

/// Regex pattern matching
fn regex_match(text: &str, pattern: &str, case_sensitive: bool) -> bool {
    let pattern = if case_sensitive {
        pattern.to_string()
    } else {
        format!("(?i){}", pattern)
    };

    match Regex::new(&pattern) {
        Ok(re) => re.is_match(text),
        Err(_) => false,
    }
}

/// Check if collection contains value
fn collection_contains(collection: &Value, value: &Value, case_sensitive: bool) -> bool {
    match collection.inner() {
        serde_json::Value::Array(arr) => {
            for item in arr {
                if values_equal(&Value::new(item.clone()), value, case_sensitive) {
                    return true;
                }
            }
            false
        }
        // For non-arrays, check string containment
        _ => {
            let collection_str = collection.to_string_value();
            let value_str = value.to_string_value();
            if case_sensitive {
                collection_str.contains(&value_str)
            } else {
                collection_str.to_lowercase().contains(&value_str.to_lowercase())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_parse_operators() {
        assert_eq!(ComparisonOp::parse("-eq"), Some(ComparisonOp::Eq));
        assert_eq!(ComparisonOp::parse("-EQ"), Some(ComparisonOp::Eq));
        assert_eq!(ComparisonOp::parse("gt"), Some(ComparisonOp::Gt));
        assert_eq!(ComparisonOp::parse("-LIKE"), Some(ComparisonOp::Like));
    }

    #[test]
    fn test_equality() {
        let a = Value::new(json!("hello"));
        let b = Value::new(json!("HELLO"));
        let c = Value::new(json!("hello"));

        assert!(ComparisonOp::Eq.evaluate(&a, &b)); // case-insensitive
        assert!(!ComparisonOp::CEq.evaluate(&a, &b)); // case-sensitive
        assert!(ComparisonOp::CEq.evaluate(&a, &c));
    }

    #[test]
    fn test_numeric_comparison() {
        let a = Value::new(json!(10));
        let b = Value::new(json!(5));

        assert!(ComparisonOp::Gt.evaluate(&a, &b));
        assert!(ComparisonOp::Ge.evaluate(&a, &b));
        assert!(!ComparisonOp::Lt.evaluate(&a, &b));
    }

    #[test]
    fn test_wildcard() {
        let text = Value::new(json!("hello world"));
        let pattern = Value::new(json!("hello*"));

        assert!(ComparisonOp::Like.evaluate(&text, &pattern));
    }

    #[test]
    fn test_regex() {
        let text = Value::new(json!("hello123"));
        let pattern = Value::new(json!(r"\d+"));

        assert!(ComparisonOp::Match.evaluate(&text, &pattern));
    }
}
