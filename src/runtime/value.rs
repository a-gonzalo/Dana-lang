/// Runtime Values for Dana
///
/// Represents data flowing through the graph during execution.

use crate::types::{DanaType, TypeError};
use serde::{Deserialize, Serialize};
use std::fmt;

/// A value at runtime
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Value {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Byte(u8),
    List(Vec<Value>),
    // Streams are handled by the runtime channel mechanism, but we might need a representation here
    Unit,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(v) => write!(f, "{}", v),
            Value::Float(v) => write!(f, "{}", v),
            Value::String(v) => write!(f, "{}", v),
            Value::Bool(v) => write!(f, "{}", v),
            Value::Byte(v) => write!(f, "{:#04x}", v),
            Value::List(v) => {
                write!(f, "[")?;
                for (i, item) in v.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", item)?;
                }
                write!(f, "]")
            }
            Value::Unit => write!(f, "()"),
        }
    }
}

impl Value {
    /// Get the DanaType of this value
    pub fn get_type(&self) -> DanaType {
        match self {
            Value::Int(_) => DanaType::Int,
            Value::Float(_) => DanaType::Float,
            Value::String(_) => DanaType::String,
            Value::Bool(_) => DanaType::Bool,
            Value::Byte(_) => DanaType::Byte,
            Value::List(v) => {
                let inner_type = if let Some(first) = v.first() {
                    first.get_type()
                } else {
                    DanaType::Any // Empty list is List<Any>
                };
                DanaType::List(Box::new(inner_type))
            }
            Value::Unit => DanaType::Unit,
        }
    }

    /// Try to convert a value to match a target type (e.g. Int -> Float)
    pub fn convert_to(&self, target_type: &DanaType) -> Result<Value, TypeError> {
        if &self.get_type() == target_type {
            return Ok(self.clone());
        }

        match (self, target_type) {
            (Value::Int(i), DanaType::Float) => Ok(Value::Float(*i as f64)),
            _ => Err(TypeError::Mismatch(
                target_type.to_string(),
                self.get_type().to_string(),
            )),
        }
    }
    
    /// Create a value from a default literal in AST (simplification)
    pub fn from_literal(lit: &crate::ast::Expression) -> Option<Value> {
        match lit {
            crate::ast::Expression::IntLiteral(i) => Some(Value::Int(*i)),
            crate::ast::Expression::FloatLiteral(f) => Some(Value::Float(*f)),
            crate::ast::Expression::StringLiteral(s) => Some(Value::String(s.clone())),
            crate::ast::Expression::BoolLiteral(b) => Some(Value::Bool(*b)),
            _ => None, // Complex expressions not supported as simple literals yet
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_value_display() {
        assert_eq!(Value::Int(42).to_string(), "42");
        assert_eq!(Value::String("hello".to_string()).to_string(), "hello");
        assert_eq!(Value::Unit.to_string(), "()");
    }

    #[test]
    fn test_get_type() {
        assert_eq!(Value::Int(10).get_type(), DanaType::Int);
        assert_eq!(Value::Bool(true).get_type(), DanaType::Bool);
    }

    #[test]
    fn test_conversion() {
        let v = Value::Int(42);
        
        // Identity conversion
        assert_eq!(v.convert_to(&DanaType::Int).unwrap(), Value::Int(42));
        
        // Promotion
        let elevated = v.convert_to(&DanaType::Float).unwrap();
        match elevated {
            Value::Float(f) => assert!((f - 42.0).abs() < f64::EPSILON),
            _ => panic!("Expected Float"),
        }
        
        // Invalid conversion
        assert!(v.convert_to(&DanaType::String).is_err());
    }
}
