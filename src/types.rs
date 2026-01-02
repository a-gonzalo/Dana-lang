/// Type system for the Dana language
/// 
/// Dana uses a strict, static type system to ensure data flow integrity at compile time.

use serde::{Deserialize, Serialize};
use std::fmt;
use thiserror::Error;

/// Core types in Dana
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum DanaType {
    /// Integer type
    Int,
    /// Floating point type
    Float,
    /// String type
    String,
    /// Boolean type
    Bool,
    /// Byte type
    Byte,
    /// Stream of values of type T
    Stream(Box<DanaType>),
    /// Unit type (for triggers/signals)
    Unit,
}

impl fmt::Display for DanaType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DanaType::Int => write!(f, "Int"),
            DanaType::Float => write!(f, "Float"),
            DanaType::String => write!(f, "String"),
            DanaType::Bool => write!(f, "Bool"),
            DanaType::Byte => write!(f, "Byte"),
            DanaType::Stream(inner) => write!(f, "Stream<{}>", inner),
            DanaType::Unit => write!(f, "Unit"),
        }
    }
}

#[derive(Error, Debug)]
pub enum TypeError {
    #[error("Type mismatch: expected {0}, got {1}")]
    Mismatch(String, String),
    
    #[error("Incompatible types in edge: {0} -> {1}")]
    IncompatibleEdge(String, String),
    
    #[error("Unknown type: {0}")]
    UnknownType(String),
}

/// Type checker for validating type compatibility
pub struct TypeChecker;

impl TypeChecker {
    /// Check if two types are compatible (can be connected via an edge)
    pub fn are_compatible(source: &DanaType, target: &DanaType) -> bool {
        match (source, target) {
            // Exact matches
            (DanaType::Int, DanaType::Int) => true,
            (DanaType::Float, DanaType::Float) => true,
            (DanaType::String, DanaType::String) => true,
            (DanaType::Bool, DanaType::Bool) => true,
            (DanaType::Byte, DanaType::Byte) => true,
            (DanaType::Unit, DanaType::Unit) => true,
            
            // Stream compatibility - inner types must match
            (DanaType::Stream(s), DanaType::Stream(t)) => {
                Self::are_compatible(s, t)
            }
            
            // Int can be promoted to Float
            (DanaType::Int, DanaType::Float) => true,
            
            // Everything else is incompatible
            _ => false,
        }
    }

    /// Validate an edge connection
    pub fn validate_edge(
        source: &DanaType,
        target: &DanaType,
    ) -> Result<(), TypeError> {
        if Self::are_compatible(source, target) {
            Ok(())
        } else {
            Err(TypeError::IncompatibleEdge(
                source.to_string(),
                target.to_string(),
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_exact_type_compatibility() {
        assert!(TypeChecker::are_compatible(&DanaType::Int, &DanaType::Int));
        assert!(TypeChecker::are_compatible(&DanaType::String, &DanaType::String));
        assert!(TypeChecker::are_compatible(&DanaType::Bool, &DanaType::Bool));
    }

    #[test]
    fn test_int_to_float_promotion() {
        assert!(TypeChecker::are_compatible(&DanaType::Int, &DanaType::Float));
        assert!(!TypeChecker::are_compatible(&DanaType::Float, &DanaType::Int));
    }

    #[test]
    fn test_stream_compatibility() {
        let stream_int = DanaType::Stream(Box::new(DanaType::Int));
        let stream_int2 = DanaType::Stream(Box::new(DanaType::Int));
        let stream_string = DanaType::Stream(Box::new(DanaType::String));

        assert!(TypeChecker::are_compatible(&stream_int, &stream_int2));
        assert!(!TypeChecker::are_compatible(&stream_int, &stream_string));
    }

    #[test]
    fn test_incompatible_types() {
        assert!(!TypeChecker::are_compatible(&DanaType::Int, &DanaType::String));
        assert!(!TypeChecker::are_compatible(&DanaType::Bool, &DanaType::Float));
    }

    #[test]
    fn test_type_display() {
        assert_eq!(DanaType::Int.to_string(), "Int");
        assert_eq!(DanaType::String.to_string(), "String");
        assert_eq!(
            DanaType::Stream(Box::new(DanaType::Int)).to_string(),
            "Stream<Int>"
        );
    }
}
