/// Type system for the Dana language
/// 
/// Dana uses a strict, static type system to ensure data flow integrity at compile time.

use serde::{Deserialize, Serialize};
use std::fmt;
use thiserror::Error;

/// Core types in Dana
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum DanaType {
    Int,
    Float,
    String,
    Bool,
    Byte,
    Stream(Box<DanaType>),
    List(Box<DanaType>),
    Unit, // Also known as 'Impulse'
    Type(String, Vec<(String, DanaType)>),
    /// Any type (universal compatibility for System.IO.stdout)
    Any,
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
            DanaType::List(inner) => write!(f, "List<{}>", inner),
            DanaType::Unit => write!(f, "Unit"),
            DanaType::Type(name, _) => write!(f, "{}", name),
            DanaType::Any => write!(f, "Any"),
        }
    }
}

#[allow(dead_code)]
#[derive(Error, Debug)]
pub enum TypeError {
    #[error("Type mismatch: expected {0}, got {1}")]
    Mismatch(String, String),
    
    #[error("Incompatible types in edge: {0} -> {1}")]
    IncompatibleEdge(String, String),
    
    #[error("Unknown type: {0}")]
    UnknownType(String),
}

pub struct TypeChecker;

impl TypeChecker {
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
            
            // List compatibility - inner types must match
            (DanaType::List(s), DanaType::List(t)) => {
                Self::are_compatible(s, t)
            }
            
            // Struct compatibility (Nominal typing: names must match)
            // (Assuming no name collisions in same namespace)
            (DanaType::Type(n1, _), DanaType::Type(n2, _)) => n1 == n2,
            
            // Int can be promoted to Float
            (DanaType::Int, DanaType::Float) => true,
            
            // Any can accept anything
            (_, DanaType::Any) => true,
            
            // Everything else is incompatible
            _ => false,
        }
    }

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
    fn test_any_compatibility() {
        assert!(TypeChecker::are_compatible(&DanaType::Int, &DanaType::Any));
        assert!(TypeChecker::are_compatible(&DanaType::String, &DanaType::Any));
        assert!(TypeChecker::are_compatible(&DanaType::Bool, &DanaType::Any));
        assert!(TypeChecker::are_compatible(&DanaType::Type("Custom".to_string(), vec![]), &DanaType::Any));
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

    #[test]
    fn test_list_compatibility() {
        let list_int = DanaType::List(Box::new(DanaType::Int));
        let list_int2 = DanaType::List(Box::new(DanaType::Int));
        let list_string = DanaType::List(Box::new(DanaType::String));

        assert!(TypeChecker::are_compatible(&list_int, &list_int2));
        assert!(!TypeChecker::are_compatible(&list_int, &list_string));
    }

    #[test]
    fn test_validate_edge_error() {
        let res = TypeChecker::validate_edge(&DanaType::Int, &DanaType::String);
        assert!(res.is_err());
        if let Err(e) = res {
            match e {
                TypeError::IncompatibleEdge(from, to) => {
                    assert_eq!(from, "Int");
                    assert_eq!(to, "String");
                }
                _ => panic!("expected IncompatibleEdge"),
            }
        }
    }
}
