//! Expression and pattern evaluator for the runtime.
//!
//! Centralizes evaluation logic used by `node` execution and guard checks.

use crate::runtime::value::Value;
use crate::ast::{Expression, BinaryOperator, Pattern};
use std::collections::HashMap;
use thiserror::Error;

/// Strongly-typed evaluator errors
#[derive(Error, Debug, Clone)]
pub enum EvalError {
    #[error("Variable not found: {0}")]
    VariableNotFound(String),

    #[error("Type error: {0}")]
    TypeError(String),

    #[error("Division by zero")]
    DivisionByZero,

    #[error("Unsupported operation or type mismatch: {0}")]
    Unsupported(String),

    #[error("Guard must evaluate to boolean")]
    GuardNotBool,

    #[error("Not implemented: {0}")]
    NotImplemented(String),
}

/// Evaluate an expression given a scope and properties.
/// Returns a runtime `Value` or an `EvalError`.
pub fn evaluate_expression(
    expr: &Expression,
    scope: &HashMap<String, Value>,
    properties: &HashMap<String, Value>,
) -> Result<Value, EvalError> {
    match expr {
        Expression::IntLiteral(i) => Ok(Value::Int(*i)),
        Expression::FloatLiteral(f) => Ok(Value::Float(*f)),
        Expression::StringLiteral(s) => Ok(Value::String(s.clone())),
        Expression::BoolLiteral(b) => Ok(Value::Bool(*b)),
        Expression::Identifier(name) => {
            scope.get(name)
                .cloned()
                .or_else(|| properties.get(name).cloned())
                .ok_or_else(|| EvalError::VariableNotFound(name.clone()))
        }
        Expression::BinaryOp { op, left, right } => {
            let l_val = evaluate_expression(left, scope, properties)?;
            let r_val = evaluate_expression(right, scope, properties)?;
            evaluate_binary_op(*op, l_val, r_val)
        }
        // Other expression types not implemented yet
        _ => Err(EvalError::NotImplemented(format!("{:?}", expr))),
    }
}

pub(crate) fn evaluate_binary_op(op: BinaryOperator, left: Value, right: Value) -> Result<Value, EvalError> {
    // keep clones for error reporting (values moved into match arms)
    let left_clone = left.clone();
    let right_clone = right.clone();

    match (op, left, right) {
        (BinaryOperator::Add, Value::Int(l), Value::Int(r)) => Ok(Value::Int(l + r)),
        (BinaryOperator::Subtract, Value::Int(l), Value::Int(r)) => Ok(Value::Int(l - r)),
        (BinaryOperator::Multiply, Value::Int(l), Value::Int(r)) => Ok(Value::Int(l * r)),
        (BinaryOperator::Divide, Value::Int(l), Value::Int(r)) => {
             if r == 0 { Err(EvalError::DivisionByZero) } else { Ok(Value::Int(l / r)) }
        },
        (BinaryOperator::Add, Value::String(l), Value::String(r)) => Ok(Value::String(l + &r)),
        (BinaryOperator::Add, Value::String(l), Value::Int(r)) => Ok(Value::String(format!("{}{}", l, r))),
        // Comparisons
        (BinaryOperator::GreaterThan, Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l > r)),
        (BinaryOperator::LessThan, Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l < r)),
        (BinaryOperator::GreaterEqual, Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l >= r)),
        (BinaryOperator::LessEqual, Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l <= r)),
        (BinaryOperator::Equal, Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l == r)),
        (BinaryOperator::NotEqual, Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l != r)),
        // String comparisons
        (BinaryOperator::Equal, Value::String(l), Value::String(r)) => Ok(Value::Bool(l == r)),
        (BinaryOperator::NotEqual, Value::String(l), Value::String(r)) => Ok(Value::Bool(l != r)),
        // Bool comparisons
        (BinaryOperator::Equal, Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l == r)),
        (BinaryOperator::NotEqual, Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l != r)),
        _ => Err(EvalError::Unsupported(format!("op={:?} left={:?} right={:?}", op, left_clone, right_clone))),
    }
}

/// Match-related helpers

/// Try to match a pattern against a value, populating bindings if successful
pub fn match_pattern(
    pattern: &Pattern,
    value: &Value,
    bindings: &mut HashMap<String, Value>,
) -> Result<bool, EvalError> {
    // Reuse the same logic as before in node.rs
    let result = match pattern {
        Pattern::Wildcard => Ok(true),
        Pattern::Literal(expr) => {
            let pattern_value = pattern_expr_to_value(expr)?;
            Ok(values_equal(&pattern_value, value))
        }
        Pattern::Binding(name) => {
            bindings.insert(name.clone(), value.clone());
            Ok(true)
        }
        Pattern::Tuple(patterns) => {
            if let Value::Tuple(values) = value {
                if patterns.len() != values.len() {
                    return Ok(false);
                }
                for (p, v) in patterns.iter().zip(values.iter()) {
                    if !match_pattern(p, v, bindings)? {
                        return Ok(false);
                    }
                }
                Ok(true)
            } else {
                Ok(false)
            }
        }
    };

    result
}

fn pattern_expr_to_value(expr: &Expression) -> Result<Value, EvalError> {
    match expr {
        Expression::IntLiteral(i) => Ok(Value::Int(*i)),
        Expression::FloatLiteral(f) => Ok(Value::Float(*f)),
        Expression::StringLiteral(s) => Ok(Value::String(s.clone())),
        Expression::BoolLiteral(b) => Ok(Value::Bool(*b)),
        _ => Err(EvalError::NotImplemented("pattern literal type".to_string())),
    }
}

pub(crate) fn values_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => x == y,
        (Value::Float(x), Value::Float(y)) => (x - y).abs() < f64::EPSILON,
        (Value::String(x), Value::String(y)) => x == y,
        (Value::Bool(x), Value::Bool(y)) => x == y,
        (Value::Unit, Value::Unit) => true,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_binary_add_int() {
        let res = evaluate_binary_op(crate::ast::BinaryOperator::Add, Value::Int(2), Value::Int(3)).unwrap();
        assert_eq!(res, Value::Int(5));
    }

    #[test]
    fn test_match_literal() {
        use crate::ast::Expression;
        let pattern = Pattern::Literal(Expression::IntLiteral(3));
        let mut bindings = std::collections::HashMap::new();
        assert!(match_pattern(&pattern, &Value::Int(3), &mut bindings).unwrap());
    }

    #[test]
    fn test_binary_add_type_mismatch() {
        use crate::ast::BinaryOperator;
        let res = evaluate_binary_op(BinaryOperator::Add, Value::Int(1), Value::String("x".to_string()));
        assert!(matches!(res, Err(EvalError::Unsupported(_))));
    }

    #[test]
    fn test_divide_by_zero() {
        use crate::ast::BinaryOperator;
        let res = evaluate_binary_op(BinaryOperator::Divide, Value::Int(4), Value::Int(0));
        assert!(matches!(res, Err(EvalError::DivisionByZero)));
    }

    #[test]
    fn test_variable_not_found() {
        use crate::ast::Expression;
        let scope = std::collections::HashMap::new();
        let props = std::collections::HashMap::new();
        let expr = Expression::Identifier("x".to_string());
        let res = evaluate_expression(&expr, &scope, &props);
        assert!(matches!(res, Err(EvalError::VariableNotFound(name)) if name == "x"));
    }

    #[test]
    fn test_string_plus_int_concat() {
        use crate::ast::BinaryOperator;
        let res = evaluate_binary_op(BinaryOperator::Add, Value::String("x".to_string()), Value::Int(3)).unwrap();
        assert_eq!(res, Value::String("x3".to_string()));
    }

    #[test]
    fn test_pattern_binding_and_tuple_mismatch() {
        use crate::ast::Pattern;
        let mut bindings = std::collections::HashMap::new();
        let pat = Pattern::Binding("a".to_string());
        assert!(match_pattern(&pat, &Value::Int(7), &mut bindings).unwrap());
        assert_eq!(bindings.get("a").unwrap(), &Value::Int(7));

        let pat_tuple = Pattern::Tuple(vec![Pattern::Literal(crate::ast::Expression::IntLiteral(1)), Pattern::Wildcard]);
        let mut binds2 = std::collections::HashMap::new();
        assert!(!match_pattern(&pat_tuple, &Value::Tuple(vec![Value::Int(1)]), &mut binds2).unwrap()); // length mismatch
    }
}
