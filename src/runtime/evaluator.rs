//! Expression and pattern evaluator for the runtime.
//!
//! Centralizes evaluation logic used by `node` execution and guard checks.

use crate::runtime::value::Value;
use crate::ast::{Expression, BinaryOperator, Pattern};
use std::collections::HashMap;

/// Evaluate an expression given a scope and properties.
/// Returns a runtime `Value` or a string error.
pub fn evaluate_expression(
    expr: &Expression,
    scope: &HashMap<String, Value>,
    properties: &HashMap<String, Value>,
) -> Result<Value, String> {
    match expr {
        Expression::IntLiteral(i) => Ok(Value::Int(*i)),
        Expression::FloatLiteral(f) => Ok(Value::Float(*f)),
        Expression::StringLiteral(s) => Ok(Value::String(s.clone())),
        Expression::BoolLiteral(b) => Ok(Value::Bool(*b)),
        Expression::Identifier(name) => {
            scope.get(name)
                .cloned()
                .or_else(|| properties.get(name).cloned())
                .ok_or_else(|| format!("Variable '{}' not found in scope or properties", name))
        }
        Expression::BinaryOp { op, left, right } => {
            let l_val = evaluate_expression(left, scope, properties)?;
            let r_val = evaluate_expression(right, scope, properties)?;
            evaluate_binary_op(*op, l_val, r_val)
        }
        // Other expression types not implemented yet
        _ => Err(format!("Expression type {:?} not implemented yet", expr)),
    }
}

pub(crate) fn evaluate_binary_op(op: BinaryOperator, left: Value, right: Value) -> Result<Value, String> {
    match (op, left, right) {
        (BinaryOperator::Add, Value::Int(l), Value::Int(r)) => Ok(Value::Int(l + r)),
        (BinaryOperator::Subtract, Value::Int(l), Value::Int(r)) => Ok(Value::Int(l - r)),
        (BinaryOperator::Multiply, Value::Int(l), Value::Int(r)) => Ok(Value::Int(l * r)),
        (BinaryOperator::Divide, Value::Int(l), Value::Int(r)) => {
             if r == 0 { Err("Division by zero".to_string()) } else { Ok(Value::Int(l / r)) }
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
        _ => Err("Unsupported binary operation or type mismatch".to_string()),
    }
}

/// Match-related helpers

/// Try to match a pattern against a value, populating bindings if successful
pub fn match_pattern(
    pattern: &Pattern,
    value: &Value,
    bindings: &mut HashMap<String, Value>,
) -> Result<bool, String> {
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

fn pattern_expr_to_value(expr: &Expression) -> Result<Value, String> {
    match expr {
        Expression::IntLiteral(i) => Ok(Value::Int(*i)),
        Expression::FloatLiteral(f) => Ok(Value::Float(*f)),
        Expression::StringLiteral(s) => Ok(Value::String(s.clone())),
        Expression::BoolLiteral(b) => Ok(Value::Bool(*b)),
        _ => Err("Only literal values allowed in patterns".to_string()),
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
}
