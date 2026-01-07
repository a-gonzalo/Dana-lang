/// Runtime Node Execution Logic
///
/// Handles the state and execution of a single node in the graph.

use crate::ast::{Expression, ProcessBlock, Statement, BinaryOperator};
use crate::runtime::value::Value;
use crate::runtime::native::NativeNode;
use crate::types::DanaType;
use petgraph::graph::NodeIndex;
use std::collections::HashMap;

/// Result of a node execution
#[derive(Debug, Clone)]
pub struct ExecutionResult {
    /// Values emitted to output ports
    pub outputs: Vec<(String, Value)>,
}

#[derive(Debug)]
pub enum NodeKind {
    DanaProcess {
        properties: HashMap<String, Value>,
        process: Option<ProcessBlock>,
    },
    Native(Box<dyn NativeNode>),
}

/// Runtime representation of a node
#[derive(Debug)]
pub struct RuntimeNode {
    pub name: String,
    pub index: NodeIndex,
    pub input_ports: HashMap<String, DanaType>,
    pub output_ports: HashMap<String, DanaType>,
    pub kind: NodeKind,
}

impl RuntimeNode {
    pub fn new_dana(
        name: String,
        index: NodeIndex,
        input_ports: HashMap<String, DanaType>,
        output_ports: HashMap<String, DanaType>,
        properties: HashMap<String, Value>,
        process: Option<ProcessBlock>,
    ) -> Self {
        Self {
            name,
            index,
            input_ports,
            output_ports,
            kind: NodeKind::DanaProcess { properties, process },
        }
    }

    pub fn new_native(
        name: String,
        index: NodeIndex,
        input_ports: HashMap<String, DanaType>,
        output_ports: HashMap<String, DanaType>,
        native_impl: Box<dyn NativeNode>,
    ) -> Self {
        Self {
            name,
            index,
            input_ports,
            output_ports,
            kind: NodeKind::Native(native_impl),
        }
    }

    /// Execute the node logic triggered by a specific input
    pub fn execute(&self, trigger_port: &str, input_value: Value, trace_state: &HashMap<String, Value>) -> Result<ExecutionResult, String> {
        match &self.kind {
            NodeKind::DanaProcess { properties, process } => {
                let mut outputs = Vec::new();
                
                // If we have a process block, execute it
                if let Some(process) = process {
                    // Check if this port triggers the process
                    if process.triggers.iter().any(|t| t == trigger_port) {
                        // Create a local scope with (properties OR trace_state) + input value
                        // In EPOS, the base properties are the default, but trace_state can override them?
                        // Actually, properties are static config, trace_state is ephemeral data.
                        let mut scope = properties.clone();
                        // Overlay trace-specific state
                        for (k, v) in trace_state {
                            scope.insert(k.clone(), v.clone());
                        }
                        scope.insert(trigger_port.to_string(), input_value);
                        
                        // Execute statements
                        for stmt in &process.statements {
                            match stmt {
                                Statement::Emit { port, value } => {
                                    let val = Self::evaluate_expression(&value, &scope, properties)?;
                                    outputs.push((port.clone(), val));
                                }
                                Statement::Let { name, value } => {
                                    let val = Self::evaluate_expression(&value, &scope, properties)?;
                                    scope.insert(name.clone(), val);
                                }
                                Statement::Expression(expr) => {
                                     Self::evaluate_expression(expr, &scope, properties)?;
                                }
                            }
                        }
                    }
                } else {
                    // STATIC NODE LOGIC: Auto-emit properties to matching outputs
                    for (name, value) in properties {
                        if self.output_ports.contains_key(name) {
                            outputs.push((name.clone(), value.clone()));
                        }
                    }
                }
                
                Ok(ExecutionResult { outputs })
            },
            NodeKind::Native(native_node) => {
                let outputs = native_node.on_input(trigger_port, input_value)?;
                Ok(ExecutionResult { outputs })
            }
        }
    }

    pub(crate) fn evaluate_expression(expr: &Expression, scope: &HashMap<String, Value>, properties: &HashMap<String, Value>) -> Result<Value, String> {
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
                let l_val = Self::evaluate_expression(left, scope, properties)?;
                let r_val = Self::evaluate_expression(right, scope, properties)?;
                Self::evaluate_binary_op(*op, l_val, r_val)
            }
            _ => Err(format!("Expression type {:?} not implemented yet", expr)),
        }
    }

    fn evaluate_binary_op(op: BinaryOperator, left: Value, right: Value) -> Result<Value, String> {
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
            
            _ => Err("Unsupported binary operation or type mismatch".to_string()),
        }
    }
}
