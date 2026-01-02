/// Runtime Node Execution Logic
///
/// Handles the state and execution of a single node in the graph.

use crate::ast::{Expression, ProcessBlock, Statement, BinaryOperator, UnaryOperator};
use crate::runtime::value::Value;
use crate::types::DanaType;
use petgraph::graph::NodeIndex;
use std::collections::HashMap;

/// Result of a node execution
#[derive(Debug, Clone)]
pub struct ExecutionResult {
    /// Values emitted to output ports
    pub outputs: Vec<(String, Value)>,
}

/// Runtime representation of a node
#[derive(Debug, Clone)]
pub struct RuntimeNode {
    pub name: String,
    pub index: NodeIndex,
    pub input_ports: HashMap<String, DanaType>,
    pub output_ports: HashMap<String, DanaType>,
    pub properties: HashMap<String, Value>,
    pub process: Option<ProcessBlock>,
}

impl RuntimeNode {
    pub fn new(
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
            properties,
            process,
        }
    }

    /// Execute the node logic triggered by a specific input
    pub fn execute(&mut self, trigger_port: &str, input_value: Value) -> Result<ExecutionResult, String> {
        let mut outputs = Vec::new();
        
        // If we have a process block, execute it
        if let Some(process) = &self.process {
            // Check if this port triggers the process
            if process.triggers.iter().any(|t| t == trigger_port) {
                // Create a local scope with properties + input value
                let mut scope = self.properties.clone();
                scope.insert(trigger_port.to_string(), input_value);
                
                // Execute statements
                for stmt in &process.statements {
                    // println!("DEBUG: Executing {:?} with scope {:?}", stmt, scope.keys());
                    match stmt {
                        Statement::Emit { port, value } => {
                            let val = self.evaluate_expression(&value, &scope)?;
                            println!("Emit {}({})", port, val); // Keeping this one as it's the only output mechanism for now
                            outputs.push((port.clone(), val));
                        }
                        Statement::Let { name, value } => {
                            let val = self.evaluate_expression(&value, &scope)?;
                            // println!("DEBUG: Let {} = {}", name, val);
                            scope.insert(name.clone(), val);
                        }
                        Statement::Expression(expr) => {
                             self.evaluate_expression(expr, &scope)?;
                        }
                    }
                }
                
                // Update properties if they were modified (simplified for now: scope update doesn't persist back to properties unless explicitly set)
                // In a real implementation, we'd distinguish between local vars and properties. 
                // For MVP, we'll assume properties are read-only during execution or explicit set needed.
                // Let's defer strict property mutation logic for now.
            }
        } else {
            // Default behavior (passthrough) if no process? 
            // Or maybe just do nothing.
        }

        Ok(ExecutionResult { outputs })
    }

    fn evaluate_expression(&self, expr: &Expression, scope: &HashMap<String, Value>) -> Result<Value, String> {
        match expr {
            Expression::IntLiteral(i) => Ok(Value::Int(*i)),
            Expression::FloatLiteral(f) => Ok(Value::Float(*f)),
            Expression::StringLiteral(s) => Ok(Value::String(s.clone())),
            Expression::BoolLiteral(b) => Ok(Value::Bool(*b)),
            Expression::Identifier(name) => {
                scope.get(name)
                    .cloned()
                    .or_else(|| self.properties.get(name).cloned())
                    .ok_or_else(|| format!("Variable '{}' not found", name))
            }
            Expression::BinaryOp { op, left, right } => {
                let l_val = self.evaluate_expression(left, scope)?;
                let r_val = self.evaluate_expression(right, scope)?;
                self.evaluate_binary_op(*op, l_val, r_val)
            }
            // Other expressions (Unary, Call, Lambda) deferred for MVP
            _ => Err(format!("Expression type {:?} not implemented yet", expr)),
        }
    }

    fn evaluate_binary_op(&self, op: BinaryOperator, left: Value, right: Value) -> Result<Value, String> {
        match (op, left, right) {
            // Integer operations
            (BinaryOperator::Add, Value::Int(l), Value::Int(r)) => Ok(Value::Int(l + r)),
            (BinaryOperator::Subtract, Value::Int(l), Value::Int(r)) => Ok(Value::Int(l - r)),
            (BinaryOperator::Multiply, Value::Int(l), Value::Int(r)) => Ok(Value::Int(l * r)),
            (BinaryOperator::Divide, Value::Int(l), Value::Int(r)) => {
                 if r == 0 { Err("Division by zero".to_string()) } else { Ok(Value::Int(l / r)) }
            },
            
            // String concatenation
            (BinaryOperator::Add, Value::String(l), Value::String(r)) => Ok(Value::String(l + &r)),
             // Int + String (convenience)
            (BinaryOperator::Add, Value::String(l), Value::Int(r)) => Ok(Value::String(format!("{}{}", l, r))),
            
            _ => Err("Unsupported binary operation or type mismatch".to_string()),
        }
    }
}
