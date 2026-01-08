/// Runtime Node Execution Logic
///
/// Handles the state and execution of a single node in the graph.

use crate::ast::{Expression, ProcessBlock, Statement, BinaryOperator, Pattern, MatchArm};
use crate::runtime::value::Value;
use crate::runtime::native::{NativeNode, NativeContext};
use crate::runtime::pulse::TraceId;
use crate::runtime::scheduler::TraceStateStore;
use crate::types::DanaType;
use crate::verbose;
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
    pub fn execute(&self, trigger_port: &str, input_value: Value, trace_id: TraceId, state_store: &TraceStateStore) -> Result<ExecutionResult, String> {
        // ALWAYS SAVE the incoming value to the trace state
        let mut full_state = state_store.get_node_state(trace_id, self.index).unwrap_or_default();
        full_state.insert(trigger_port.to_string(), input_value.clone());
        state_store.set_node_state(trace_id, self.index, full_state.clone());

        match &self.kind {
            NodeKind::DanaProcess { properties, process } => {
                let mut outputs = Vec::new();
                
                // If we have a process block, execute it
                if let Some(process) = process {
                    // Implicit Join: Verify all required inputs are present in properties or state
                    let mut missing_inputs = Vec::new();
                    for req_port in &process.triggers {
                        if !full_state.contains_key(req_port) && !properties.contains_key(req_port) {
                            // Missing a required input, skip execution (it's persistent now, so we'll try again next pulse)
                            missing_inputs.push(req_port.clone());
                        }
                    }
                    
                    if !missing_inputs.is_empty() {
                        verbose!("[JOIN {}] Node '{}' waiting for inputs: {:?} (have: {:?})", 
                            trace_id, self.name, missing_inputs, full_state.keys().collect::<Vec<_>>());
                        return Ok(ExecutionResult { outputs: Vec::new() });
                    }

                    verbose!("[JOIN {}] Node '{}' has all inputs, executing", trace_id, self.name);

                    // Create a local scope with properties + trace_state
                    let mut scope = properties.clone();
                    for (k, v) in &full_state {
                        scope.insert(k.clone(), v.clone());
                    }
                    
                    // Execute statements
                    for stmt in &process.statements {
                        Self::execute_statement(stmt, &mut scope, properties, &mut outputs)?;
                    }

                        // CONSUME inputs to prevent double-firing (Implicit synchronization)
                        for req_port in &process.triggers {
                            full_state.remove(req_port);
                        }
                        state_store.set_node_state(trace_id, self.index, full_state);
                    } else {
                    // STATIC/RELAY NODE LOGIC
                    // 1. Auto-emit properties (Constants)
                    for (name, value) in properties {
                        if self.output_ports.contains_key(name) {
                            outputs.push((name.clone(), value.clone()));
                        }
                    }
                    // 2. Relay inputs to matching outputs (Virtual nodes / Pass-through)
                    if self.output_ports.contains_key(trigger_port) {
                        outputs.push((trigger_port.to_string(), input_value));
                    }
                }
                
                Ok(ExecutionResult { outputs })
            },
            NodeKind::Native(native_node) => {
                let ctx = NativeContext {
                    trace_id,
                    node_idx: self.index,
                    state_store,
                };
                let outputs = native_node.on_input(trigger_port, input_value, &ctx)?;
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
            
            // String comparisons
            (BinaryOperator::Equal, Value::String(l), Value::String(r)) => Ok(Value::Bool(l == r)),
            (BinaryOperator::NotEqual, Value::String(l), Value::String(r)) => Ok(Value::Bool(l != r)),
            
            // Bool comparisons
            (BinaryOperator::Equal, Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l == r)),
            (BinaryOperator::NotEqual, Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l != r)),
            
            _ => Err("Unsupported binary operation or type mismatch".to_string()),
        }
    }
    
    /// Execute a single statement, potentially modifying scope and outputs
    fn execute_statement(
        stmt: &Statement,
        scope: &mut HashMap<String, Value>,
        properties: &HashMap<String, Value>,
        outputs: &mut Vec<(String, Value)>,
    ) -> Result<(), String> {
        match stmt {
            Statement::Emit { port, value } => {
                let val = Self::evaluate_expression(value, scope, properties)?;
                outputs.push((port.clone(), val));
            }
            Statement::Let { name, value } => {
                let val = Self::evaluate_expression(value, scope, properties)?;
                scope.insert(name.clone(), val);
            }
            Statement::Match { expression, arms } => {
                let match_value = Self::evaluate_expression(expression, scope, properties)?;
                Self::execute_match(&match_value, arms, scope, properties, outputs)?;
            }
            Statement::Expression(expr) => {
                Self::evaluate_expression(expr, scope, properties)?;
            }
        }
        Ok(())
    }
    
    /// Execute a match statement by finding the first matching arm
    fn execute_match(
        value: &Value,
        arms: &[MatchArm],
        scope: &mut HashMap<String, Value>,
        properties: &HashMap<String, Value>,
        outputs: &mut Vec<(String, Value)>,
    ) -> Result<(), String> {
        verbose!("[MATCH] Matching value {:?} against {} arms", value, arms.len());
        for (i, arm) in arms.iter().enumerate() {
            // Create a temporary scope for pattern bindings
            let mut arm_scope = scope.clone();
            
            // Try to match the pattern
            verbose!("[MATCH] Trying arm {} with pattern {:?}", i, arm.pattern);
            if Self::match_pattern(&arm.pattern, value, &mut arm_scope)? {
                verbose!("[MATCH] Pattern matched!");
                // Check guard if present
                if let Some(guard_expr) = &arm.guard {
                    let guard_result = Self::evaluate_expression(guard_expr, &arm_scope, properties)?;
                    match guard_result {
                        Value::Bool(true) => {}
                        Value::Bool(false) => continue, // Guard failed, try next arm
                        _ => return Err("Guard must evaluate to boolean".to_string()),
                    }
                }
                
                // Execute the arm's body statements
                verbose!("[MATCH] Executing {} statements", arm.body.len());
                for stmt in &arm.body {
                    Self::execute_statement(stmt, &mut arm_scope, properties, outputs)?;
                }
                
                // First matching arm wins - stop here
                return Ok(());
            }
        }
        
        verbose!("[MATCH] No arm matched");
        // No arm matched - this is not an error, just no emission
        Ok(())
    }
    
    /// Try to match a pattern against a value, populating bindings if successful
    fn match_pattern(
        pattern: &Pattern,
        value: &Value,
        bindings: &mut HashMap<String, Value>,
    ) -> Result<bool, String> {
        verbose!("[MATCH_PAT] Matching pattern {:?} against value {:?}", pattern, value);
        let result = match pattern {
            Pattern::Wildcard => Ok(true),
            
            Pattern::Literal(expr) => {
                // For literals, we need to compare the value
                // The expression should be a literal (Int, String, Bool, etc.)
                let pattern_value = Self::pattern_expr_to_value(expr)?;
                verbose!("[MATCH_PAT] Literal: comparing {:?} with {:?}", pattern_value, value);
                Ok(Self::values_equal(&pattern_value, value))
            }
            
            Pattern::Binding(name) => {
                // Binding always matches and captures the value
                bindings.insert(name.clone(), value.clone());
                Ok(true)
            }
            
            Pattern::Tuple(patterns) => {
                // For tuples, we need the value to be a tuple with matching length
                if let Value::Tuple(values) = value {
                    if patterns.len() != values.len() {
                        return Ok(false);
                    }
                    for (p, v) in patterns.iter().zip(values.iter()) {
                        if !Self::match_pattern(p, v, bindings)? {
                            return Ok(false);
                        }
                    }
                    Ok(true)
                } else {
                    Ok(false)
                }
            }
        };
        verbose!("[MATCH_PAT] Result: {:?}", result);
        result
    }
    
    /// Convert a pattern expression to a Value (only literals allowed in patterns)
    fn pattern_expr_to_value(expr: &Expression) -> Result<Value, String> {
        match expr {
            Expression::IntLiteral(i) => Ok(Value::Int(*i)),
            Expression::FloatLiteral(f) => Ok(Value::Float(*f)),
            Expression::StringLiteral(s) => Ok(Value::String(s.clone())),
            Expression::BoolLiteral(b) => Ok(Value::Bool(*b)),
            _ => Err("Only literal values allowed in patterns".to_string()),
        }
    }
    
    /// Check if two values are equal (for pattern matching)
    fn values_equal(a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::Int(x), Value::Int(y)) => x == y,
            (Value::Float(x), Value::Float(y)) => (x - y).abs() < f64::EPSILON,
            (Value::String(x), Value::String(y)) => x == y,
            (Value::Bool(x), Value::Bool(y)) => x == y,
            (Value::Unit, Value::Unit) => true,
            _ => false,
        }
    }
}
