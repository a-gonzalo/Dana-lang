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
    pub fn execute(&self, trigger_port: &str, input_value: Value, trace_id: TraceId, state_store: &TraceStateStore) -> Result<ExecutionResult, crate::runtime::error::RuntimeError> {
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
                        if let Err(e) = Self::execute_statement(stmt, &mut scope, properties, &mut outputs) {
                            return Err(crate::runtime::error::RuntimeError::Eval(e));
                        }
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
                match native_node.on_input(trigger_port, input_value, &ctx) {
                    Ok(outputs) => Ok(ExecutionResult { outputs }),
                    Err(err) => Err(err),
                }
            }
        }
    }

    pub(crate) fn evaluate_expression(expr: &Expression, scope: &HashMap<String, Value>, properties: &HashMap<String, Value>) -> Result<Value, crate::runtime::evaluator::EvalError> {
        crate::runtime::evaluator::evaluate_expression(expr, scope, properties)
    }

    // Delegate pattern/match helpers to evaluator module
    fn match_pattern(
        pattern: &Pattern,
        value: &Value,
        bindings: &mut HashMap<String, Value>,
    ) -> Result<bool, crate::runtime::evaluator::EvalError> {
        crate::runtime::evaluator::match_pattern(pattern, value, bindings)
    }

    fn pattern_expr_to_value(expr: &Expression) -> Result<Value, String> {
        // Kept for compatibility with tests, but forwarded
        // (evaluator::pattern_expr_to_value is private)
        match expr {
            Expression::IntLiteral(i) => Ok(Value::Int(*i)),
            Expression::FloatLiteral(f) => Ok(Value::Float(*f)),
            Expression::StringLiteral(s) => Ok(Value::String(s.clone())),
            Expression::BoolLiteral(b) => Ok(Value::Bool(*b)),
            _ => Err("Only literal values allowed in patterns".to_string()),
        }
    }

    fn values_equal(a: &Value, b: &Value) -> bool {
        crate::runtime::evaluator::values_equal(a, b)
    }

    fn evaluate_binary_op(op: BinaryOperator, left: Value, right: Value) -> Result<Value, crate::runtime::evaluator::EvalError> {
        crate::runtime::evaluator::evaluate_binary_op(op, left, right)
    }
    
    /// Execute a single statement, potentially modifying scope and outputs
    fn execute_statement(
        stmt: &Statement,
        scope: &mut HashMap<String, Value>,
        properties: &HashMap<String, Value>,
        outputs: &mut Vec<(String, Value)>,
    ) -> Result<(), crate::runtime::evaluator::EvalError> {
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
    ) -> Result<(), crate::runtime::evaluator::EvalError> {
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
                        _ => return Err(crate::runtime::evaluator::EvalError::GuardNotBool),
                    }
                }

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
}
