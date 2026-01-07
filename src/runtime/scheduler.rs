use crate::graph::ExecutableGraph;
use crate::runtime::value::Value;
use crate::runtime::pulse::{Pulse, TraceId};
use petgraph::graph::NodeIndex;
use std::collections::VecDeque;
use petgraph::Direction;
use petgraph::visit::EdgeRef;
use crate::runtime::node::{RuntimeNode, NodeKind}; 
use std::collections::HashMap;
use std::sync::{Arc, RwLock};

/// An event in the system (Legacy, will be replaced by Pulse)
#[derive(Debug, Clone)]
pub struct Event {
    pub target_node: NodeIndex,
    pub target_port: String,
    pub payload: Value,
}

use std::sync::atomic::{AtomicUsize, Ordering};
use dashmap::DashMap;
use crossbeam_channel::{Sender, Receiver, unbounded};
use std::thread;
use std::sync::Mutex;

const MAX_DEPTH: u32 = 1000;

/// The state store for ephemeral trace-local data
pub struct TraceStateStore {
    // Key: (TraceId, NodeIndex) -> Properties/State of that node for that trace
    states: RwLock<HashMap<(TraceId, NodeIndex), HashMap<String, Value>>>,
}

impl TraceStateStore {
    pub fn new() -> Self {
        Self {
            states: RwLock::new(HashMap::new()),
        }
    }

    pub fn get_node_state(&self, trace_id: TraceId, node_idx: NodeIndex) -> Option<HashMap<String, Value>> {
        let lock = self.states.read().unwrap();
        lock.get(&(trace_id, node_idx)).cloned()
    }

    pub fn set_node_state(&self, trace_id: TraceId, node_idx: NodeIndex, state: HashMap<String, Value>) {
        let mut lock = self.states.write().unwrap();
        lock.insert((trace_id, node_idx), state);
    }
    
    pub fn clear_trace(&self, trace_id: TraceId) {
        let mut lock = self.states.write().unwrap();
        lock.retain(|(tid, _), _| *tid != trace_id);
    }
}

pub struct Scheduler {
    graph: Arc<ExecutableGraph>,
    state_store: Arc<TraceStateStore>,
    pulse_tx: Sender<Pulse>,
    pulse_rx: Receiver<Pulse>,
    tracker: Arc<DashMap<TraceId, Arc<AtomicUsize>>>,
    trace_errors: Arc<DashMap<TraceId, Arc<Mutex<Option<String>>>>>,
}

impl Scheduler {
    pub fn new(graph: ExecutableGraph) -> Self {
        let (tx, rx) = unbounded();
        Self {
            graph: Arc::new(graph),
            state_store: Arc::new(TraceStateStore::new()),
            pulse_tx: tx,
            pulse_rx: rx,
            tracker: Arc::new(DashMap::new()),
            trace_errors: Arc::new(DashMap::new()),
        }
    }

    /// Inject an initial event to start execution, creating a new TraceID
    pub fn inject_event(&mut self, node_name: &str, port: &str, value: Value) -> Result<(), String> {
        let node_idx = *self.graph.node_map.get(node_name)
            .ok_or_else(|| format!("Node '{}' not found", node_name))?;
        
        let trace_id = TraceId::new();
        
        // Initialize tracker and error map for this trace
        self.tracker.insert(trace_id, Arc::new(AtomicUsize::new(1)));
        self.trace_errors.insert(trace_id, Arc::new(Mutex::new(None)));
        
        self.pulse_tx.send(Pulse::new(
            trace_id,
            node_idx,
            port.to_string(),
            value,
            0, // Start depth
        )).map_err(|e| format!("Failed to send initial pulse: {}", e))?;

        Ok(())
    }

    /// Run the scheduler until all traces are finished
    pub fn run(&mut self) -> Result<(), String> {
        let num_workers = thread::available_parallelism().map(|n| n.get()).unwrap_or(4);
        let mut workers = Vec::new();

        for _ in 0..num_workers {
            let rx = self.pulse_rx.clone();
            let tx = self.pulse_tx.clone();
            let graph = Arc::clone(&self.graph);
            let state_store = Arc::clone(&self.state_store);
            let tracker = Arc::clone(&self.tracker);
            let trace_errors = Arc::clone(&self.trace_errors);

            let handle = thread::spawn(move || {
                while let Ok(pulse) = rx.recv() {
                    // Check if this trace already has an error
                    if let Some(err_mutex) = trace_errors.get(&pulse.trace_id) {
                        if err_mutex.lock().unwrap().is_some() {
                            // Abandon this pulse
                            if let Some(counter) = tracker.get(&pulse.trace_id) {
                                counter.fetch_sub(1, Ordering::SeqCst);
                            }
                            continue;
                        }
                    }

                    // Check Depth for Loop Detection
                    if pulse.depth > MAX_DEPTH {
                        let err_msg = format!("Max execution depth reached for trace {} at node {}", pulse.trace_id, graph.graph[pulse.target_node].name);
                        if let Some(err_mutex) = trace_errors.get(&pulse.trace_id) {
                            let mut lock = err_mutex.lock().unwrap();
                            if lock.is_none() {
                                *lock = Some(err_msg);
                            }
                        }
                        
                        if let Some(counter) = tracker.get(&pulse.trace_id) {
                            counter.fetch_sub(1, Ordering::SeqCst);
                        }
                        continue;
                    }

                    if let Err(e) = Self::process_pulse_static(pulse, &graph, &state_store, &tx, &tracker, &trace_errors) {
                        eprintln!("Error processing pulse: {}", e);
                    }
                }
            });
            workers.push(handle);
        }

        // Wait until all trackers are zero
        let mut final_error = None;
        loop {
            if self.tracker.is_empty() {
                break;
            }
            
            // Cleanup finished traces or collect errors
            self.tracker.retain(|tid, counter| {
                if counter.load(Ordering::SeqCst) == 0 {
                    // Trace finished. Check if it had an error.
                    if let Some(err_mutex) = self.trace_errors.get(tid) {
                        let lock = err_mutex.lock().unwrap();
                        if let Some(err) = &*lock {
                            if final_error.is_none() {
                                final_error = Some(err.clone());
                            }
                        }
                    }
                    
                    self.state_store.clear_trace(*tid);
                    self.trace_errors.remove(tid);
                    false // Remove from tracker
                } else {
                    true
                }
            });

            if self.tracker.is_empty() {
                break;
            }
            
            thread::sleep(std::time::Duration::from_millis(10));
        }

        if let Some(err) = final_error {
            return Err(err);
        }

        Ok(())
    }

    fn process_pulse_static(
        pulse: Pulse, 
        graph: &Arc<ExecutableGraph>, 
        state_store: &Arc<TraceStateStore>,
        tx: &Sender<Pulse>,
        tracker: &Arc<DashMap<TraceId, Arc<AtomicUsize>>>,
        trace_errors: &Arc<DashMap<TraceId, Arc<Mutex<Option<String>>>>>
    ) -> Result<(), String> {
        let result = Self::execute_and_propagate(pulse.clone(), graph, state_store, tx, tracker, trace_errors);
        
        // ALWAYS DECREMENT when this pulse is done
        if let Some(counter) = tracker.get(&pulse.trace_id) {
            counter.fetch_sub(1, Ordering::SeqCst);
        }

        result
    }

    fn execute_and_propagate(
        pulse: Pulse, 
        graph: &Arc<ExecutableGraph>, 
        state_store: &Arc<TraceStateStore>,
        tx: &Sender<Pulse>,
        tracker: &Arc<DashMap<TraceId, Arc<AtomicUsize>>>,
        trace_errors: &Arc<DashMap<TraceId, Arc<Mutex<Option<String>>>>>
    ) -> Result<(), String> {
        // 1. Get current node state from TraceStateStore
        let trace_state = state_store.get_node_state(pulse.trace_id, pulse.target_node)
            .unwrap_or_else(HashMap::new);

        // 2. Execute the target node
        let result_raw = {
            let node = &graph.graph[pulse.target_node];
            
            if !node.input_ports.contains_key(&pulse.target_port) {
                 return Err(format!("Node '{}' has no input port '{}'", node.name, pulse.target_port));
            }

            node.execute(&pulse.target_port, pulse.payload, &trace_state)
        };

        let result = match result_raw {
            Ok(res) => res,
            Err(e) => {
                // Record error in tracker
                if let Some(err_mutex) = trace_errors.get(&pulse.trace_id) {
                    let mut lock = err_mutex.lock().unwrap();
                    if lock.is_none() {
                        *lock = Some(e.clone());
                    }
                }
                return Err(e);
            }
        };

        // 3. Propagate outputs to connected downstream nodes
        for (out_port, out_value) in result.outputs {
            let mut outgoing_pulses = Vec::new();
            
            for edge in graph.graph.edges_directed(pulse.target_node, Direction::Outgoing) {
                 if edge.weight().source_port == out_port {
                     // Guard check
                     if let Some(guard) = &edge.weight().guard {
                         let source_node = &graph.graph[pulse.target_node];
                         let properties = match &source_node.kind {
                             NodeKind::DanaProcess { properties, .. } => Some(properties),
                             _ => None,
                         };
                         
                         let empty_props = HashMap::new();
                         let props_ref = properties.unwrap_or(&empty_props);

                         let mut scope = HashMap::new();
                         scope.insert(out_port.clone(), out_value.clone());
                         for (k, v) in &trace_state {
                             scope.insert(k.clone(), v.clone());
                         }

                         match RuntimeNode::evaluate_expression(&guard.condition, &scope, props_ref) {
                             Ok(Value::Bool(true)) => {},
                             Ok(Value::Bool(false)) => continue,
                             _ => continue,
                         }
                     }
                     outgoing_pulses.push((edge.target(), edge.weight().target_port.clone()));
                 }
            }

            // Update tracker with new pulses BEFORE sending them
            if let Some(counter) = tracker.get(&pulse.trace_id) {
                counter.fetch_add(outgoing_pulses.len(), Ordering::SeqCst);
            }

            for (target_idx, target_port) in outgoing_pulses {
                tx.send(Pulse::new(
                    pulse.trace_id,
                    target_idx,
                    target_port,
                    out_value.clone(),
                    pulse.depth + 1,
                )).map_err(|e| format!("Failed to send pulse: {}", e))?;
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Graph, Node, Port, ProcessBlock, Statement, Expression};
    use crate::types::DanaType;

    #[test]
    fn test_simple_pipeline() {
        // Setup: Source -> Target
        // Source emits 42 on 'start' trigger
        // Target receives on 'in'
        
        // 1. Create AST
        let mut ast = Graph::new();
        
        // Source node logic: on start => emit out(42)
        let process = ProcessBlock {
            triggers: vec!["start".to_string()],
            statements: vec![
                Statement::Emit { 
                    port: "out".to_string(), 
                    value: Expression::IntLiteral(42) 
                }
            ],
        };
        
        let mut source = Node::new("Source");
        source.input_ports.push(Port { name: "start".to_string(), type_annotation: DanaType::Unit });
        source.output_ports.push(Port { name: "out".to_string(), type_annotation: DanaType::Int });
        source.process = Some(process);
        
        let mut target = Node::new("Target");
        target.input_ports.push(Port { name: "in".to_string(), type_annotation: DanaType::Int });
        
        ast.add_node(source);
        ast.add_node(target);
        
        ast.add_edge(crate::ast::Edge {
            source: crate::ast::PortRef::new("Source", "out"),
            target: crate::ast::PortRef::new("Target", "in"),
            edge_type: crate::ast::EdgeType::Sync,
            guard: None,
        });

        // 2. Build Graph
        let graph = ExecutableGraph::from_ast(ast).unwrap();
        
        // 3. Run Scheduler
        let mut scheduler = Scheduler::new(graph);
        scheduler.inject_event("Source", "start", Value::Unit).unwrap();
        
        // This should run: Source.start -> emit 42 -> Target.in
        let res = scheduler.run();
        assert!(res.is_ok());
        
        // Verify Target processed event (in this mock we just check it finished ok)
        // To verify properly we'd need to inspect Target's state or side effects.
        // For MVP tests, passing without error means events propagated.
    }

    #[test]
    fn test_guard_blocking() {
        // Setup scenarios: 
        // 1. Source -> [val > 10] -> Target (Failer)
        // Inject 5: Should block. Target not reached. Result OK.
        
        let mut ast = Graph::new();
        
        // Source Process
        let process = ProcessBlock {
            triggers: vec!["start".to_string()],
            statements: vec![
                Statement::Emit { 
                    port: "out".to_string(), 
                    value: Expression::Identifier("start".to_string()) 
                }
            ],
        };
        
        let mut source = Node::new("Source");
        source.input_ports.push(Port { name: "start".to_string(), type_annotation: DanaType::Int });
        source.output_ports.push(Port { name: "out".to_string(), type_annotation: DanaType::Int });
        source.process = Some(process);
        
        // Target (Failer)
        let mut target = Node::new("Failer");
        target.input_ports.push(Port { name: "in".to_string(), type_annotation: DanaType::Int });
        
        ast.add_node(source);
        ast.add_node(target);
        
        // Guard: out > 10
        let guard_expr = Expression::BinaryOp { 
            op: crate::ast::BinaryOperator::GreaterThan, 
            left: Box::new(Expression::Identifier("out".to_string())), 
            right: Box::new(Expression::IntLiteral(10)) 
        };
        
        ast.add_edge(crate::ast::Edge {
            source: crate::ast::PortRef::new("Source", "out"),
            target: crate::ast::PortRef::new("Failer", "in"),
            edge_type: crate::ast::EdgeType::Sync,
            guard: Some(crate::ast::Guard { condition: guard_expr }),
        });

        let mut graph = ExecutableGraph::from_ast(ast).unwrap();
        
        // Inject Failer Native Logic
        #[derive(Debug)]
        struct FailNode;
        impl crate::runtime::native::NativeNode for FailNode {
            fn on_input(&self, _port: &str, _value: Value) -> Result<Vec<(String, Value)>, String> {
                Err("FailNode executed".to_string())
            }
        }
        
        let fail_idx = *graph.node_map.get("Failer").unwrap();
        graph.graph[fail_idx] = RuntimeNode::new_native(
            "Failer".to_string(), 
            fail_idx, 
            graph.graph[fail_idx].input_ports.clone(), 
            HashMap::new(), 
            Box::new(FailNode)
        );

        let mut scheduler = Scheduler::new(graph);
        scheduler.inject_event("Source", "start", Value::Int(5)).unwrap();
        
        let res = scheduler.run();
        assert!(res.is_ok(), "Guard should Block 5. Result OK means Failer NOT reached.");
    }

    #[test]
    fn test_guard_passing() {
        // Setup scenarios: 
        // 2. Source -> [val > 10] -> Target (Failer)
        // Inject 15: Should pass. Target reached. Result Err.
        
        let mut ast = Graph::new();
        
        let process = ProcessBlock {
            triggers: vec!["start".to_string()],
            statements: vec![
                Statement::Emit { 
                    port: "out".to_string(), 
                    value: Expression::Identifier("start".to_string()) 
                }
            ],
        };
        
        let mut source = Node::new("Source");
        source.input_ports.push(Port { name: "start".to_string(), type_annotation: DanaType::Int });
        source.output_ports.push(Port { name: "out".to_string(), type_annotation: DanaType::Int });
        source.process = Some(process);
        
        let mut target = Node::new("Failer");
        target.input_ports.push(Port { name: "in".to_string(), type_annotation: DanaType::Int });
        
        ast.add_node(source);
        ast.add_node(target);
        
        let guard_expr = Expression::BinaryOp { 
            op: crate::ast::BinaryOperator::GreaterThan, 
            left: Box::new(Expression::Identifier("out".to_string())), 
            right: Box::new(Expression::IntLiteral(10)) 
        };
        
        ast.add_edge(crate::ast::Edge {
            source: crate::ast::PortRef::new("Source", "out"),
            target: crate::ast::PortRef::new("Failer", "in"),
            edge_type: crate::ast::EdgeType::Sync,
            guard: Some(crate::ast::Guard { condition: guard_expr }),
        });

        let mut graph = ExecutableGraph::from_ast(ast).unwrap();
        
        #[derive(Debug)]
        struct FailNode;
        impl crate::runtime::native::NativeNode for FailNode {
            fn on_input(&self, _port: &str, _value: Value) -> Result<Vec<(String, Value)>, String> {
                Err("FailNode executed".to_string())
            }
        }
        
        let fail_idx = *graph.node_map.get("Failer").unwrap();
        graph.graph[fail_idx] = RuntimeNode::new_native(
            "Failer".to_string(), 
            fail_idx, 
            graph.graph[fail_idx].input_ports.clone(), 
            HashMap::new(), 
            Box::new(FailNode)
        );
        
        let mut scheduler = Scheduler::new(graph);
        scheduler.inject_event("Source", "start", Value::Int(15)).unwrap();
        let res = scheduler.run();
        
        assert!(res.is_err(), "Guard should Pass 15. Result Err means Failer WAS reached.");
        assert_eq!(res.err().unwrap(), "FailNode executed");
    }

    #[test]
    fn test_infinite_loop_detection() {
        // Setup: Looper.out -> Looper.in (Cycle)
        let mut ast = Graph::new();
        
        let process = ProcessBlock {
            triggers: vec!["in".to_string()],
            statements: vec![
                Statement::Emit { 
                    port: "out".to_string(), 
                    value: Expression::IntLiteral(1) 
                }
            ],
        };
        
        let mut node = Node::new("Looper");
        node.input_ports.push(Port { name: "in".to_string(), type_annotation: DanaType::Int });
        node.output_ports.push(Port { name: "out".to_string(), type_annotation: DanaType::Int });
        node.process = Some(process);
        
        ast.add_node(node);
        ast.add_edge(crate::ast::Edge {
            source: crate::ast::PortRef::new("Looper", "out"),
            target: crate::ast::PortRef::new("Looper", "in"),
            edge_type: crate::ast::EdgeType::Sync,
            guard: None,
        });

        let graph = ExecutableGraph::from_ast(ast).unwrap();
        let mut scheduler = Scheduler::new(graph);
        
        scheduler.inject_event("Looper", "in", Value::Int(1)).unwrap();
        
        let res = scheduler.run();
        
        assert!(res.is_err(), "Infinite loop should be detected and return Err");
        assert!(res.err().unwrap().contains("Max execution depth reached"));
    }
}
