/// Synchronous Scheduler for Dana MVP
///
/// Drives the execution of the graph by propagating events (values) between nodes.

use crate::graph::ExecutableGraph;
use crate::runtime::value::Value;
use petgraph::graph::NodeIndex;
use std::collections::VecDeque;
use petgraph::Direction;
use petgraph::visit::EdgeRef;
use crate::runtime::node::{RuntimeNode, NodeKind}; // Need RuntimeNode and NodeKind for evaluating expression
use std::collections::HashMap;

/// An event in the system
#[derive(Debug, Clone)]
pub struct Event {
    pub target_node: NodeIndex,
    pub target_port: String,
    pub payload: Value,
}

pub struct Scheduler {
    graph: ExecutableGraph,
    event_queue: VecDeque<Event>,
    max_steps: usize,
}

impl Scheduler {
    pub fn new(graph: ExecutableGraph) -> Self {
        Self {
            graph,
            event_queue: VecDeque::new(),
            max_steps: 1000, // Safety limit for infinite loops
        }
    }

    /// Inject an initial event to start execution
    pub fn inject_event(&mut self, node_name: &str, port: &str, value: Value) -> Result<(), String> {
        let node_idx = *self.graph.node_map.get(node_name)
            .ok_or_else(|| format!("Node '{}' not found", node_name))?;
        
        self.event_queue.push_back(Event {
            target_node: node_idx,
            target_port: port.to_string(),
            payload: value,
        });

        Ok(())
    }

    /// Run the scheduler until the queue is empty or max steps reached
    pub fn run(&mut self) -> Result<(), String> {
        let mut steps = 0;

        while let Some(event) = self.event_queue.pop_front() {
            if steps >= self.max_steps {
                return Err("Max execution steps reached (infinite loop suspected)".to_string());
            }
            steps += 1;

            self.process_event(event)?;
        }

        Ok(())
    }

    fn process_event(&mut self, event: Event) -> Result<(), String> {
        // 1. Execute the target node
        let result = {
            let node = &mut self.graph.graph[event.target_node];
            
            // Skip if port doesn't exist (validation should have caught this, but runtime safety)
            if !node.input_ports.contains_key(&event.target_port) {
                 return Err(format!("Node '{}' has no input port '{}'", node.name, event.target_port));
            }

            node.execute(&event.target_port, event.payload)?
        };

        // 2. Propagate outputs to connected downstream nodes
        for (out_port, out_value) in result.outputs {
            let mut outgoing_events = Vec::new();
            
            // Find all edges connected to this output port
            for edge in self.graph.graph.edges_directed(event.target_node, Direction::Outgoing) {
                 if edge.weight().source_port == out_port {
                     // Check Guard
                     if let Some(guard) = &edge.weight().guard {
                         // To evaluate the guard, we need a scope.
                         // For now (MVP v0.1), the scope contains the output value bound to the port name.
                         // AND potentially we should allow access to the source node properties?
                         // Let's allow it. We need access to the source node again.
                         
                         // We can't easily get mutable access to the node because we are iterating the graph (immutable borrow?).
                         // Wait, self.graph.graph is used for iteration.
                         // However, 'node' (mutable borrow) scope ended above? No, 'let node = &mut ...' is alive?
                         // Line 67: let node = &mut self.graph.graph[...]
                         // If 'node' is still alive, we can't iterate self.graph.graph.
                         // We should drop 'node' borrow before iterating edges. 
                         // But we need 'result' which came from 'node'.
                         
                         // Fix: 'node' borrow must end. 'result' is owned (ExecutionResult cloned outputs).
                         // So we are good if we don't use 'node' here.
                         // But to evaluate guard with Properties, we need read access to the node.
                         
                         // Let's re-borrow the node immutably to get properties.
                         let source_node = &self.graph.graph[event.target_node];
                         let properties = match &source_node.kind {
                             NodeKind::DanaProcess { properties, .. } => Some(properties),
                             _ => None,
                         };
                         
                         let empty_props = HashMap::new();
                         let props_ref = properties.unwrap_or(&empty_props);

                         let mut scope = HashMap::new();
                         scope.insert(out_port.clone(), out_value.clone());
                         
                         match RuntimeNode::evaluate_expression(&guard.condition, &scope, props_ref) {
                             Ok(Value::Bool(true)) => {
                                 // Guard pass
                             },
                             Ok(Value::Bool(false)) => {
                                 continue; // Guard blocked
                             },
                             Ok(_) => {
                                 // Non-bool result treated as false/error? For now block.
                                 eprintln!("Warning: Guard evaluated to non-boolean value. Blocking edge.");
                                 continue;
                             }
                             Err(e) => {
                                 eprintln!("Error evaluating guard: {}. Blocking edge.", e);
                                 continue;
                             }
                         }
                     }

                     outgoing_events.push((edge.target(), edge.weight().target_port.clone()));
                 }
            }

            // Create events for downstream nodes
            for (target_idx, target_port) in outgoing_events {
                self.event_queue.push_back(Event {
                    target_node: target_idx,
                    target_port,
                    payload: out_value.clone(),
                });
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
            fn on_input(&mut self, _port: &str, _value: Value) -> Result<Vec<(String, Value)>, String> {
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
            fn on_input(&mut self, _port: &str, _value: Value) -> Result<Vec<(String, Value)>, String> {
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
}
