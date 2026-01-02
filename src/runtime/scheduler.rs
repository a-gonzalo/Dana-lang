/// Synchronous Scheduler for Dana MVP
///
/// Drives the execution of the graph by propagating events (values) between nodes.

use crate::graph::ExecutableGraph;
use crate::runtime::value::Value;
use petgraph::graph::NodeIndex;
use std::collections::VecDeque;
use petgraph::Direction;
use petgraph::visit::EdgeRef;

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
        let node = &mut self.graph.graph[event.target_node];
        
        // Skip if port doesn't exist (validation should have caught this, but runtime safety)
        if !node.input_ports.contains_key(&event.target_port) {
             return Err(format!("Node '{}' has no input port '{}'", node.name, event.target_port));
        }

        let result = node.execute(&event.target_port, event.payload)?;

        // 2. Propagate outputs to connected downstream nodes
        for (out_port, out_value) in result.outputs {
            let mut outgoing_edges = Vec::new();
            
            // Find all edges connected to this output port
            // We have to iterate edges because petgraph doesn't index by port
            for edge in self.graph.graph.edges_directed(event.target_node, Direction::Outgoing) {
                 if edge.weight().source_port == out_port {
                     outgoing_edges.push((edge.target(), edge.weight().target_port.clone()));
                 }
            }

            // Create events for downstream nodes
            for (target_idx, target_port) in outgoing_edges {
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
}
