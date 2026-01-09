use crate::graph::ExecutableGraph;
use crate::runtime::value::Value;
use crate::runtime::pulse::Pulse;
use petgraph::graph::NodeIndex;
use petgraph::Direction;
use petgraph::visit::EdgeRef;
use crate::runtime::node::NodeKind;
use crate::verbose;
use std::collections::HashMap;
use std::sync::Arc;
use crossbeam_channel::Sender;
use dashmap::DashMap;
use std::sync::atomic::AtomicUsize;
use crate::runtime::pulse::TraceId;

/// Propagate outputs from a source node port to downstream nodes, evaluating guards.
pub fn propagate_from_port(
    source_idx: NodeIndex,
    source_port: &str,
    value: &Value,
    all_outputs: &HashMap<String, Value>, // All outputs from the node for guard evaluation
    graph: &Arc<ExecutableGraph>,
    tx: &Sender<Pulse>,
    tracker: &Arc<DashMap<TraceId, Arc<AtomicUsize>>>,
    trace_id: TraceId,
    depth: u32,
) {
    let source_node_name = &graph.graph[source_idx].name;
    let mut outgoing_pulses = Vec::new();
    
    for edge in graph.graph.edges_directed(source_idx, Direction::Outgoing) {
         if edge.weight().source_port == source_port {
             let target_idx = edge.target();
             let target_node = &graph.graph[target_idx];
             let target_node_name = &target_node.name;
             let target_port = &edge.weight().target_port;
             
             // Guard check
             if let Some(guard) = &edge.weight().guard {
                 let source_node = &graph.graph[source_idx];
                 let properties = match &source_node.kind {
                     NodeKind::DanaProcess { properties, .. } => Some(properties),
                     _ => None,
                 };
                 
                 let empty_props = HashMap::new();
                 let props_ref = properties.unwrap_or(&empty_props);

                 // Build scope with current value and all outputs for guard evaluation
                 let mut scope = HashMap::new();
                 scope.insert(source_port.to_string(), value.clone());
                 for (k, v) in all_outputs {
                     scope.insert(k.clone(), v.clone());
                 }

                 match crate::runtime::node::RuntimeNode::evaluate_expression(&guard.condition, &scope, props_ref) {
                     Ok(Value::Bool(true)) => {
                         verbose!("[PROP {}] Guard passed for {}.{} -> {}.{}", trace_id, source_node_name, source_port, target_node_name, target_port);
                     },
                     Ok(Value::Bool(false)) => {
                         verbose!("[PROP {}] Guard failed for {}.{} -> {}.{}", trace_id, source_node_name, source_port, target_node_name, target_port);
                         continue;
                     },
                     _ => {
                         verbose!("[PROP {}] Guard error for {}.{} -> {}.{}", trace_id, source_node_name, source_port, target_node_name, target_port);
                         continue;
                     }
                 }
             } else {
                 verbose!("[PROP {}] Propagating {}.{} -> {}.{} with value {:?}", trace_id, source_node_name, source_port, target_node_name, target_port, value);
             }
             outgoing_pulses.push((target_idx, target_port.clone()));
         }
    }

    // Update tracker with new pulses BEFORE sending them
    if !outgoing_pulses.is_empty() {
        if let Some(counter) = tracker.get(&trace_id) {
            counter.fetch_add(outgoing_pulses.len(), std::sync::atomic::Ordering::SeqCst);
        }

        for (target_idx, target_port) in outgoing_pulses {
            let target_node_name = &graph.graph[target_idx].name;
            let target_node = &graph.graph[target_idx];
            
            // Simple depth increment for loop detection only
            let next_depth = depth + 1;
            
            verbose!("[SEND {}] Pulse sent to {}.{} (depth={})", trace_id, target_node_name, target_port, next_depth);
            let _ = tx.send(Pulse::new(
                trace_id,
                target_idx,
                target_port.clone(),
                value.clone(),
                next_depth,
            ));
            
            // For virtual nodes (subgraphs), when a pulse arrives at a port that is ALSO an output,
            // automatically propagate it downstream. This enables patterns like:
            // internal_node.result -> subgraph.result -> downstream_node
            // 
            // Key insight: We check if the port is in output_ports AND we're sending TO an input port
            // to avoid infinite recursion. The auto-propagation happens when we reach an OUTPUT port.
            if matches!(&target_node.kind, NodeKind::DanaProcess { process, .. } if process.is_none()) {
                // This is a virtual node (subgraph)
                if target_node.output_ports.contains_key(&target_port) {
                    verbose!("[AUTO] Virtual node '{}' has output port '{}', auto-propagating downstream", target_node_name, target_port);
                    
                    propagate_from_port(
                        target_idx,
                        &target_port,
                        value,
                        &HashMap::new(),
                        graph,
                        tx,
                        tracker,
                        trace_id,
                        next_depth,
                    );
                }
            }
        }
    } else {
        verbose!("[PROP {}] No outgoing pulses from {}.{}", trace_id, source_node_name, source_port);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Graph, Node, Port};
    use crate::types::DanaType;
    use crate::runtime::value::Value;
    use crate::graph::ExecutableGraph;
    use crossbeam_channel::unbounded;
    use dashmap::DashMap;
    use std::sync::Arc;
    use std::sync::atomic::AtomicUsize;

    #[test]
    fn test_propagate_simple() {
        // Build AST: A.out -> B.in
        let mut ast = Graph::new();
        let mut a = Node::new("A");
        a.output_ports.push(Port { name: "out".to_string(), type_annotation: DanaType::Int, default_value: None});
        let mut b = Node::new("B");
        b.input_ports.push(Port { name: "in".to_string(), type_annotation: DanaType::Int, default_value: None});
        ast.add_node(a);
        ast.add_node(b);
        ast.add_edge(crate::ast::Edge { source: crate::ast::PortRef::new("A", "out"), target: crate::ast::PortRef::new("B", "in"), edge_type: crate::ast::EdgeType::Sync, guard: None});

        let graph = ExecutableGraph::from_ast(ast).unwrap();
        let src_idx = *graph.node_map.get("A").unwrap();

        let (tx, rx) = unbounded();
        let tracker = Arc::new(DashMap::new());
        let tid = crate::runtime::pulse::TraceId::new();
        tracker.entry(tid).or_insert(Arc::new(AtomicUsize::new(1)));

        propagate_from_port(src_idx, "out", &Value::Int(7), &std::collections::HashMap::new(), &Arc::new(graph), &tx, &tracker, tid, 0);

        let received = rx.try_recv().expect("should have a pulse");
        assert_eq!(received.payload, Value::Int(7));
    }
}
