use crate::graph::ExecutableGraph;
use crate::runtime::value::Value;
use crate::runtime::pulse::Pulse;
use crate::runtime::pulse::TraceId;
use crate::runtime::scheduler::state::TraceStateStore;
use crate::verbose;
use std::collections::HashMap;
use std::sync::Arc;
use crossbeam_channel::Sender;
use dashmap::DashMap;
use std::sync::atomic::AtomicUsize;
use std::sync::Mutex;

use crate::runtime::scheduler::propagator;

pub fn execute_and_propagate(
    pulse: Pulse, 
    graph: &Arc<ExecutableGraph>, 
    state_store: &Arc<TraceStateStore>,
    tx: &Sender<Pulse>,
    tracker: &Arc<DashMap<TraceId, Arc<AtomicUsize>>>,
    trace_errors: &Arc<DashMap<TraceId, Arc<Mutex<Option<crate::runtime::error::RuntimeError>>>>>
) -> Result<(), String> {
    // 1. Get current node state from TraceStateStore
    let node = &graph.graph[pulse.target_node];
    let trace_state = state_store.get_node_state(pulse.trace_id, pulse.target_node)
        .unwrap_or_else(HashMap::new);

    verbose!("[EXEC {}] Node '{}' received on port '{}' with value {:?} (depth={})", pulse.trace_id, node.name, pulse.target_port, pulse.payload, pulse.depth);

    // NOTE: Constant injection disabled - it causes race conditions when constant nodes
    // also send pulses via auto_trigger. The pulses from constant nodes are sufficient.
    // Self::inject_constant_inputs(pulse.target_node, pulse.trace_id, &pulse.target_port, graph, state_store);

    // 2. Execute the target node
    let result = match node.execute(&pulse.target_port, pulse.payload.clone(), pulse.trace_id, state_store) {
        Ok(res) => res,
        Err(e) => {
            // Record error in tracker (wrap as RuntimeError::Other)
            if let Some(err_mutex) = trace_errors.get(&pulse.trace_id) {
                let mut lock = err_mutex.lock().unwrap();
                if lock.is_none() {
                    *lock = Some(crate::runtime::error::RuntimeError::Other(e.clone()));
                }
            }
            return Err(e);
        }
    };

    verbose!("[EXEC {}] Node '{}' produced {} outputs", pulse.trace_id, node.name, result.outputs.len());

    // Mark this node as having executed (for constant injection logic)
    if !result.outputs.is_empty() {
        state_store.mark_node_executed(pulse.trace_id, pulse.target_node);
    }

    // Build a map of all outputs for guard evaluation
    let outputs_map: HashMap<String, Value> = result.outputs.iter()
        .map(|(k, v)| (k.clone(), v.clone()))
        .collect();

    // 3. Propagate outputs to connected downstream nodes
    for (out_port, out_value) in result.outputs {
        propagator::propagate_from_port(
            pulse.target_node,
            &out_port,
            &out_value,
            &outputs_map, // Pass all outputs for guard evaluation
            graph,
            tx,
            tracker,
            pulse.trace_id,
            pulse.depth,
        );
    }

    // NOTE: We intentionally do NOT propagate the input value itself here.
    // If a node needs to "fork" its input to multiple destinations, it should
    // explicitly emit it as an output. This prevents duplicate propagation.

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Graph, Node, Port, ProcessBlock, Statement, Expression};
    use crate::types::DanaType;
    use crate::graph::ExecutableGraph;
    use crossbeam_channel::unbounded;
    use dashmap::DashMap;
    use std::sync::Arc;
    use std::sync::atomic::AtomicUsize;
    use std::sync::Mutex;

    #[test]
    fn test_execute_and_propagate_simple() {
        let mut ast = Graph::new();
        // Source node: on start emit out(42)
        let process = ProcessBlock { triggers: vec!["start".to_string()], statements: vec![Statement::Emit { port: "out".to_string(), value: Expression::IntLiteral(42) } ] };
        let mut source = Node::new("Source");
        source.input_ports.push(Port { name: "start".to_string(), type_annotation: DanaType::Unit, default_value: None });
        source.output_ports.push(Port { name: "out".to_string(), type_annotation: DanaType::Int, default_value: None });
        source.process = Some(process);

        let mut target = Node::new("Target");
        target.input_ports.push(Port { name: "in".to_string(), type_annotation: DanaType::Int, default_value: None });

        ast.add_node(source);
        ast.add_node(target);
        ast.add_edge(crate::ast::Edge { source: crate::ast::PortRef::new("Source", "out"), target: crate::ast::PortRef::new("Target", "in"), edge_type: crate::ast::EdgeType::Sync, guard: None });

        let mut graph = ExecutableGraph::from_ast(ast).unwrap();

        let (tx, rx) = unbounded();
        let tracker = Arc::new(DashMap::new());
        let tid = crate::runtime::pulse::TraceId::new();
        tracker.entry(tid).or_insert(Arc::new(AtomicUsize::new(1)));
        let trace_errors = Arc::new(DashMap::new());
        trace_errors.entry(tid).or_insert(Arc::new(Mutex::new(None)));

        let state = Arc::new(crate::runtime::scheduler::state::TraceStateStore::new());

        // Pulse to start the Source node
        let pulse = crate::runtime::pulse::Pulse::new(tid, *graph.node_map.get("Source").unwrap(), "start".to_string(), crate::runtime::value::Value::Unit, 0);

        execute_and_propagate(pulse, &Arc::new(graph), &state, &tx, &tracker, &trace_errors).unwrap();

        // Now expect a pulse to Target.in with payload 42
        let received = rx.try_recv().expect("should have a propagated pulse");
        assert_eq!(received.payload, crate::runtime::value::Value::Int(42));
    }
}
