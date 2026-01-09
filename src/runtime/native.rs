use crate::runtime::value::Value;
use crate::runtime::pulse::TraceId;
use crate::runtime::scheduler::TraceStateStore;
use petgraph::graph::NodeIndex;
use std::fmt::Debug;
use crate::runtime::error::RuntimeError;   

/// Context passed to native nodes during execution
pub struct NativeContext<'a> {
    pub trace_id: TraceId,
    pub node_idx: NodeIndex,
    pub state_store: &'a TraceStateStore,
}

/// Trait for native Rust nodes
pub trait NativeNode: Debug + Send + Sync {
    /// Execute the node logic given an input on a specific port
    /// Returns a list of (output_port, value) tuples to emit
    fn on_input(&self, port: &str, value: Value, ctx: &NativeContext) -> Result<Vec<(String, Value)>, RuntimeError>;
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::value::Value;
    use crate::runtime::scheduler::state::TraceStateStore;
    use crate::runtime::pulse::TraceId;
    use petgraph::graph::NodeIndex;

    #[derive(Debug)]
    struct EchoNode;
    impl NativeNode for EchoNode {
        fn on_input(&self, port: &str, value: Value, ctx: &NativeContext) -> Result<Vec<(String, Value)>, RuntimeError> {
            // Echo the port and include trace info via an output value map
            let state = ctx.state_store.get_node_state(ctx.trace_id, ctx.node_idx).unwrap_or_default();
            // Return an output 'out' with a tuple (port, traceid as int if set in state 'marker')
            let mut out_value = Value::String(format!("{}", port));
            if let Some(Value::Int(marker)) = state.get("marker") {
                out_value = Value::List(vec![Value::String(port.to_string()), Value::Int(*marker)]);
            }
            Ok(vec![("out".to_string(), out_value)])
        }
    }

    #[derive(Debug)]
    struct FailNode;
    impl NativeNode for FailNode {
        fn on_input(&self, _port: &str, _value: Value, _ctx: &NativeContext) -> Result<Vec<(String, Value)>, RuntimeError> {
            Err(RuntimeError::Native("native failure".to_string()))
        }
    }

    #[test]
    fn test_native_context_access_and_output() {
        let store = TraceStateStore::new();
        let tid = TraceId::new();
        let idx = NodeIndex::new(2);
        // Set a marker in the state to verify access
        let mut state = std::collections::HashMap::new();
        state.insert("marker".to_string(), Value::Int(99));
        store.set_node_state(tid, idx, state);

        let ctx = NativeContext { trace_id: tid, node_idx: idx, state_store: &store };
        let node = EchoNode;
        let res = node.on_input("in", Value::Int(1), &ctx).unwrap();
        assert_eq!(res.len(), 1);
        if let Value::List(items) = &res[0].1 {
            assert_eq!(items[0], Value::String("in".to_string()));
            assert_eq!(items[1], Value::Int(99));
        } else {
            panic!("expected list output");
        }
    }

    #[test]
    fn test_native_error_propagation() {
        let store = TraceStateStore::new();
        let tid = TraceId::new();
        let idx = NodeIndex::new(3);
        let ctx = NativeContext { trace_id: tid, node_idx: idx, state_store: &store };
        let node = FailNode;
        let err = node.on_input("x", Value::Unit, &ctx).err().unwrap();
        match err {
            RuntimeError::Native(s) => assert_eq!(s, "native failure"),
            other => panic!("expected Native error, got {:?}", other),
        }
    }
}
