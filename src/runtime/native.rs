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
