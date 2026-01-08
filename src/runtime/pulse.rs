use crate::runtime::value::Value;
use petgraph::graph::NodeIndex;
use std::sync::atomic::{AtomicU64, Ordering};
use std::fmt;

static NEXT_TRACE_ID: AtomicU64 = AtomicU64::new(1);

/// Unique identifier for a data flow trace
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TraceId(u64);

impl TraceId {
    pub fn new() -> Self {
        Self(NEXT_TRACE_ID.fetch_add(1, Ordering::SeqCst))
    }
    
    pub fn value(&self) -> u64 {
        self.0
    }
}

impl fmt::Display for TraceId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "T{:04}", self.0)
    }
}

/// The atomic unit of execution in EPOS
#[derive(Debug, Clone)]
pub struct Pulse {
    /// The data payload being carried
    pub payload: Value,
    
    /// The trace context this pulse belongs to
    pub trace_id: TraceId,
    
    /// The target node where this pulse is directed
    pub target_node: NodeIndex,
    
    /// The input port of the target node
    pub target_port: String,

    /// Recursion depth (only for loop detection, not for state isolation)
    pub depth: u32,
}

impl Pulse {
    pub fn new(trace_id: TraceId, target_node: NodeIndex, target_port: String, payload: Value, depth: u32) -> Self {
        Self {
            trace_id,
            target_node,
            target_port,
            payload,
            depth,
        }
    }
}
