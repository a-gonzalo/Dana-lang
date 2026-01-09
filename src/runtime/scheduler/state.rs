use crate::runtime::value::Value;
use petgraph::graph::NodeIndex;
use crate::runtime::pulse::TraceId;
use std::collections::{HashMap, HashSet};
use std::sync::RwLock;

/// The state store for ephemeral trace-local data
/// Key: (TraceId, NodeIndex) -> State of that node for that trace
#[derive(Debug)]
pub struct TraceStateStore {
    states: RwLock<HashMap<(TraceId, NodeIndex), HashMap<String, Value>>>,
    /// Tracks which nodes have executed at least once for each trace
    /// Used to determine if constant injection should happen
    executed_nodes: RwLock<HashSet<(TraceId, NodeIndex)>>,
}

impl TraceStateStore {
    pub fn new() -> Self {
        Self {
            states: RwLock::new(HashMap::new()),
            executed_nodes: RwLock::new(HashSet::new()),
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
    
    pub fn has_node_executed(&self, trace_id: TraceId, node_idx: NodeIndex) -> bool {
        let lock = self.executed_nodes.read().unwrap();
        lock.contains(&(trace_id, node_idx))
    }
    
    pub fn mark_node_executed(&self, trace_id: TraceId, node_idx: NodeIndex) {
        let mut lock = self.executed_nodes.write().unwrap();
        lock.insert((trace_id, node_idx));
    }
    
    pub fn clear_trace(&self, trace_id: TraceId) {
        let mut lock = self.states.write().unwrap();
        lock.retain(|(tid, _), _| *tid != trace_id);
        
        let mut exec_lock = self.executed_nodes.write().unwrap();
        exec_lock.retain(|(tid, _)| *tid != trace_id);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use petgraph::graph::NodeIndex;

    #[test]
    fn test_trace_state_store_basic() {
        let store = TraceStateStore::new();
        let tid = TraceId::new();
        let idx = NodeIndex::new(1);

        assert!(store.get_node_state(tid, idx).is_none());

        let mut st = std::collections::HashMap::new();
        st.insert("a".to_string(), crate::runtime::value::Value::Int(5));
        store.set_node_state(tid, idx, st.clone());

        let got = store.get_node_state(tid, idx).unwrap();
        assert_eq!(got.get("a").cloned().unwrap(), crate::runtime::value::Value::Int(5));

        assert!(!store.has_node_executed(tid, idx));
        store.mark_node_executed(tid, idx);
        assert!(store.has_node_executed(tid, idx));

        store.clear_trace(tid);
        assert!(store.get_node_state(tid, idx).is_none());
        assert!(!store.has_node_executed(tid, idx));
    }
}
