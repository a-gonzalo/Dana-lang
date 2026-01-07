use crate::runtime::value::Value;
use std::fmt::Debug;

/// Trait for native Rust nodes
pub trait NativeNode: Debug + Send + Sync {
    /// Execute the node logic given an input on a specific port
    /// Returns a list of (output_port, value) tuples to emit
    fn on_input(&self, port: &str, value: Value) -> Result<Vec<(String, Value)>, String>;
}
