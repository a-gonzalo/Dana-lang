use crate::runtime::native::{NativeNode, NativeContext};
use crate::runtime::value::Value;
use std::collections::HashMap;

/// System.Kernel.Collector
/// Accumulates pulses in a list and emits them on reset
#[derive(Debug)]
pub struct CollectorNode;

impl NativeNode for CollectorNode {
    fn on_input(&self, port: &str, value: Value, ctx: &NativeContext) -> Result<Vec<(String, Value)>, String> {
        let mut state = ctx.state_store.get_node_state(ctx.trace_id, ctx.node_idx);
        
        match port {
            "in" => {
                // Get or create the list
                let mut list = match state.remove("list") {
                    Some(Value::List(l)) => l,
                    _ => Vec::new(),
                };
                list.push(value);
                state.insert("list".to_string(), Value::List(list));
                ctx.state_store.set_node_state(ctx.trace_id, ctx.node_idx, state);
                Ok(Vec::new())
            }
            "reset" => {
                // Emit current list and clear
                let list_val = state.remove("list").unwrap_or(Value::List(Vec::new()));
                ctx.state_store.set_node_state(ctx.trace_id, ctx.node_idx, state);
                Ok(vec![("out".to_string(), list_val)])
            }
            _ => Err(format!("Unknown input port {} for Collector", port)),
        }
    }
}

/// System.Kernel.Join
/// Waits for both 'a' and 'b' to have values, then emits [a, b]
#[derive(Debug)]
pub struct JoinNode;

impl NativeNode for JoinNode {
    fn on_input(&self, port: &str, value: Value, ctx: &NativeContext) -> Result<Vec<(String, Value)>, String> {
        let mut state = ctx.state_store.get_node_state(ctx.trace_id, ctx.node_idx);
        
        match port {
            "a" | "b" => {
                state.insert(port.to_string(), value);
                
                if state.contains_key("a") && state.contains_key("b") {
                    let a = state.remove("a").unwrap();
                    let b = state.remove("b").unwrap();
                    ctx.state_store.set_node_state(ctx.trace_id, ctx.node_idx, state);
                    Ok(vec![("out".to_string(), Value::List(vec![a, b]))])
                } else {
                    ctx.state_store.set_node_state(ctx.trace_id, ctx.node_idx, state);
                    Ok(Vec::new())
                }
            }
            _ => Err(format!("Unknown input port {} for Join", port)),
        }
    }
}
