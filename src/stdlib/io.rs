use crate::runtime::native::{NativeNode, NativeContext};
use crate::runtime::value::Value;
use crate::runtime::error::RuntimeError;

#[derive(Debug)]
pub struct SystemIONode;

impl NativeNode for SystemIONode {
    fn on_input(&self, port: &str, value: Value, _ctx: &NativeContext) -> Result<Vec<(String, Value)>, RuntimeError> {
        match port {
            "stdout" => {
                println!("{}", value);
                Ok(Vec::new())
            }
            _ => Err(RuntimeError::Native(format!("Unknown input port {} for System.IO", port))),
        }
    }
}
