use crate::runtime::native::{NativeNode, NativeContext};
use crate::runtime::value::Value;

#[derive(Debug)]
pub struct SystemIONode;

impl NativeNode for SystemIONode {
    fn on_input(&self, port: &str, value: Value, _ctx: &NativeContext) -> Result<Vec<(String, Value)>, crate::runtime::error::RuntimeError> {
        match port {
            "stdout" => {
                println!("{}", value);
                Ok(Vec::new())
            }
            _ => Err(crate::runtime::error::RuntimeError::Native(format!("Unknown input port {} for System.IO", port))),
        }
    }
}
