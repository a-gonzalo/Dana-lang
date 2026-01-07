use crate::runtime::native::{NativeNode, NativeContext};
use crate::runtime::value::Value;

#[derive(Debug)]
pub struct SystemIONode;

impl NativeNode for SystemIONode {
    fn on_input(&self, port: &str, value: Value, _ctx: &NativeContext) -> Result<Vec<(String, Value)>, String> {
        match port {
            "stdout" => {
                println!("{}", value);
                Ok(Vec::new())
            }
            _ => Err(format!("Unknown input port {} for System.IO", port)),
        }
    }
}
