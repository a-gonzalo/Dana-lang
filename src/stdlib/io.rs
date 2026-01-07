use crate::runtime::native::NativeNode;
use crate::runtime::value::Value;

#[derive(Debug)]
pub struct SystemIONode;

impl NativeNode for SystemIONode {
    fn on_input(&self, port: &str, value: Value) -> Result<Vec<(String, Value)>, String> {
        match port {
            "stdout" => {
                println!("{}", value);
                Ok(Vec::new())
            }
            _ => Err(format!("Unknown input port {} for System.IO", port)),
        }
    }
}
