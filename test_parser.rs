use dana_lang::parser::{DanaParser, parse_file};
use pest::Parser;

fn main() {
    let input = r#"
node TestNode {
    count: Int = 0
}
"#;

    match parse_file(input) {
        Ok(graph) => {
            println!("Successfully parsed!");
            println!("Nodes: {}", graph.nodes.len());
        }
        Err(e) => {
            eprintln!("Parse error: {}", e);
        }
    }
}
