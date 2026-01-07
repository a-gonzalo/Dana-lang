mod ast;
mod types;
mod parser;
mod graph;
mod runtime;
mod stdlib;

use clap::{Parser, Subcommand};
use std::fs;
use std::path::PathBuf;
use crate::graph::ExecutableGraph;
use crate::runtime::{scheduler::Scheduler, value::Value};

#[derive(Parser)]
#[command(name = "dana")]
#[command(about = "Dana Language Compiler and Runtime", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Run a Dana program
    Run {
        /// Path to the source file
        path: PathBuf,
        
        /// Initial trigger (Node.Port)
        #[arg(short, long)]
        trigger: Option<String>,
        
        /// Initial value (default: Unit)
        #[arg(short, long)]
        value: Option<String>,
    },
    /// Check syntax and types without running
    Check {
        /// Path to the source file
        path: PathBuf,
    },
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Run { path, trigger, value } => {
            if let Err(e) = run_program(path, trigger, value) {
                eprintln!("Error: {}", e);
                std::process::exit(1);
            }
        }
        Commands::Check { path } => {
             if let Err(e) = check_program(path) {
                eprintln!("Check failed: {}", e);
                std::process::exit(1);
            } else {
                println!("Feature checks passed.");
            }
        }
    }
}

fn check_program(path: PathBuf) -> Result<(), String> {
    let content = fs::read_to_string(&path)
        .map_err(|e| format!("Could not read file {:?}: {}", path, e))?;

    println!("Parsing...");
    let ast = parser::parse_file(&content)
        .map_err(|e| format!("{}", e))?;

    println!("Building graph...");
    let _graph = ExecutableGraph::from_ast(ast)
        .map_err(|e| format!("{}", e))?;
        
    Ok(())
}

fn run_program(path: PathBuf, trigger: Option<String>, val_str: Option<String>) -> Result<(), String> {
    let content = fs::read_to_string(&path)
        .map_err(|e| format!("Could not read file {:?}: {}", path, e))?;

    // 1. Parse
    let ast = parser::parse_file(&content)
        .map_err(|e| format!("Parse error: {}", e))?;

    // 2. Build
    let graph = ExecutableGraph::from_ast(ast)
        .map_err(|e| format!("Build error: {}", e))?;

    // 3. Run
    let mut scheduler = Scheduler::new(graph);

    // Parse trigger "Node.Port" or "Package.Node.Port"
    if let Some(trig) = trigger {
        let parts: Vec<&str> = trig.split('.').collect();
        if parts.len() < 2 {
            return Err("Trigger must be in format Node.Port".to_string());
        }
        let port = parts.last().unwrap();
        let node = parts[0..parts.len()-1].join(".");
        
        let val = if let Some(s) = val_str {
            // Very basic parsing for now. TODO: improved value parsing
             if let Ok(i) = s.parse::<i64>() {
                Value::Int(i)
            } else if let Ok(f) = s.parse::<f64>() {
                Value::Float(f)
            } else if s == "true" {
                Value::Bool(true)
            } else if s == "false" {
                Value::Bool(false)
            } else {
                Value::String(s)
            }
        } else {
            Value::Unit
        };

        scheduler.inject_event(&node, *port, val)?;
    } else {
        // Automatically find and trigger all Impulse/Unit ports
        if !scheduler.auto_trigger()? {
            println!("No trigger specified and no Impulse ports found. Graph loaded but no events injected.");
            println!("Use --trigger Node.Port to start execution.");
        }
    }

    scheduler.run()?;
    
    println!("Execution finished correctly");
    Ok(())
}
