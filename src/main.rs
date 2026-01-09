mod ast;
mod types;
mod parser;
mod graph;
mod runtime;
mod stdlib;

use clap::{Parser, Subcommand};
use std::fs;
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, Ordering};
use crate::graph::ExecutableGraph;
use crate::runtime::{scheduler::Scheduler, value::Value};


pub static VERBOSE: AtomicBool = AtomicBool::new(false);

#[inline]
pub fn is_verbose() -> bool {
    VERBOSE.load(Ordering::Relaxed)
}


#[macro_export]
macro_rules! verbose {
    ($($arg:tt)*) => {
        if $crate::is_verbose() {
            eprintln!($($arg)*);
        }
    };
}

#[derive(Parser)]
#[command(name = "dana")]
#[command(about = "Dana Language Compiler and Runtime", long_about = None)]
struct Cli {

    #[arg(long, global = true)]
    verbose: bool,
    
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {

    Run {

        path: PathBuf,
        #[arg(short, long)]
        trigger: Option<String>,
        #[arg(short, long)]
        value: Option<String>,
    },
    Check {
        path: PathBuf,
    },
}

fn main() {
    let cli = Cli::parse();
    
    VERBOSE.store(cli.verbose, Ordering::Relaxed);

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

    let ast = parser::parse_file(&content)
        .map_err(|e| format!("Parse error: {}", e))?;

    let graph = ExecutableGraph::from_ast(ast)
        .map_err(|e| format!("Build error: {}", e))?;

    let mut scheduler = Scheduler::new(graph);

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

        scheduler.inject_event(&node, *port, val).map_err(|e| e.to_string())?;
    } else {
        if !scheduler.auto_trigger().map_err(|e| e.to_string())? {
            println!("No trigger specified and no Impulse ports found. Graph loaded but no events injected.");
            println!("Use --trigger Node.Port to start execution.");
        }
    }

    scheduler.run().map_err(|e| e.to_string())?;
    
    println!("Execution finished correctly");
    Ok(())
}
