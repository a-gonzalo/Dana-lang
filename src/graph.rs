//! Build and validate an executable graph from AST.
//!
//! Converts `Ast::Graph` into an `ExecutableGraph` (petgraph::DiGraph) with `RuntimeNode` / `RuntimeEdge`.
//! Handles virtual nodes for subgraphs and injects system nodes (IO, Kernel).
use crate::ast::{Graph as AstGraph, Edge as AstEdge, EdgeType, Guard};
use crate::types::{DanaType, TypeChecker, TypeError};
use crate::runtime::node::RuntimeNode;
use crate::runtime::value::Value;
use petgraph::graph::{DiGraph, NodeIndex};
use std::collections::HashMap;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum BuildError {
    #[error("Node not found: {0}")]
    NodeNotFound(String),
    
    #[error("Port not found: {0}.{1}")]
    PortNotFound(String, String),
    
    #[error("Type error: {0}")]
    TypeError(#[from] TypeError),
    
    #[error("Duplicate node name: {0}")]
    DuplicateNode(String),
}


#[derive(Debug, Clone)]
pub struct RuntimeEdge {
    pub source_port: String,
    pub target_port: String,
    pub edge_type: EdgeType,
    pub guard: Option<Guard>,
}


pub struct ExecutableGraph {
    pub graph: DiGraph<RuntimeNode, RuntimeEdge>,
    pub node_map: HashMap<String, NodeIndex>,
}

impl ExecutableGraph {
    pub fn from_ast(ast: AstGraph) -> Result<Self, BuildError> {
        let mut graph = DiGraph::new();
        let mut node_map = HashMap::new();

        Self::add_system_nodes(&mut graph, &mut node_map);

        for ast_node in ast.nodes {
            Self::add_node_to_graph(&mut graph, &mut node_map, ast_node)?;
        }

        for subgraph in &ast.subgraphs {
            // Create a virtual node for the graph's own ports
            // It is bi-directional: 
            // - Internal nodes can consume from graph inputs (it's an output from our perspective)
            // - Internal nodes can emit to graph outputs (it's an input from our perspective)
            // - Internal nodes can emit to graph inputs for recursion
            let mut internal_ports = HashMap::new();
            for port in subgraph.input_ports.iter().chain(subgraph.output_ports.iter()) {
                internal_ports.insert(port.name.clone(), port.type_annotation.clone());
            }

            // Add implicit '_start' port for graphs with 0 inputs (source graphs)
            if internal_ports.is_empty() {
                internal_ports.insert("_start".to_string(), DanaType::Unit);
            }

            let virtual_node = RuntimeNode::new_dana(
                subgraph.name.clone(),
                NodeIndex::new(0),
                internal_ports.clone(), 
                internal_ports,
                HashMap::new(),
                None,
            );
            let idx = graph.add_node(virtual_node);
            graph[idx].index = idx;
            node_map.insert(subgraph.name.clone(), idx);

            let prefix = if subgraph.name.is_empty() { "".to_string() } else { format!("{}.", subgraph.name) };
            for ast_node in &subgraph.nodes {
                let mut node_clone = ast_node.clone();
                node_clone.name = format!("{}{}", prefix, ast_node.name);
                Self::add_node_to_graph(&mut graph, &mut node_map, node_clone)?;
            }
        }


        for ast_edge in ast.edges {
            Self::validate_and_add_edge(&mut graph, &node_map, &ast_edge)?;
        }

        for subgraph in &ast.subgraphs {
            for ast_edge in &subgraph.edges {
                let mut edge_clone = ast_edge.clone();
                // Resolve empty node names to the subgraph node name
                if edge_clone.source.node.is_empty() {
                    edge_clone.source.node = subgraph.name.clone();
                }
                if edge_clone.target.node.is_empty() {
                    edge_clone.target.node = subgraph.name.clone();
                }

                // Prefix internal node references if they don't contain a dot (and aren't system/graph nodes)
                // Actually, if we allow cross-subgraph connections like Math.Sub.a, 
                // we should probably check if the node exists as-is first.
                
                if !edge_clone.source.node.contains('.') && !node_map.contains_key(&edge_clone.source.node) {
                     edge_clone.source.node = format!("{}.{}", subgraph.name, edge_clone.source.node);
                }
                if !edge_clone.target.node.contains('.') && !node_map.contains_key(&edge_clone.target.node) {
                     edge_clone.target.node = format!("{}.{}", subgraph.name, edge_clone.target.node);
                }

                Self::validate_and_add_edge(&mut graph, &node_map, &edge_clone)?;
            }
        }

        Ok(Self { graph, node_map })
    }

    fn add_system_nodes(graph: &mut DiGraph<RuntimeNode, RuntimeEdge>, node_map: &mut HashMap<String, NodeIndex>) {
        let name = "System.IO".to_string();
        if !node_map.contains_key(&name) {
            let mut input_ports = HashMap::new();
            input_ports.insert("stdout".to_string(), DanaType::Any);
            
            let output_ports = HashMap::new();
            
            let rt_node = RuntimeNode::new_native(
                name.clone(),
                NodeIndex::new(0),
                input_ports,
                output_ports,
                Box::new(crate::stdlib::io::SystemIONode),
            );
            
            let idx = graph.add_node(rt_node);
            graph[idx].index = idx;
            node_map.insert(name, idx);
        }

        let name = "System.Kernel.Collector".to_string();
        if !node_map.contains_key(&name) {
            let mut input_ports = HashMap::new();
            input_ports.insert("in".to_string(), DanaType::Any);
            input_ports.insert("reset".to_string(), DanaType::Unit);
            
            let mut output_ports = HashMap::new();
            output_ports.insert("send".to_string(), DanaType::Any); // Emits a list
            
            let rt_node = RuntimeNode::new_native(
                name.clone(),
                NodeIndex::new(0),
                input_ports,
                output_ports,
                Box::new(crate::stdlib::kernel::CollectorNode),
            );
            
            let idx = graph.add_node(rt_node);
            graph[idx].index = idx;
            node_map.insert(name, idx);
        }

        let name = "System.Kernel.Join".to_string();
        if !node_map.contains_key(&name) {
            let mut input_ports = HashMap::new();
            input_ports.insert("a".to_string(), DanaType::Any);
            input_ports.insert("b".to_string(), DanaType::Any);
            
            let mut output_ports = HashMap::new();
            output_ports.insert("send".to_string(), DanaType::Any); // Emits a list/pair
            
            let rt_node = RuntimeNode::new_native(
                name.clone(),
                NodeIndex::new(0),
                input_ports,
                output_ports,
                Box::new(crate::stdlib::kernel::JoinNode),
            );
            
            let idx = graph.add_node(rt_node);
            graph[idx].index = idx;
            node_map.insert(name, idx);
        }
    }

    fn add_node_to_graph(
        graph: &mut DiGraph<RuntimeNode, RuntimeEdge>, 
        node_map: &mut HashMap<String, NodeIndex>,
        ast_node: crate::ast::Node
    ) -> Result<(), BuildError> {
        if node_map.contains_key(&ast_node.name) {
            return Err(BuildError::DuplicateNode(ast_node.name.clone()));
        }

        let mut input_ports = HashMap::new();
        for port in &ast_node.input_ports {
            input_ports.insert(port.name.clone(), port.type_annotation.clone());
        }

        if input_ports.is_empty() {
            input_ports.insert("_start".to_string(), DanaType::Unit);
        }

        let mut output_ports = HashMap::new();
        for port in &ast_node.output_ports {
            output_ports.insert(port.name.clone(), port.type_annotation.clone());
        }

        let mut properties = HashMap::new();
        for prop in &ast_node.properties {
            if let Some(default_expr) = &prop.default_value {
                if let Some(val) = Value::from_literal(default_expr) {
                    properties.insert(prop.name.clone(), val);
                }
            }
        }
        for port in ast_node.input_ports.iter().chain(ast_node.output_ports.iter()) {
            if let Some(default_expr) = &port.default_value {
                if let Some(val) = Value::from_literal(default_expr) {
                    properties.insert(port.name.clone(), val);
                }
            }
        }

        let rt_node = RuntimeNode::new_dana(
            ast_node.name.clone(),
            NodeIndex::new(0),
            input_ports,
            output_ports,
            properties,
            ast_node.process.clone(),
        );

        let idx = graph.add_node(rt_node);
        graph[idx].index = idx;
        node_map.insert(ast_node.name.clone(), idx);
        Ok(())
    }

    fn validate_and_add_edge(
        graph: &mut DiGraph<RuntimeNode, RuntimeEdge>,
        node_map: &HashMap<String, NodeIndex>,
        edge: &AstEdge,
    ) -> Result<(), BuildError> {
        let source_idx = *node_map
            .get(&edge.source.node)
            .ok_or_else(|| BuildError::NodeNotFound(edge.source.node.clone()))?;

        let target_idx = *node_map
            .get(&edge.target.node)
            .ok_or_else(|| BuildError::NodeNotFound(edge.target.node.clone()))?;

        let source_node = &graph[source_idx];
        let source_type = source_node
            .output_ports
            .get(&edge.source.port)
            .or_else(|| source_node.input_ports.get(&edge.source.port)) // Check input ports too
            .ok_or_else(|| {
                BuildError::PortNotFound(edge.source.node.clone(), edge.source.port.clone())
            })?
            .clone();

        // Get target port type
        let target_node = &graph[target_idx];
        let target_type = target_node
            .input_ports
            .get(&edge.target.port)
            .ok_or_else(|| {
                BuildError::PortNotFound(edge.target.node.clone(), edge.target.port.clone())
            })?
            .clone();

        TypeChecker::validate_edge(&source_type, &target_type)?;

        let runtime_edge = RuntimeEdge {
            source_port: edge.source.port.clone(),
            target_port: edge.target.port.clone(),
            edge_type: edge.edge_type,
            guard: edge.guard.clone(),
        };

        graph.add_edge(source_idx, target_idx, runtime_edge);

        Ok(())
    }
    #[warn(dead_code)]
    pub fn get_node(&self, name: &str) -> Option<&RuntimeNode> {
        self.node_map.get(name).map(|idx| &self.graph[*idx])
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Node, Port, PortRef};

    #[test]
    fn test_build_simple_graph() {
        let mut ast = AstGraph::new();
        
        let source = Node::new("Source")
            .with_output(Port {
                name: "out".to_string(),
                type_annotation: DanaType::Int,
                default_value: None,
            });
        
        let target = Node::new("Target")
            .with_input(Port {
                name: "in".to_string(),
                type_annotation: DanaType::Int,
                default_value: None,
            });

        ast.add_node(source);
        ast.add_node(target);
        
        ast.add_edge(AstEdge {
            source: PortRef::new("Source", "out"),
            target: PortRef::new("Target", "in"),
            edge_type: EdgeType::Sync,
            guard: None,
        });

        let result = ExecutableGraph::from_ast(ast);
        assert!(result.is_ok());
        let graph = result.unwrap();
        
        assert_eq!(graph.graph.node_count(), 5); // Source + Target + 3 System nodes (IO, Collector, Join)
        assert_eq!(graph.graph.edge_count(), 1);
    }

    #[test]
    fn test_build_with_subgraphs() {
        let mut ast = AstGraph::new();
        
        let subgraph = crate::ast::GraphDef {
            name: "Main".to_string(),
            properties: Vec::new(),
            input_ports: Vec::new(),
            output_ports: Vec::new(),
            nodes: vec![
                Node::new("A").with_output(Port { name: "out".to_string(), type_annotation: DanaType::Int, default_value: None }),
                Node::new("B").with_input(Port { name: "in".to_string(), type_annotation: DanaType::Int, default_value: None }),
            ],
            edges: vec![
                AstEdge {
                    source: PortRef::new("A", "out"),
                    target: PortRef::new("B", "in"),
                    edge_type: EdgeType::Sync,
                    guard: None,
                }
            ],
        };
        
        ast.subgraphs.push(subgraph);
        
        let graph = ExecutableGraph::from_ast(ast).unwrap();
        
        println!("Node map keys: {:?}", graph.node_map.keys());
        assert_eq!(graph.graph.node_count(), 6); // A + B + 3 System nodes (IO, Collector, Join) + 1 Subgraph node
        assert_eq!(graph.graph.edge_count(), 1);
        assert!(graph.node_map.contains_key("Main.A"));
        assert!(graph.node_map.contains_key("Main.B"));
    }

    #[test]
    fn test_input_port_as_edge_source() {
        let mut ast = AstGraph::new();
        
        // NodeA(in x) -> NodeB(in y)
        let a = Node::new("NodeA").with_input(Port { name: "x".to_string(), type_annotation: DanaType::Int, default_value: None });
        let b = Node::new("NodeB").with_input(Port { name: "y".to_string(), type_annotation: DanaType::Int, default_value: None });
        
        ast.add_node(a);
        ast.add_node(b);
        ast.add_edge(AstEdge {
            source: PortRef::new("NodeA", "x"), // Input port as source!
            target: PortRef::new("NodeB", "y"),
            edge_type: EdgeType::Sync,
            guard: None,
        });

        let result = ExecutableGraph::from_ast(ast);
        assert!(result.is_ok(), "Should allow input port as edge source");
    }

    #[test]
    fn test_missing_node_error() {
        let mut ast = AstGraph::new();
        ast.add_edge(AstEdge {
            source: PortRef::new("Missing", "out"),
            target: PortRef::new("AlsoMissing", "in"),
            edge_type: EdgeType::Sync,
            guard: None,
        });

        let result = ExecutableGraph::from_ast(ast);
        assert!(matches!(result, Err(BuildError::NodeNotFound(_))));
    }

    #[test]
    fn test_missing_port_error() {
        let mut ast = AstGraph::new();
        let node = Node::new("MyNode"); // No ports
        ast.add_node(node);
        
        ast.add_edge(AstEdge {
            source: PortRef::new("MyNode", "missing_port"),
            target: PortRef::new("MyNode", "missing_port"),
            edge_type: EdgeType::Sync,
            guard: None,
        });

        let result = ExecutableGraph::from_ast(ast);
        assert!(matches!(result, Err(BuildError::PortNotFound(_, _))));
    }

    #[test]
    fn test_stdout_accepts_any() {
        let mut ast = AstGraph::new();
        let node = Node::new("Source").with_output(Port { name: "out".to_string(), type_annotation: DanaType::Int, default_value: None });
        ast.add_node(node);
        ast.add_edge(AstEdge {
            source: PortRef::new("Source", "out"),
            target: PortRef::new("System.IO", "stdout"),
            edge_type: EdgeType::Sync,
            guard: None,
        });

        let result = ExecutableGraph::from_ast(ast);
        assert!(result.is_ok(), "stdout should accept Int via Any");
    }

    #[test]
    fn test_type_mismatch_error() {
        let mut ast = AstGraph::new();
        
        let source = Node::new("Source")
            .with_output(Port {
                name: "out".to_string(),
                type_annotation: DanaType::String,
                default_value: None,
            });
        
        let target = Node::new("Target")
            .with_input(Port {
                name: "in".to_string(),
                type_annotation: DanaType::Int,
                default_value: None,
            });

        ast.add_node(source);
        ast.add_node(target);
        
        ast.add_edge(AstEdge {
            source: PortRef::new("Source", "out"),
            target: PortRef::new("Target", "in"),
            edge_type: EdgeType::Sync,
            guard: None,
        });

        let result = ExecutableGraph::from_ast(ast);
        assert!(matches!(result, Err(BuildError::TypeError(_))));
    }
}
