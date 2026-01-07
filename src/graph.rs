use crate::ast::{Graph as AstGraph, Edge as AstEdge, EdgeType, PortRef, Guard};
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

/// Runtime representation of an edge
#[derive(Debug, Clone)]
pub struct RuntimeEdge {
    pub source_port: String,
    pub target_port: String,
    pub edge_type: EdgeType,
    pub guard: Option<Guard>,
}

/// The executable graph structure
pub struct ExecutableGraph {
    pub graph: DiGraph<RuntimeNode, RuntimeEdge>,
    pub node_map: HashMap<String, NodeIndex>,
}

impl ExecutableGraph {
    /// Build an executable graph from an AST
    pub fn from_ast(ast: AstGraph) -> Result<Self, BuildError> {
        let mut graph = DiGraph::new();
        let mut node_map = HashMap::new();

        // 0. Pre-register Standard Library Nodes
        Self::add_system_nodes(&mut graph, &mut node_map);

        // 1. Add top-level nodes
        for ast_node in ast.nodes {
            Self::add_node_to_graph(&mut graph, &mut node_map, ast_node)?;
        }

        // 2. Add subgraph nodes (Flattening)
        for subgraph in &ast.subgraphs {
            for ast_node in &subgraph.nodes {
                // For now, we flatten the names. TODO: Support scoped names
                Self::add_node_to_graph(&mut graph, &mut node_map, ast_node.clone())?;
            }
        }

        // 3. Add top-level edges
        for ast_edge in ast.edges {
            Self::validate_and_add_edge(&mut graph, &node_map, &ast_edge)?;
        }

        // 4. Add subgraph edges
        for subgraph in &ast.subgraphs {
            for ast_edge in &subgraph.edges {
                Self::validate_and_add_edge(&mut graph, &node_map, ast_edge)?;
            }
        }

        Ok(Self { graph, node_map })
    }

    fn add_system_nodes(graph: &mut DiGraph<RuntimeNode, RuntimeEdge>, node_map: &mut HashMap<String, NodeIndex>) {
        // System.IO
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

        // System.Kernel.Collector
        let name = "System.Kernel.Collector".to_string();
        if !node_map.contains_key(&name) {
            let mut input_ports = HashMap::new();
            input_ports.insert("in".to_string(), DanaType::Any);
            input_ports.insert("reset".to_string(), DanaType::Unit);
            
            let mut output_ports = HashMap::new();
            output_ports.insert("out".to_string(), DanaType::Any); // Emits a list
            
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

        // System.Kernel.Join
        let name = "System.Kernel.Join".to_string();
        if !node_map.contains_key(&name) {
            let mut input_ports = HashMap::new();
            input_ports.insert("a".to_string(), DanaType::Any);
            input_ports.insert("b".to_string(), DanaType::Any);
            
            let mut output_ports = HashMap::new();
            output_ports.insert("out".to_string(), DanaType::Any); // Emits a list/pair
            
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

        // Add implicit '_start' port for nodes with 0 inputs (source nodes)
        if input_ports.is_empty() {
            input_ports.insert("_start".to_string(), DanaType::Unit);
        }

        let mut output_ports = HashMap::new();
        for port in &ast_node.output_ports {
            output_ports.insert(port.name.clone(), port.type_annotation.clone());
        }

        let mut properties = HashMap::new();
        // 1. Existing properties
        for prop in &ast_node.properties {
            if let Some(default_expr) = &prop.default_value {
                if let Some(val) = Value::from_literal(default_expr) {
                    properties.insert(prop.name.clone(), val);
                }
            }
        }
        // 2. Default values from Ports (Shorthand)
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
        // Find source node
        let source_idx = *node_map
            .get(&edge.source.node)
            .ok_or_else(|| BuildError::NodeNotFound(edge.source.node.clone()))?;

        // Find target node
        let target_idx = *node_map
            .get(&edge.target.node)
            .ok_or_else(|| BuildError::NodeNotFound(edge.target.node.clone()))?;

        // Get source port type
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

        // Validate type compatibility
        TypeChecker::validate_edge(&source_type, &target_type)?;

        // Add edge to graph
        let runtime_edge = RuntimeEdge {
            source_port: edge.source.port.clone(),
            target_port: edge.target.port.clone(),
            edge_type: edge.edge_type,
            guard: edge.guard.clone(),
        };

        graph.add_edge(source_idx, target_idx, runtime_edge);

        Ok(())
    }

    /// Get a node by name
    pub fn get_node(&self, name: &str) -> Option<&RuntimeNode> {
        self.node_map.get(name).map(|idx| &self.graph[*idx])
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Node, Port};

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
        
        assert_eq!(graph.graph.node_count(), 3); // Source + Target + System.IO
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
        
        assert_eq!(graph.graph.node_count(), 3); // A + B + System.IO
        assert_eq!(graph.graph.edge_count(), 1);
        assert!(graph.node_map.contains_key("A"));
        assert!(graph.node_map.contains_key("B"));
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
        // Add edge referencing missing nodes
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
                type_annotation: DanaType::String, // String output
                default_value: None,
            });
        
        let target = Node::new("Target")
            .with_input(Port {
                name: "in".to_string(),
                type_annotation: DanaType::Int, // Int input
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
