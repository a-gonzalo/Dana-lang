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

        // Pre-register Standard Library Nodes
        
        // System.IO
        {
            let name = "System.IO".to_string();
            if !node_map.contains_key(&name) {
                let mut input_ports = HashMap::new();
                input_ports.insert("stdout".to_string(), DanaType::String); // Accepting String (auto-converted)
                
                let output_ports = HashMap::new();
                // input_ports.insert("stdin".to_string(), DanaType::String); 
                
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
        }

        // 1. Add all nodes to the graph
        for ast_node in ast.nodes {
            if node_map.contains_key(&ast_node.name) {
                return Err(BuildError::DuplicateNode(ast_node.name.clone()));
            }

            let mut input_ports = HashMap::new();
            for port in &ast_node.input_ports {
                input_ports.insert(port.name.clone(), port.type_annotation.clone());
            }

            let mut output_ports = HashMap::new();
            for port in &ast_node.output_ports {
                output_ports.insert(port.name.clone(), port.type_annotation.clone());
            }

            // Initialize properties with default values
            let mut properties = HashMap::new();
            for prop in &ast_node.properties {
                if let Some(default_expr) = &prop.default_value {
                    // Evaluate simple literals immediately
                    if let Some(val) = Value::from_literal(default_expr) {
                        properties.insert(prop.name.clone(), val);
                    }
                }
            }

            let rt_node = RuntimeNode::new_dana(
                ast_node.name.clone(),
                NodeIndex::new(0), // Placeholder
                input_ports,
                output_ports,
                properties,
                ast_node.process.clone(),
            );

            let idx = graph.add_node(rt_node);
            graph[idx].index = idx; // Update self-reference index
            node_map.insert(ast_node.name.clone(), idx);
        }

        // 2. Add edges and validate connections
        for ast_edge in ast.edges {
            Self::validate_and_add_edge(&mut graph, &node_map, &ast_edge)?;
        }

        Ok(Self { graph, node_map })
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
            });
        
        let target = Node::new("Target")
            .with_input(Port {
                name: "in".to_string(),
                type_annotation: DanaType::Int,
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
    fn test_type_mismatch_error() {
        let mut ast = AstGraph::new();
        
        let source = Node::new("Source")
            .with_output(Port {
                name: "out".to_string(),
                type_annotation: DanaType::String, // String output
            });
        
        let target = Node::new("Target")
            .with_input(Port {
                name: "in".to_string(),
                type_annotation: DanaType::Int, // Int input
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
