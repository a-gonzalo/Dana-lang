//! AST types used by parser and runtime.
//!
//! Compact, serde-serializable representation of parsed Dana programs: Graph, Node, Edge, Port, Types, Expressions, and Statements.
//! Invariants: edges reference nodes/ports by name; default values are simple literal expressions.

use crate::types::DanaType;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeDef {
    pub name: String,
    pub fields: Vec<(String, DanaType)>,
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Graph {
    pub nodes: Vec<Node>,
    pub edges: Vec<Edge>,
    pub types: Vec<TypeDef>,
    pub subgraphs: Vec<GraphDef>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GraphDef {
    pub name: String,
    pub properties: Vec<Property>,
    pub input_ports: Vec<Port>,
    pub output_ports: Vec<Port>,
    pub nodes: Vec<Node>,
    pub edges: Vec<Edge>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Node {
    pub name: String,
    pub properties: Vec<Property>,
    pub input_ports: Vec<Port>,
    pub output_ports: Vec<Port>,
    pub process: Option<ProcessBlock>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Property {
    pub name: String,
    pub type_annotation: DanaType,
    pub default_value: Option<Expression>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Port {
    pub name: String,
    pub type_annotation: DanaType,
    pub default_value: Option<Expression>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProcessBlock {
    pub triggers: Vec<String>,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Pattern {
    Literal(Expression),
    Wildcard,
    Binding(String),
    Tuple(Vec<Pattern>),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<Expression>,
    pub body: Vec<Statement>,
}


#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Statement {
    Emit {
        port: String,
        value: Expression,
    },
    Let {
        name: String,
        value: Expression,
    },
    Match {
        expression: Box<Expression>,
        arms: Vec<MatchArm>,
    },
    Expression(Expression),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Expression {
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    BoolLiteral(bool),
    Identifier(String),
    BinaryOp {
        op: BinaryOperator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    UnaryOp {
        op: UnaryOperator,
        operand: Box<Expression>,
    },
    Call {
        function: String,
        args: Vec<Expression>,
    },
    Lambda {
        params: Vec<String>,
        body: Box<Expression>,
    },
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    GreaterThan,
    LessThan,
    GreaterEqual,
    LessEqual,
    And,
    Or,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum UnaryOperator {
    Negate,
    Not,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Edge {
    pub source: PortRef,
    pub target: PortRef,
    pub edge_type: EdgeType,
    pub guard: Option<Guard>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PortRef {
    pub node: String,
    pub port: String,
}

impl PortRef {
    pub fn new(node: impl Into<String>, port: impl Into<String>) -> Self {
        Self {
            node: node.into(),
            port: port.into(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum EdgeType {
    /// Synchronous edge (->)
    Sync,
    /// Asynchronous edge (~>)
    Async,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Guard {
    pub condition: Expression,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LambdaNode {
    pub params: Vec<String>,
    pub body: Expression,
}

impl Graph {
    pub fn new() -> Self {
        Self {
            nodes: Vec::new(),
            edges: Vec::new(),
            types: Vec::new(),
            subgraphs: Vec::new(),
        }
    }

    pub fn add_type(&mut self, type_def: TypeDef) {
        self.types.push(type_def);
    }

    pub fn add_node(&mut self, node: Node) {
        self.nodes.push(node);
    }

    pub fn add_edge(&mut self, edge: Edge) {
        self.edges.push(edge);
    }

    pub fn find_node(&self, name: &str) -> Option<&Node> {
        self.nodes.iter().find(|n| n.name == name)
    }
}

impl Node {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            properties: Vec::new(),
            input_ports: Vec::new(),
            output_ports: Vec::new(),
            process: None,
        }
    }

    pub fn with_property(mut self, prop: Property) -> Self {
        self.properties.push(prop);
        self
    }

    pub fn with_input(mut self, port: Port) -> Self {
        self.input_ports.push(port);
        self
    }

    pub fn with_output(mut self, port: Port) -> Self {
        self.output_ports.push(port);
        self
    }

    pub fn with_process(mut self, process: ProcessBlock) -> Self {
        self.process = Some(process);
        self
    }

    pub fn find_input_port(&self, name: &str) -> Option<&Port> {
        self.input_ports.iter().find(|p| p.name == name)
    }

    pub fn find_output_port(&self, name: &str) -> Option<&Port> {
        self.output_ports.iter().find(|p| p.name == name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_graph_creation() {
        let mut graph = Graph::new();
        let node = Node::new("TestNode");
        graph.add_node(node);

        assert_eq!(graph.nodes.len(), 1);
        assert_eq!(graph.nodes[0].name, "TestNode");
    }

    #[test]
    fn test_node_builder() {
        let node = Node::new("Counter")
            .with_property(Property {
                name: "count".to_string(),
                type_annotation: DanaType::Int,
                default_value: Some(Expression::IntLiteral(0)),
            })
            .with_input(Port {
                name: "increment".to_string(),
                type_annotation: DanaType::Unit,
                default_value: None,
            })
            .with_output(Port {
                name: "current".to_string(),
                type_annotation: DanaType::Int,
                default_value: None,
            });

        assert_eq!(node.name, "Counter");
        assert_eq!(node.properties.len(), 1);
        assert_eq!(node.input_ports.len(), 1);
        assert_eq!(node.output_ports.len(), 1);
    }

    #[test]
    fn test_port_ref() {
        let port_ref = PortRef::new("NodeA", "output");
        assert_eq!(port_ref.node, "NodeA");
        assert_eq!(port_ref.port, "output");
    }

    #[test]
    fn test_edge_creation() {
        let edge = Edge {
            source: PortRef::new("Source", "out"),
            target: PortRef::new("Target", "in"),
            edge_type: EdgeType::Sync,
            guard: None,
        };

        assert_eq!(edge.source.node, "Source");
        assert_eq!(edge.target.node, "Target");
        assert_eq!(edge.edge_type, EdgeType::Sync);
    }
}
