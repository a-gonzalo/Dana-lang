/// Abstract Syntax Tree (AST) definitions for Dana language
/// 
/// This module defines the structure of Dana programs after parsing.

use crate::types::DanaType;
use serde::{Deserialize, Serialize};

/// Definition of a custom type (struct)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeDef {
    pub name: String,
    pub fields: Vec<(String, DanaType)>,
}

/// A complete Dana program (collection of graphs and types)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Graph { // Renaming to Program would be better, but keeping Graph for now as top container
    pub nodes: Vec<Node>, // Global nodes (legacy v0.1 support or top-level)
    pub edges: Vec<Edge>, // Global edges (legacy v0.1)
    pub types: Vec<TypeDef>,
    pub subgraphs: Vec<GraphDef>, // New: Defined subgraphs (including Main)
}

/// A graph definition (composite node)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GraphDef {
    pub name: String,
    pub properties: Vec<Property>,
    pub input_ports: Vec<Port>,
    pub output_ports: Vec<Port>,
    // Components inside the graph (declared inline)
    pub nodes: Vec<Node>,
    pub edges: Vec<Edge>,
}

/// A node in the graph
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Node {
    pub name: String,
    pub properties: Vec<Property>,
    pub input_ports: Vec<Port>,
    pub output_ports: Vec<Port>,
    pub process: Option<ProcessBlock>,
}

/// A property (state) within a node
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Property {
    pub name: String,
    pub type_annotation: DanaType,
    pub default_value: Option<Expression>,
}

/// An input or output port
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Port {
    pub name: String,
    pub type_annotation: DanaType,
    pub default_value: Option<Expression>,
}

/// Process logic block
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProcessBlock {
    /// Input parameters that trigger this process
    pub triggers: Vec<String>,
    /// Statements in the process body
    pub statements: Vec<Statement>,
}

/// A pattern in a match expression
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Pattern {
    /// Literal value: 42, "hello", true
    Literal(Expression),
    /// Wildcard: _ (matches anything)
    Wildcard,
    /// Variable binding: captures value into identifier
    Binding(String),
    /// Tuple pattern: (a, b, c)
    Tuple(Vec<Pattern>),
}

/// A single arm in a match statement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MatchArm {
    /// The pattern to match against
    pub pattern: Pattern,
    /// Optional guard condition: if expr
    pub guard: Option<Expression>,
    /// Statements to execute if pattern matches
    pub body: Vec<Statement>,
}

/// A statement within a process block
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Statement {
    /// Emit data to an output port: emit portName(value)
    Emit {
        port: String,
        value: Expression,
    },
    /// Variable binding: let x = expr
    Let {
        name: String,
        value: Expression,
    },
    /// Match statement: match expr { arms... }
    Match {
        expression: Box<Expression>,
        arms: Vec<MatchArm>,
    },
    /// Expression statement
    Expression(Expression),
}

/// Expressions in Dana
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Expression {
    /// Integer literal
    IntLiteral(i64),
    /// Float literal
    FloatLiteral(f64),
    /// String literal
    StringLiteral(String),
    /// Boolean literal
    BoolLiteral(bool),
    /// Identifier (variable/property reference)
    Identifier(String),
    /// Binary operation
    BinaryOp {
        op: BinaryOperator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    /// Unary operation
    UnaryOp {
        op: UnaryOperator,
        operand: Box<Expression>,
    },
    /// Function call
    Call {
        function: String,
        args: Vec<Expression>,
    },
    /// Lambda expression: (x => x * 2)
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

/// An edge connecting two nodes
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Edge {
    pub source: PortRef,
    pub target: PortRef,
    pub edge_type: EdgeType,
    pub guard: Option<Guard>,
}

/// Reference to a port on a node
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

/// Type of edge
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum EdgeType {
    /// Synchronous edge (->)
    Sync,
    /// Asynchronous edge (~>)
    Async,
}

/// A guard condition on an edge
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Guard {
    pub condition: Expression,
}

/// Inline lambda node for transformations
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
