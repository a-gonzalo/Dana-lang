/// Parser for Dana language using Pest
///
/// Converts Dana source code into an Abstract Syntax Tree (AST)

use crate::ast::*;
use crate::types::DanaType;
use pest::Parser;
use pest_derive::Parser;
use thiserror::Error;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct DanaParser;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Pest parsing error: {0}")]
    PestError(#[from] pest::error::Error<Rule>),
    
    #[error("Invalid syntax at: {0}")]
    InvalidSyntax(String),
    
    #[error("Unexpected rule: expected {expected}, got {actual}")]
    UnexpectedRule { expected: String, actual: String },
}

type ParseResult<T> = Result<T, ParseError>;

/// Parse a Dana source file into a Graph AST
pub fn parse_file(input: &str) -> ParseResult<Graph> {
    let pairs = DanaParser::parse(Rule::program, input)?;
    let mut graph = Graph::new();

    for pair in pairs {
        match pair.as_rule() {
            Rule::program => {
                for item in pair.into_inner() {
                    match item.as_rule() {
                        Rule::item => {
                            // Unwrap the item to get the actual node_decl or edge_decl
                            let inner = item.into_inner().next().ok_or_else(|| {
                                ParseError::InvalidSyntax("Empty item".to_string())
                            })?;
                            
                            match inner.as_rule() {
                                Rule::node_decl => {
                                    let node = parse_node(inner)?;
                                    graph.add_node(node);
                                }
                                Rule::edge_decl => {
                                    let edge = parse_edge(inner)?;
                                    graph.add_edge(edge);
                                }
                                _ => {
                                    return Err(ParseError::InvalidSyntax(format!(
                                        "Unexpected item content: {:?}",
                                        inner.as_rule()
                                    )))
                                }
                            }
                        }
                        Rule::EOI => {}
                        _ => {
                            return Err(ParseError::InvalidSyntax(format!(
                                "Unexpected program content: {:?}",
                                item.as_rule()
                            )))
                        }
                    }
                }
            }
            Rule::EOI => {}
            _ => {
                return Err(ParseError::InvalidSyntax(format!(
                    "Expected program, got {:?}",
                    pair.as_rule()
                )))
            }
        }
    }

    Ok(graph)
}

fn parse_node(pair: pest::iterators::Pair<Rule>) -> ParseResult<Node> {
    // Clone the pair so we can iterate twice
    let pair_clone = pair.clone();
    
    let mut inner = pair.into_inner();
    
    // Skip KW_NODE token and get the identifier
    let name = inner
        .find(|p| p.as_rule() == Rule::identifier)
        .ok_or_else(|| ParseError::InvalidSyntax("Missing node name".to_string()))?
        .as_str()
        .to_string();

    let mut node = Node::new(name);

    // First pass to get process block
    for item in inner {
        match item.as_rule() {
            Rule::property_decl => {
                let prop = parse_property(item)?;
                node.properties.push(prop);
            }
            Rule::process_block => {
                let process = parse_process_block(item)?;
                node.process = Some(process);
            }
            _ => {}
        }
    }

    // Second pass  with context awareness for ports
    let mut inner = pair_clone.into_inner();
    let _name = inner.next(); // Skip name, already parsed
    
    let mut parsing_inputs = false;
    let mut parsing_outputs = false;

    for item in inner {
        match item.as_rule() {
            Rule::KW_IN => {
                parsing_inputs = true;
                parsing_outputs = false;
            }
            Rule::KW_OUT => {
                parsing_inputs = false;
                parsing_outputs = true;
            }
            Rule::port_decl => {
                let port = parse_port(item)?;
                if parsing_inputs {
                    node.input_ports.push(port);
                } else if parsing_outputs {
                    node.output_ports.push(port);
                }
            }
            _ => {}
        }
    }

    Ok(node)
}

fn parse_property(pair: pest::iterators::Pair<Rule>) -> ParseResult<Property> {
    let mut inner = pair.into_inner();
    
    let name = inner
        .next()
        .ok_or_else(|| ParseError::InvalidSyntax("Missing property name".to_string()))?
        .as_str()
        .to_string();
    
    let type_annotation = parse_type(
        inner
            .next()
            .ok_or_else(|| ParseError::InvalidSyntax("Missing property type".to_string()))?,
    )?;
    
    let default_value = inner.next().map(parse_expression).transpose()?;

    Ok(Property {
        name,
        type_annotation,
        default_value,
    })
}

fn parse_port(pair: pest::iterators::Pair<Rule>) -> ParseResult<Port> {
    let mut inner = pair.into_inner();
    
    let name = inner
        .next()
        .ok_or_else(|| ParseError::InvalidSyntax("Missing port name".to_string()))?
        .as_str()
        .to_string();
    
    let type_annotation = parse_type(
        inner
            .next()
            .ok_or_else(|| ParseError::InvalidSyntax("Missing port type".to_string()))?,
    )?;

    Ok(Port {
        name,
        type_annotation,
    })
}

fn parse_type(pair: pest::iterators::Pair<Rule>) -> ParseResult<DanaType> {
    let inner = pair.into_inner().next().ok_or_else(|| {
        ParseError::InvalidSyntax("Missing type specification".to_string())
    })?;

    match inner.as_rule() {
        Rule::type_int => Ok(DanaType::Int),
        Rule::type_float => Ok(DanaType::Float),
        Rule::type_string => Ok(DanaType::String),
        Rule::type_bool => Ok(DanaType::Bool),
        Rule::type_byte => Ok(DanaType::Byte),
        Rule::type_unit => Ok(DanaType::Unit),
        Rule::type_stream => {
            let inner_type = parse_type(
                inner
                    .into_inner()
                    .next()
                    .ok_or_else(|| ParseError::InvalidSyntax("Missing stream type".to_string()))?,
            )?;
            Ok(DanaType::Stream(Box::new(inner_type)))
        }
        _ => Err(ParseError::InvalidSyntax(format!(
            "Unknown type: {:?}",
            inner.as_str()
        ))),
    }
}

fn parse_process_block(pair: pest::iterators::Pair<Rule>) -> ParseResult<ProcessBlock> {
    let inner = pair.into_inner();
    
    // Parse parameter list
    let mut triggers = Vec::new();
    let mut statements = Vec::new();
    
    for item in inner {
        match item.as_rule() {
            Rule::param_list => {
                for param in item.into_inner() {
                    triggers.push(param.as_str().to_string());
                }
            }
            Rule::statement => {
                statements.push(parse_statement(item)?);
            }
            _ => {}
        }
    }

    Ok(ProcessBlock {
        triggers,
        statements,
    })
}

fn parse_statement(pair: pest::iterators::Pair<Rule>) -> ParseResult<Statement> {
    let inner = pair.into_inner().next().ok_or_else(|| {
        ParseError::InvalidSyntax("Empty statement".to_string())
    })?;

    match inner.as_rule() {
        Rule::emit_stmt => {
            let mut parts = inner.into_inner();
            let port = parts
                .find(|p| p.as_rule() == Rule::identifier)
                .ok_or_else(|| ParseError::InvalidSyntax("Missing emit port".to_string()))?
                .as_str()
                .to_string();
            // After finding identifier, next rule should be expression (skipping parenthesis tokens if any, though usually punctuation isn't a rule unless named)
            // Wait, expression is a rule. Punctuation like "(" is not a rule in my grammar (it's string literal in rule def).
            // So next rule is expression.
            let value = parse_expression(parts.next().ok_or_else(|| {
                ParseError::InvalidSyntax("Missing emit value".to_string())
            })?)?;

            Ok(Statement::Emit { port, value })
        }
        Rule::let_stmt => {
            let mut parts = inner.into_inner();
            let name = parts
                .find(|p| p.as_rule() == Rule::identifier)
                .ok_or_else(|| ParseError::InvalidSyntax("Missing let binding name".to_string()))?
                .as_str()
                .to_string();
            let value = parse_expression(parts.next().ok_or_else(|| {
                ParseError::InvalidSyntax("Missing let binding value".to_string())
            })?)?;

            Ok(Statement::Let { name, value })
        }
        Rule::expr_stmt => {
             Ok(Statement::Expression(parse_expression(inner.into_inner().next().unwrap())?))
        }
        _ => Err(ParseError::InvalidSyntax(format!(
            "Unknown statement rule: {:?}",
            inner.as_rule()
        ))),
    }
}

fn parse_expression(pair: pest::iterators::Pair<Rule>) -> ParseResult<Expression> {
    parse_precedence_expr(pair)
}

fn parse_precedence_expr(pair: pest::iterators::Pair<Rule>) -> ParseResult<Expression> {
    match pair.as_rule() {
        Rule::expression
        | Rule::logical_or_expr
        | Rule::logical_and_expr
        | Rule::comparison_expr
        | Rule::add_expr
        | Rule::mult_expr
        | Rule::unary_expr
        | Rule::postfix_expr => {
            let mut inner = pair.into_inner();
            let first = inner.next().ok_or_else(|| {
                ParseError::InvalidSyntax("Empty expression".to_string())
            })?;
            
            let mut expr = parse_precedence_expr(first)?;
            
            // Handle binary operators
            while let Some(op_pair) = inner.next() {
                let op_str = op_pair.as_str();
                let right = parse_precedence_expr(inner.next().ok_or_else(|| {
                    ParseError::InvalidSyntax("Missing right operand".to_string())
                })?)?;
                
                let op = match op_str {
                    "+" => BinaryOperator::Add,
                    "-" => BinaryOperator::Subtract,
                    "*" => BinaryOperator::Multiply,
                    "/" => BinaryOperator::Divide,
                    "==" => BinaryOperator::Equal,
                    "!=" => BinaryOperator::NotEqual,
                    ">" => BinaryOperator::GreaterThan,
                    "<" => BinaryOperator::LessThan,
                    ">=" => BinaryOperator::GreaterEqual,
                    "<=" => BinaryOperator::LessEqual,
                    "&&" => BinaryOperator::And,
                    "||" => BinaryOperator::Or,
                    _ => {
                        return Err(ParseError::InvalidSyntax(format!(
                            "Unknown binary operator: {}",
                            op_str
                        )))
                    }
                };
                
                expr = Expression::BinaryOp {
                    op,
                    left: Box::new(expr),
                    right: Box::new(right),
                };
            }
            
            Ok(expr)
        }
        Rule::primary_expr => {
            let inner = pair.into_inner().next().ok_or_else(|| {
                ParseError::InvalidSyntax("Empty primary expression".to_string())
            })?;
            parse_precedence_expr(inner)
        }
        Rule::int_literal => Ok(Expression::IntLiteral(pair.as_str().parse().map_err(
            |_| ParseError::InvalidSyntax(format!("Invalid integer: {}", pair.as_str())),
        )?)),
        Rule::float_literal => Ok(Expression::FloatLiteral(pair.as_str().parse().map_err(
            |_| ParseError::InvalidSyntax(format!("Invalid float: {}", pair.as_str())),
        )?)),
        Rule::string_literal => {
            let s = pair.as_str();
            // Simplify parsing by just stripping quotes. 
            // Proper unescaping would go here.
            let content = &s[1..s.len()-1];
            Ok(Expression::StringLiteral(content.to_string()))
        }
        Rule::bool_literal => {
            let val = pair.as_str() == "true";
            Ok(Expression::BoolLiteral(val))
        }
        Rule::identifier => Ok(Expression::Identifier(pair.as_str().to_string())),
        Rule::lambda_expr => {
            let mut inner = pair.into_inner();
            let mut params = Vec::new();
            
            // Parse parameters
            if let Some(param_list) = inner.next() {
                if param_list.as_rule() == Rule::param_list {
                    for param in param_list.into_inner() {
                        params.push(param.as_str().to_string());
                    }
                }
            }
            
            // Parse body
            let body = parse_expression(inner.next().ok_or_else(|| {
                ParseError::InvalidSyntax("Missing lambda_body".to_string())
            })?)?;
            
            Ok(Expression::Lambda {
                params,
                body: Box::new(body),
            })
        }
        _ => Err(ParseError::InvalidSyntax(format!(
            "Unexpected expression rule: {:?}",
            pair.as_rule()
        ))),
    }
}

fn parse_edge(pair: pest::iterators::Pair<Rule>) -> ParseResult<Edge> {
    let mut inner = pair.into_inner();
    
    let source = parse_port_ref(inner.next().ok_or_else(|| {
        ParseError::InvalidSyntax("Missing edge source".to_string())
    })?)?;
    
    let mut guard = None;
    let mut edge_type = EdgeType::Sync;
    
    // Parse optional guard and edge operator
    for item in inner.clone() {
        match item.as_rule() {
            Rule::guard => {
                guard = Some(parse_guard(item)?);
            }
            Rule::edge_op => {
                let op = item.into_inner().next().ok_or_else(|| {
                    ParseError::InvalidSyntax("Missing edge operator".to_string())
                })?;
                edge_type = match op.as_rule() {
                    Rule::edge_sync => EdgeType::Sync,
                    Rule::edge_async => EdgeType::Async,
                    _ => EdgeType::Sync,
                };
            }
            _ => {}
        }
    }
    
    // Get target (last port_ref)
    let target = parse_port_ref(inner.last().ok_or_else(|| {
        ParseError::InvalidSyntax("Missing edge target".to_string())
    })?)?;

    Ok(Edge {
        source,
        target,
        edge_type,
        guard,
    })
}

fn parse_port_ref(pair: pest::iterators::Pair<Rule>) -> ParseResult<PortRef> {
    let mut parts: Vec<String> = pair
        .into_inner()
        .map(|p| p.as_str().to_string())
        .collect();

    if parts.len() < 2 {
        return Err(ParseError::InvalidSyntax("Port reference must have at least Node.Port".to_string()));
    }

    let port = parts.pop().unwrap(); // Last one is port
    let node = parts.join("."); // Rest is node name

    Ok(PortRef::new(node, port))
}

fn parse_guard(pair: pest::iterators::Pair<Rule>) -> ParseResult<Guard> {
    let condition = parse_expression(pair.into_inner().next().ok_or_else(|| {
        ParseError::InvalidSyntax("Empty guard condition".to_string())
    })?)?;

    Ok(Guard { condition })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_node() {
        let input = r#"
            node TestNode {
                count: Int = 0
            }
        "#;
        
        let result = parse_file(input);
        assert!(result.is_ok());
        let graph = result.unwrap();
        assert_eq!(graph.nodes.len(), 1);
        assert_eq!(graph.nodes[0].name, "TestNode");
    }

    #[test]
    fn test_parse_node_with_ports() {
        let input = r#"
            node Counter {
                current: Int = 0
                in increment: Unit
                out value: Int
            }
        "#;
        
        let result = parse_file(input);
        if let Err(e) = &result {
            eprintln!("Parse error: {}", e);
        }
        assert!(result.is_ok());
        let graph = result.unwrap();
        let node = &graph.nodes[0];
        assert_eq!(node.input_ports.len(), 1);
        assert_eq!(node.output_ports.len(), 1);
    }

    #[test]
    fn test_parse_edge() {
        let input = r#"
            node A {
                out x: Int
            }
            node B {
                in y: Int
            }
            A.x -> B.y
        "#;
        
        let result = parse_file(input);
        assert!(result.is_ok());
        let graph = result.unwrap();
        assert_eq!(graph.edges.len(), 1);
        assert_eq!(graph.edges[0].source.node, "A");
        assert_eq!(graph.edges[0].target.node, "B");
    }
}
