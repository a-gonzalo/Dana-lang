/// Parser for Dana language using Pest
///
/// Converts Dana source code into an Abstract Syntax Tree (AST)

use crate::ast::*;
use crate::types::DanaType;
use crate::verbose;
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

pub fn parse_file(input: &str) -> ParseResult<Graph> {
    let pairs = DanaParser::parse(Rule::program, input)?;
    let mut graph = Graph::new();

    for pair in pairs {
        match pair.as_rule() {
            Rule::program => {
                for item in pair.into_inner() {
                    match item.as_rule() {
                        Rule::item => {
                            let inner = item.into_inner().next().ok_or_else(|| {
                                ParseError::InvalidSyntax("Empty item".to_string())
                            })?;
                            
                            match inner.as_rule() {
                                Rule::graph_decl => {
                                    let graph_def = parse_graph(inner)?;
                                    graph.subgraphs.push(graph_def);
                                }
                                Rule::type_decl => {
                                    let struct_def = parse_types(inner)?;
                                    graph.add_type(struct_def);
                                }
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
    let pair_clone = pair.clone();
    
    let mut inner = pair.into_inner();
    
    let name = inner
        .find(|p| p.as_rule() == Rule::identifier)
        .ok_or_else(|| ParseError::InvalidSyntax("Missing node name".to_string()))?
        .as_str()
        .to_string();

    let mut node = Node::new(name);

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

    let default_value = inner.next().map(parse_expression).transpose()?;

    Ok(Port {
        name,
        type_annotation,
        default_value,
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
        Rule::type_any => Ok(DanaType::Any),
        Rule::type_stream => {
            let inner_type = parse_type(
                inner
                    .into_inner()
                    .next()
                    .ok_or_else(|| ParseError::InvalidSyntax("Missing stream type".to_string()))?,
            )?;
            Ok(DanaType::Stream(Box::new(inner_type)))
        }
        Rule::identifier => {
             Ok(DanaType::Type(inner.as_str().to_string(), Vec::new()))
        }
        _ => Err(ParseError::InvalidSyntax(format!(
            "Unknown type: {:?}",
            inner.as_str()
        ))),
    }
}

fn parse_types(pair: pest::iterators::Pair<Rule>) -> ParseResult<TypeDef> {
    let mut inner = pair.into_inner();
    
    let name = inner
        .find(|p| p.as_rule() == Rule::identifier)
        .ok_or_else(|| ParseError::InvalidSyntax("Missing struct name".to_string()))?
        .as_str()
        .to_string();
        
    let mut fields = Vec::new();
    
    for item in inner {
        match item.as_rule() {
            Rule::struct_field => {
                let mut field_parts = item.into_inner();
                let field_name = field_parts.next().unwrap().as_str().to_string();
                let field_type = parse_type(field_parts.next().unwrap())?;
                fields.push((field_name, field_type));
            }
            _ => {}
        }
    }
    
    Ok(TypeDef { name, fields })
}

fn parse_graph(pair: pest::iterators::Pair<Rule>) -> ParseResult<GraphDef> {
    let pair_clone = pair.clone();
    let mut inner = pair.into_inner();
    
    let name = inner
        .find(|p| p.as_rule() == Rule::identifier)
        .ok_or_else(|| ParseError::InvalidSyntax("Missing graph name".to_string()))?
        .as_str()
        .to_string();
    
    let mut properties = Vec::new();
    let mut input_ports = Vec::new();
    let mut output_ports = Vec::new();
    let mut nodes = Vec::new(); 
    let mut edges = Vec::new();

    let mut parsing_inputs = false;
    let mut parsing_outputs = false;
    
    let mut inner = pair_clone.into_inner();
    inner.next(); // Skip name
    
    for item in inner {
        match item.as_rule() {
            Rule::property_decl => {
                properties.push(parse_property(item)?);
            }
            Rule::KW_IN => { parsing_inputs = true; parsing_outputs = false; }
            Rule::KW_OUT => { parsing_inputs = false; parsing_outputs = true; }
            Rule::port_decl => {
                let port = parse_port(item)?;
                if parsing_inputs { input_ports.push(port); }
                else if parsing_outputs { output_ports.push(port); }
            }
            Rule::node_decl => {
                let node = parse_node(item)?;
                nodes.push(node);
            }
            Rule::edge_decl => {
                edges.push(parse_edge(item)?);
            }
            _ => {}
        }
    }

    Ok(GraphDef {
        name,
        properties,
        input_ports,
        output_ports,
        nodes,
        edges,
    })
}

fn parse_process_block(pair: pest::iterators::Pair<Rule>) -> ParseResult<ProcessBlock> {
    let inner = pair.into_inner();
    
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

fn parse_emit_stmt_direct(pair: pest::iterators::Pair<Rule>) -> ParseResult<Statement> {
    let mut parts = pair.into_inner();
    let port = parts
        .find(|p| p.as_rule() == Rule::identifier)
        .ok_or_else(|| ParseError::InvalidSyntax("Missing emit port".to_string()))?
        .as_str()
        .to_string();
    let value = parse_expression(parts.next().ok_or_else(|| {
        ParseError::InvalidSyntax("Missing emit value".to_string())
    })?)?;
    Ok(Statement::Emit { port, value })
}

fn parse_let_stmt_direct(pair: pest::iterators::Pair<Rule>) -> ParseResult<Statement> {
    let mut parts = pair.into_inner();
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

fn parse_statement(pair: pest::iterators::Pair<Rule>) -> ParseResult<Statement> {
    let inner = pair.into_inner().next().ok_or_else(|| {
        ParseError::InvalidSyntax("Empty statement".to_string())
    })?;

    match inner.as_rule() {
        Rule::match_stmt => {
            parse_match_statement(inner)
        }
        Rule::emit_stmt => {
            parse_emit_stmt_direct(inner)
        }
        Rule::let_stmt => {
            parse_let_stmt_direct(inner)
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

fn parse_match_statement(pair: pest::iterators::Pair<Rule>) -> ParseResult<Statement> {
    let mut inner = pair.into_inner();
    
    let expr_pair = inner.find(|p| p.as_rule() == Rule::expression).ok_or_else(|| {
        ParseError::InvalidSyntax("Missing match expression".to_string())
    })?;
    let expression = Box::new(parse_expression(expr_pair)?);
    
    let mut arms = Vec::new();
    for arm_pair in inner {
        if arm_pair.as_rule() == Rule::match_arm {
            arms.push(parse_match_arm(arm_pair)?);
        }
    }
    
    Ok(Statement::Match { expression, arms })
}

fn parse_match_arm(pair: pest::iterators::Pair<Rule>) -> ParseResult<MatchArm> {
    let mut inner = pair.into_inner();

    let pattern_pair = inner.next().ok_or_else(|| {
        ParseError::InvalidSyntax("Missing pattern in match arm".to_string())
    })?;
    let pattern = parse_pattern(pattern_pair)?;

    let mut guard = None;
    let mut body = Vec::new();
    
    for part in inner {
        verbose!("[PARSE_ARM] Found part: {:?}", part.as_rule());
        match part.as_rule() {
            Rule::pattern_guard => {
                let guard_expr = part.into_inner().next().ok_or_else(|| {
                    ParseError::InvalidSyntax("Empty guard".to_string())
                })?;
                guard = Some(parse_expression(guard_expr)?);
            }
            Rule::match_arm_body => {
                for stmt_pair in part.into_inner() {
                    verbose!("[PARSE_ARM] Body part: {:?}", stmt_pair.as_rule());
                    if stmt_pair.as_rule() == Rule::statement {
                        body.push(parse_statement(stmt_pair)?);

                    } else if stmt_pair.as_rule() == Rule::emit_stmt {
                        body.push(parse_emit_stmt_direct(stmt_pair)?);

                    } else if stmt_pair.as_rule() == Rule::let_stmt {
                        body.push(parse_let_stmt_direct(stmt_pair)?);

                    }
                }
            }
            Rule::statement => {
                body.push(parse_statement(part)?);
            }
            _ => {}
        }
    }
    
    verbose!("[PARSE_ARM] Body has {} statements", body.len());
    Ok(MatchArm { pattern, guard, body })
}

fn parse_pattern(pair: pest::iterators::Pair<Rule>) -> ParseResult<Pattern> {
    let inner = pair.into_inner().next().ok_or_else(|| {
        ParseError::InvalidSyntax("Empty pattern".to_string())
    })?;
    
    match inner.as_rule() {
        Rule::pattern_wildcard => Ok(Pattern::Wildcard),
        Rule::pattern_literal => {
            let lit_inner = inner.into_inner().next().ok_or_else(|| {
                ParseError::InvalidSyntax("Empty pattern literal".to_string())
            })?;
            let expr = parse_literal(lit_inner)?;
            Ok(Pattern::Literal(expr))
        }
        Rule::pattern_tuple => {
            let patterns: Result<Vec<Pattern>, _> = inner
                .into_inner()
                .map(|p| parse_pattern(p))
                .collect();
            Ok(Pattern::Tuple(patterns?))
        }
        Rule::pattern_binding => {
            let name = inner.as_str().to_string();
            Ok(Pattern::Binding(name))
        }
        Rule::identifier => {
            let name = inner.as_str().to_string();
            Ok(Pattern::Binding(name))
        }
        _ => Err(ParseError::InvalidSyntax(format!(
            "Unknown pattern rule: {:?}",
            inner.as_rule()
        ))),
    }
}

fn parse_literal(pair: pest::iterators::Pair<Rule>) -> ParseResult<Expression> {
    match pair.as_rule() {
        Rule::int_literal => {
            let val: i64 = pair.as_str().parse().map_err(|_| {
                ParseError::InvalidSyntax("Invalid integer literal".to_string())
            })?;
            Ok(Expression::IntLiteral(val))
        }
        Rule::float_literal => {
            let val: f64 = pair.as_str().parse().map_err(|_| {
                ParseError::InvalidSyntax("Invalid float literal".to_string())
            })?;
            Ok(Expression::FloatLiteral(val))
        }
        Rule::string_literal => {
            let s = pair.as_str();
            let content = &s[1..s.len() - 1];
            Ok(Expression::StringLiteral(content.to_string()))
        }
        Rule::bool_literal => {
            let val = pair.as_str() == "true";
            Ok(Expression::BoolLiteral(val))
        }
        _ => Err(ParseError::InvalidSyntax(format!(
            "Unknown literal rule: {:?}",
            pair.as_rule()
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
            
            if let Some(param_list) = inner.next() {
                if param_list.as_rule() == Rule::param_list {
                    for param in param_list.into_inner() {
                        params.push(param.as_str().to_string());
                    }
                }
            }
            
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

    if parts.is_empty() {
        return Err(ParseError::InvalidSyntax("Empty port reference".to_string()));
    }

    if parts.len() == 1 {
        let port = parts.pop().unwrap();
        return Ok(PortRef::new("", port));
    }

    let port = parts.pop().unwrap();
    let node = parts.join(".");

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
    

    #[test]
    fn test_parse_type() {
        let input = r#"
            type Point {
                x: Int,
                y: Int
            }
            node User {
                in loc: Point
            }
        "#;
        
        let result = parse_file(input);
        if let Err(e) = &result {
             eprintln!("Parse error: {}", e);
        }
        assert!(result.is_ok());
        let graph = result.unwrap();
        assert_eq!(graph.types.len(), 1);
        assert_eq!(graph.types[0].name, "Point");
        assert_eq!(graph.types[0].fields.len(), 2);
        assert_eq!(graph.types[0].fields[0].0, "x");
        
        let node = &graph.nodes[0];
        match &node.input_ports[0].type_annotation {
            DanaType::Type(name, _) => assert_eq!(name, "Point"),
            _ => panic!("Expected Type for 'loc' port"),
        }
    }

    #[test]
    fn test_parse_graph() {
        let input = r#"
            node Worker {
                in data: Int
                out res: Int
                process: (data) => {
                    emit res(42)
                }
            }

            graph Main {
                in start_param: Int
                out final_res: Int

                Worker.res -> System.IO.Stdout
            }
        "#;
        
        let result = parse_file(input);
        if let Err(e) = &result {
             eprintln!("Parse error: {}", e);
        }
        assert!(result.is_ok());
        let graph = result.unwrap();
        
        assert_eq!(graph.nodes.len(), 1);
        assert_eq!(graph.nodes[0].name, "Worker");

        assert_eq!(graph.subgraphs.len(), 1);
        let main = &graph.subgraphs[0];
        assert_eq!(main.name, "Main");

        assert_eq!(main.edges.len(), 1);
        assert_eq!(main.edges[0].source.node, "Worker");
        assert_eq!(main.edges[0].source.port, "res");
        assert_eq!(main.edges[0].target.node, "System.IO");
        assert_eq!(main.edges[0].target.port, "Stdout");
    }

    #[test]
    fn test_parse_another_node() {
        let input = r#"
        node Source {
            prefix: String = "Hello from "
            in start: Impulse
            out data: String
            
            process: (start) => {
                emit data(prefix + "Graph!")
            }
        }
        node Output {
            in data: String
            out new_data: String
            process: (data) => {
                emit new_data(data)
            }
        }

        graph SubGraph {
            node AnotherNode {
                in a_string: String
                out new_data : String
                process: (a_string) => {
                    emit new_data(a_string + "!!!!!!!")
                }
            }
            Source.data -> Output.new_data
            AnotherNode.a_string -> Output.new_data
        }
        graph Main {
            SubGraph.new_data -> System.IO.Stdout
        }
        "#;
        
        let result = parse_file(input);
        if let Err(e) = &result {
             eprintln!("Parse error: {}", e);
        }
        assert!(result.is_ok());
        let graph = result.unwrap();
            
        // Check global node definition
        assert_eq!(graph.nodes.len(), 2);
        assert_eq!(graph.nodes[0].name, "Source");
        assert_eq!(graph.nodes[1].name, "Output");

        // Check subgraph definition
        assert_eq!(graph.subgraphs.len(), 2);
        let main = &graph.subgraphs[1];
        assert_eq!(main.name, "Main");
        let subgraph = &graph.subgraphs[0];
        assert_eq!(subgraph.name, "SubGraph");
        
        // Check edges within graph
        assert_eq!(main.edges.len(), 1);
        assert_eq!(main.edges[0].source.node, "SubGraph");
        assert_eq!(main.edges[0].source.port, "new_data");
        assert_eq!(main.edges[0].target.node, "System.IO");
        assert_eq!(main.edges[0].target.port, "Stdout");

        assert_eq!(subgraph.edges.len(), 2);
        assert_eq!(subgraph.edges[0].source.node, "Source");
        assert_eq!(subgraph.edges[0].source.port, "data");
        assert_eq!(subgraph.edges[0].target.node, "Output");
        assert_eq!(subgraph.edges[0].target.port, "new_data");
    }
}
