use crate::error::*;
use crate::lexer::*;
use crate::types::ToppleType;

pub type AST = Vec<Node>;

pub struct Node {
    left: Val,
    right: Option<Val>,
    node_type: NodeType,
    line: usize,
    chr: usize,
}

impl Node {
    fn new_unary(left: Val, node_type: NodeType, line: usize, chr: usize) -> Self {
        Self {
            left,
            right: None,
            node_type,
            line,
            chr,
        }
    }

    fn new_binary(left: Val, right: Val, node_type: NodeType, line: usize, chr: usize) -> Self {
        Self {
            left,
            right: Some(right),
            node_type,
            line,
            chr,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum NodeType {
    Literal,
    BitNot,
}

pub enum Val {
    Literal(ToppleType),
    Ident(String),
    Node(Box<Node>),
}

impl std::cmp::PartialEq<NodeType> for Node {
    fn eq(&self, other: &NodeType) -> bool {
        self.node_type == *other
    }
}

pub fn parse_tokens(stream: TokenStream) -> ToppleResult<AST> {
    let mut ast = Vec::new();
    let mut expr_start = 0;
    let len = stream.len();
    while expr_start < len {
        let mut expr_end = expr_start + 1;
        while stream[expr_end].0 != Token::SemiColon {
            expr_end += 1;
            if expr_end >= len {
                let (_, line, chr) = stream[len - 1];
                return Err(ToppleError::OpenExprError(line, chr));
            }
        }
        let node = parse_expr(&stream[expr_start..expr_end])?;
        ast.push(node);
        expr_start = expr_end + 1;
    }
    Ok(ast)
}

fn parse_expr(expr: TokenStreamSlice) -> ToppleResult<Node> {
    todo!()
}
