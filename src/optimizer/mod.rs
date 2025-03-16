use crate::lexer::Keyword;
use crate::parser::{Ident, Node, NodeType, Val, AST};
use crate::types::ByteTable;
use std::convert::From;

mod ssa;

pub use ssa::ssa_conversion;

pub type SlimAST = Vec<SlimNode>;

#[derive(Debug, PartialEq, Clone)]
pub struct SlimNode {
    left: SlimVal,
    right: Option<SlimVal>,
    node_type: NodeType,
}

impl SlimNode {
    pub fn new_unary(left: SlimVal, node_type: NodeType) -> Self {
        Self {
            left,
            right: None,
            node_type,
        }
    }

    pub fn new_binary(left: SlimVal, right: SlimVal, node_type: NodeType) -> Self {
        Self {
            left,
            right: Some(right),
            node_type,
        }
    }
}

impl From<Node> for SlimNode {
    fn from(value: Node) -> Self {
        let (left, right, node_type, ..) = value.destructure();
        let right = match right {
            Some(v) => Some(v.into()),
            None => None,
        };
        Self {
            left: left.into(),
            right,
            node_type,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum SlimVal {
    ByteTable(ByteTable),
    Ident(Ident),
    Keyword(Keyword),
    Table(Vec<SlimNode>),
    Block(SlimAST),
    Node(Box<SlimNode>),
}

impl From<Val> for SlimVal {
    fn from(value: Val) -> Self {
        match value {
            Val::ByteTable(b) => SlimVal::ByteTable(b),
            Val::Ident(i) => SlimVal::Ident(i),
            Val::Keyword(k) => SlimVal::Keyword(k),
            Val::Table(t) => SlimVal::Table(ast_from_parser(t)),
            Val::Block(b) => SlimVal::Block(ast_from_parser(b)),
            Val::Node(n) => SlimVal::Node(Box::new(Into::into(*n))),
        }
    }
}

fn ast_from_parser(ast: crate::parser::AST) -> Vec<SlimNode> {
    let mut res = Vec::with_capacity(ast.len());
    for n in ast {
        res.push(n.into());
    }
    res
}
