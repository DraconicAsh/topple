use crate::lexer::Keyword;
use crate::parser::{Ident, Node, NodeType, Val, AST};
use crate::types::ByteTable;
use std::convert::From;

mod ssa;

pub type SlimAST = Vec<SlimNode>;

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub enum SlimVal {
    ByteTable(ByteTable),
    Ident(Ident),
    Keyword(Keyword),
    Table(Vec<SlimNode>),
    Block(Block),
    Node(Box<SlimNode>),
}

impl From<Val> for SlimVal {
    fn from(value: Val) -> Self {
        match value {
            Val::ByteTable(b) => SlimVal::ByteTable(b),
            Val::Ident(i) => SlimVal::Ident(i),
            Val::Keyword(k) => SlimVal::Keyword(k),
            Val::Table(t) => SlimVal::Table(table_from_parser(t)),
            Val::Block(b) => SlimVal::Block(b.into()),
            Val::Node(n) => SlimVal::Node(Box::new(Into::into(*n))),
        }
    }
}

fn table_from_parser(table: crate::parser::AST) -> Vec<SlimNode> {
    let mut res = Vec::with_capacity(table.len());
    for n in table {
        res.push(n.into());
    }
    res
}

#[derive(Debug, PartialEq)]
pub struct Block {
    block: SlimAST,
    implicit_args: Vec<String>,
}

impl Block {
    pub fn new(block: SlimAST) -> Self {
        Self {
            block,
            implicit_args: Vec::new(),
        }
    }

    pub fn with_args(block: SlimAST, implicit_args: Vec<String>) -> Self {
        Self {
            block,
            implicit_args,
        }
    }

    pub fn args(&mut self) -> &mut Vec<String> {
        &mut self.implicit_args
    }
}

impl From<AST> for Block {
    fn from(value: AST) -> Self {
        let mut block = Vec::with_capacity(value.len());
        for n in value {
            block.push(n.into());
        }
        Self {
            block,
            implicit_args: Vec::new(),
        }
    }
}
