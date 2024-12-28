use crate::lexer::*;
use crate::ToppleType;

pub type AST = Node;
type Ident = String;

pub enum Node {
    EndPoint(Option<Box<Node>>),
    Expr(Box<Expr>),
    Index(Box<Node>, Box<Node>),
    Func(String, Vec<Node>),
    Block(Box<Node>),
    Def(Ident),
}

pub enum Expr {
    Literal(ToppleType),
    Paren(Node),
    Math(Math),
    BitOp(BitOp),
    Cmp(Cmp),
    Append(Node, Node),
    Pop(Node),
    Assign(Ident, Node),
}

pub enum Math {
    Mult(Node, Node),
    Div(Node, Node),
    Add(Node, Node),
    Sub(Node, Node),
}

pub enum BitOp {
    BitNot(Node),
    BitAnd(Node, Node),
    BitOr(Node, Node),
    BitXor(Node, Node),
    ShiftLeft(Node, Node),
    ShiftRight(Node, Node),
}

pub enum Cmp {
    Eq(Node, Node),
    NotEq(Node, Node),
    GreaterEq(Node, Node),
    LessEq(Node, Node),
    Greater(Node, Node),
    Less(Node, Node),
}

pub struct Parser<'a> {
    am_block: bool,
    stream: TokenStreamSlice<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(stream: &'a TokenStream) -> Self {
        Self::new_raw(false, &stream[..])
    }

    fn new_raw(am_block: bool, stream: TokenStreamSlice<'a>) -> Self {
        Self { am_block, stream }
    }

    pub fn parse(&self) -> AST {
        let mut ast = Node::EndPoint(None);
        todo!()
    }

    fn literal(pos: usize) -> Expr {
        todo!()
    }

    fn parentheses(pos: usize) -> Expr {
        todo!()
    }

    fn mult(pos: usize) -> Expr {
        todo!()
    }

    fn div(pos: usize) -> Expr {
        todo!()
    }

    fn add(pos: usize) -> Expr {
        todo!()
    }

    fn sub(pos: usize) -> Expr {
        todo!()
    }

    fn bit_not(pos: usize) -> Expr {
        todo!()
    }

    fn bit_and(pos: usize) -> Expr {
        todo!()
    }

    fn bit_or(pos: usize) -> Expr {
        todo!()
    }

    fn bit_xor(pos: usize) -> Expr {
        todo!()
    }

    fn shift_left(pos: usize) -> Expr {
        todo!()
    }

    fn shift_right(pos: usize) -> Expr {
        todo!()
    }

    fn equals(pos: usize) -> Expr {
        todo!()
    }

    fn not_equals(pos: usize) -> Expr {
        todo!()
    }

    fn greater_or_equals(pos: usize) -> Expr {
        todo!()
    }

    fn less_or_equals(pos: usize) -> Expr {
        todo!()
    }

    fn greater_than(pos: usize) -> Expr {
        todo!()
    }

    fn less_than(pos: usize) -> Expr {
        todo!()
    }

    fn append(pos: usize) -> Expr {
        todo!()
    }

    fn pop(pos: usize) -> Expr {
        todo!()
    }
}
