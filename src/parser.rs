use crate::error::*;
use crate::lexer::*;
use crate::types::ToppleType;

pub type AST = Vec<Node>;

#[derive(Debug, PartialEq)]
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

// Ordered from tightest to loosest bind
#[derive(Debug, PartialEq)]
pub enum NodeType {
    Literal,
    Index,
    Call,
    BitNot,
    Mult,
    Div,
    Mod,
    Add,
    Sub,
    BitAnd,
    BitOr,
    BitXor,
    ShiftLeft,
    ShiftRight,
    Pop,
    Push,
    Assign,
    Def,
}

#[derive(Debug, PartialEq)]
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
        let node = parse_def(&stream[expr_start..expr_end])?;
        ast.push(node);
        expr_start = expr_end + 1;
    }
    Ok(ast)
}

fn parse_def(expr: TokenStreamSlice) -> ToppleResult<Node> {
    todo!()
}

impl std::fmt::Display for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(l) => match l {
                ToppleType::ByteTable(t) => match t.as_unsigned() {
                    Some(n) => write!(f, "{n}"),
                    None => write!(f, "{t}"),
                },
                _ => write!(f, "{l}"),
            },
            Self::Ident(s) => write!(f, "{s}"),
            Self::Node(n) => write!(f, "({n})"),
        }
    }
}

impl std::fmt::Binary for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(l) => write!(f, "{l}"),
            Self::Ident(s) => write!(f, "{s}"),
            Self::Node(n) => write!(f, "({n})"),
        }
    }
}

impl std::fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let right = match &self.right {
            Some(v) => v.to_string(),
            None => "WARNING: Right side empty".into(),
        };
        match self.node_type {
            NodeType::Literal => write!(f, "{}", self.left),
            NodeType::Index => write!(f, "{}.{}", self.left, right),
            NodeType::Call => write!(f, "{}({})", self.left, right),
            NodeType::BitNot => write!(f, "!{}", self.left),
            NodeType::Mult => write!(f, "{} * {}", self.left, right),
            NodeType::Div => write!(f, "{} / {}", self.left, right),
            NodeType::Mod => write!(f, "{} % {}", self.left, right),
            NodeType::Add => write!(f, "{} + {}", self.left, right),
            NodeType::Sub => write!(f, "{} - {}", self.left, right),
            NodeType::BitAnd => write!(f, "{} & {}", self.left, right),
            NodeType::BitOr => write!(f, "{} | {}", self.left, right),
            NodeType::BitXor => write!(f, "{} ^ {}", self.left, right),
            NodeType::ShiftLeft => write!(f, "{} << {}", self.left, right),
            NodeType::ShiftRight => write!(f, "{} >> {}", self.left, right),
            NodeType::Pop => write!(f, "{}--", self.left),
            NodeType::Push => write!(f, "{} ++ {}", self.left, right),
            NodeType::Assign => write!(f, "{} = {}", self.left, right),
            NodeType::Def => write!(f, "let {}", self.left),
        }
    }
}

impl std::fmt::Binary for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let right = match &self.right {
            Some(v) => v.to_string(),
            None => "WARNING: Right side empty".into(),
        };
        match self.node_type {
            NodeType::Literal => write!(f, "{:b}", self.left),
            NodeType::Index => write!(f, "{:b}.{}", self.left, right),
            NodeType::Call => write!(f, "{:b}({})", self.left, right),
            NodeType::BitNot => write!(f, "!{:b}", self.left),
            NodeType::Mult => write!(f, "{:b} * {}", self.left, right),
            NodeType::Div => write!(f, "{:b} / {}", self.left, right),
            NodeType::Mod => write!(f, "{:b} % {}", self.left, right),
            NodeType::Add => write!(f, "{:b} + {}", self.left, right),
            NodeType::Sub => write!(f, "{:b} - {}", self.left, right),
            NodeType::BitAnd => write!(f, "{:b} & {}", self.left, right),
            NodeType::BitOr => write!(f, "{:b} | {}", self.left, right),
            NodeType::BitXor => write!(f, "{:b} ^ {}", self.left, right),
            NodeType::ShiftLeft => write!(f, "{:b} << {}", self.left, right),
            NodeType::ShiftRight => write!(f, "{:b} >> {}", self.left, right),
            NodeType::Pop => write!(f, "{:b}--", self.left),
            NodeType::Push => write!(f, "{:b} ++ {}", self.left, right),
            NodeType::Assign => write!(f, "{:b} = {}", self.left, right),
            NodeType::Def => write!(f, "let {:b}", self.left),
        }
    }
}

#[cfg(test)]
mod parser_tests {
    use super::*;

    #[test]
    fn math_ops() {
        let buf = "3 + 2 * 5 / 5 - 1;";
        let stream = lex(buf.as_bytes()).unwrap();
        let out = parse_tokens(stream).unwrap();
        assert_eq!(math_node(), out[0]);
    }

    #[test]
    fn print_node() {
        let node = math_node();
        let s = "(3 + ((2 * 5) / 5)) - 1";
        assert_eq!(s, format!("{node}"));
    }

    fn math_node() -> Node {
        let mult = Node::new_binary(num_lit(2), num_lit(5), NodeType::Mult, 0, 4);
        let div = Node::new_binary(node_val(mult), num_lit(5), NodeType::Div, 0, 4);
        let add = Node::new_binary(num_lit(3), node_val(div), NodeType::Add, 0, 0);
        let sub = Node::new_binary(node_val(add), num_lit(1), NodeType::Sub, 0, 0);
        sub
    }

    fn num_lit(num: u64) -> Val {
        Val::Literal(ToppleType::ByteTable(num.into()))
    }

    fn ident(s: &str) -> Val {
        Val::Ident(s.into())
    }

    fn node_val(node: Node) -> Val {
        Val::Node(Box::new(node))
    }
}
