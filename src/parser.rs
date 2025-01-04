use crate::error::*;
use crate::lexer::*;
use crate::types::BitTable;
use crate::types::ByteTable;
use crate::types::ToppleType;
use std::fmt::{Binary, Display};

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
    Block,
}

#[derive(Debug, PartialEq)]
pub enum Val {
    Literal(ToppleType),
    Ident(String),
    Node(Box<Node>),
    Table(Vec<Node>),
    Block(AST),
}

impl std::cmp::PartialEq<NodeType> for Node {
    fn eq(&self, other: &NodeType) -> bool {
        self.node_type == *other
    }
}

pub fn parse_tokens(stream: TokenStreamSlice) -> ToppleResult<AST> {
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

fn parse_table(slice: TokenStreamSlice, idx: &mut usize) -> ToppleResult<Node> {
    let mut table = Vec::new();
    let mut expr_start = *idx + 1;
    let mut expr_end = expr_start;
    loop {
        let (t, line, chr) = &slice[expr_end];
        if expr_start >= slice.len() || expr_end >= slice.len() {
            return Err(ToppleError::OpenBracketError(*line, *chr));
        }
        if *t == Token::Comma {
            let e = parse_expr(&slice[expr_start..expr_end])?;
            expr_end += 1;
            expr_start = expr_end;
            table.push(e);
        } else if *t == Token::RightBracket {
            if expr_end == expr_start {
                break;
            }
            let e = parse_expr(&slice[expr_start..expr_end])?;
            table.push(e);
            break;
        } else {
            expr_end += 1;
        }
    }
    let (_, line, chr) = slice[*idx];
    *idx = expr_end + 1;
    if table.is_empty() {
        let v = Val::Table(table);
        let n = Node::new_unary(v, NodeType::Literal, line, chr);
        return Ok(n);
    }
    let mut simple_table = Vec::new();
    for i in 0..table.len() {
        let node = simplify_expr(&table[i]);
        simple_table.push(node);
    }
    let v = Val::Table(simple_table);
    let n = Node::new_unary(v, NodeType::Literal, line, chr);
    Ok(n)
}

fn parse_literal(slice: TokenStreamSlice, idx: &mut usize) -> ToppleResult<Node> {
    let (token, line, chr) = &slice[*idx];
    let left = match token {
        Token::Num(n) => match n {
            Num::Imm(i) => Val::Literal(ToppleType::ByteTable(i.into())),
            Num::Bits(s) => Val::Literal(ToppleType::BitTable(BitTable::from_str(&s))),
        },
        Token::Str(s) => Val::Literal(ToppleType::ByteTable(ByteTable::from_str(&s))),
        Token::Ident(s) => Val::Ident(s.to_string()),
        Token::LeftBracket => return parse_table(slice, idx),
        Token::LeftCurly => {
            let mut block_end = *idx + 1;
            let mut inner_blocks = 0;
            loop {
                let (t, line, chr) = &slice[block_end];
                if block_end >= slice.len() {
                    return Err(ToppleError::OpenBlockError(*line, *chr));
                }
                if *t == Token::RightCurly {
                    if inner_blocks > 0 {
                        inner_blocks -= 1;
                    } else {
                        break;
                    }
                } else if *t == Token::LeftCurly {
                    inner_blocks += 1;
                }
                block_end += 1;
            }
            if block_end == *idx + 1 {
                Val::Block(Vec::new())
            } else {
                let res = Val::Block(parse_tokens(&slice[(*idx + 1)..block_end])?);
                *idx = block_end + 1;
                res
            }
        }
        Token::LeftParen => {
            let mut expr_end = *idx + 1;
            let mut inner_expr = 0;
            loop {
                let (t, line, chr) = &slice[expr_end];
                if expr_end >= slice.len() {
                    return Err(ToppleError::OpenParenError(*line, *chr));
                }
                if *t == Token::RightParen {
                    if inner_expr > 0 {
                        inner_expr -= 1;
                    } else {
                        break;
                    }
                } else if *t == Token::LeftParen {
                    inner_expr += 1;
                }
                expr_end += 1;
            }
            if expr_end == *idx + 1 {
                Val::Block(Vec::new())
            } else {
                let res = parse_expr(&slice[(*idx + 1)..expr_end])?;
                *idx = expr_end + 1;
                Val::Node(Box::new(res))
            }
        }
        _ => return Err(ToppleError::UnexpectedToken(token.clone(), *line, *chr)),
    };
    let n = Node::new_unary(left, NodeType::Literal, *line, *chr);
    Ok(n)
}

fn parse_expr(expr: TokenStreamSlice) -> ToppleResult<Node> {
    todo!()
}

fn simplify_expr(node: &Node) -> Node {
    todo!()
}

impl Display for Val {
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
            Self::Block(b) => {
                let mut s = String::with_capacity(b.len() * 16);
                s.push('{');
                let mut i = 0;
                s += &format!("{};", b[i]);
                i += 1;
                while i < b.len() {
                    s.push('\n');
                    s += &format!("{};", b[i]);
                }
                s.push('}');
                write!(f, "{s}")
            }
            Self::Table(t) => write!(f, "{t:?}"),
        }
    }
}

impl Binary for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(l) => write!(f, "{l}"),
            Self::Ident(s) => write!(f, "{s}"),
            Self::Node(n) => write!(f, "({n})"),
            Self::Block(b) => {
                let mut s = String::with_capacity(b.len() * 16);
                s.push('{');
                let mut i = 0;
                s += &format!("{:b};", b[i]);
                i += 1;
                while i < b.len() {
                    s.push('\n');
                    s += &format!("{:b};", b[i]);
                }
                s.push('}');
                write!(f, "{s}")
            }
            Self::Table(t) => write!(f, "{t:?}"),
        }
    }
}

impl Display for Node {
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
            NodeType::Block => write!(f, "{}", self.left),
        }
    }
}

impl Binary for Node {
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
            NodeType::Block => write!(f, "{:b}", self.left),
        }
    }
}

#[cfg(test)]
mod parser_tests {
    use crate::types::BitTable;

    use super::*;

    #[test]
    fn math_ops() {
        let buf = "3 + 2 * 5 / 5 - 1;";
        let stream = lex(buf.as_bytes()).unwrap();
        let out = parse_tokens(&stream[..]).unwrap();
        assert_eq!(math_node(), out[0]);
    }

    #[test]
    fn bitwise_ops() {
        let buf = "0b1100 ^ 0b0101 & !0b0100 | 0b0100;";
        let stream = lex(buf.as_bytes()).unwrap();
        let out = parse_tokens(&stream[..]).unwrap();
        assert_eq!(bitwise_node(), out[0]);
    }

    #[test]
    fn op_order() {
        let buf = "3 + 5 >> 6 / 3;";
        let stream = lex(buf.as_bytes()).unwrap();
        let out = parse_tokens(&stream[..]).unwrap();
        assert_eq!(order_node(), out[0]);
    }

    #[test]
    fn def_statement() {
        let buf = "let a; let b = 3;";
        let stream = lex(buf.as_bytes()).unwrap();
        let out = parse_tokens(&stream[..]).unwrap();

        let def = Node::new_unary(ident("a", 0, 4), NodeType::Def, 0, 0);
        assert_eq!(def, out[0]);

        let def_assign =
            Node::new_binary(ident("b", 0, 11), num_lit(3, 0, 15), NodeType::Def, 0, 7);
        assert_eq!(def_assign, out[1]);
    }

    #[test]
    #[should_panic]
    fn def_immediate() {
        let buf = "let 5 = 4;";
        let stream = lex(buf.as_bytes()).unwrap();
        parse_tokens(&stream[..]).unwrap();
    }

    #[test]
    fn paren_expr() {
        let buf = "3 * (2 + 4) - 4;";
        let stream = lex(buf.as_bytes()).unwrap();
        let out = parse_tokens(&stream[..]).unwrap();

        let paren = Node::new_binary(num_lit(2, 0, 5), num_lit(4, 0, 9), NodeType::Add, 0, 5);
        let mult = Node::new_binary(num_lit(3, 0, 0), node_val(paren), NodeType::Mult, 0, 0);
        let sub = Node::new_binary(node_val(mult), num_lit(4, 0, 14), NodeType::Sub, 0, 0);
        assert_eq!(sub, out[0]);
    }

    #[test]
    fn print_node() {
        let node = math_node();
        let s = "(3 + ((2 * 5) / 5)) - 1";
        assert_eq!(s, format!("{node}"));
    }

    fn math_node() -> Node {
        // 3 + 2 * 5 / 5 - 1;
        let mult = Node::new_binary(num_lit(2, 0, 4), num_lit(5, 0, 8), NodeType::Mult, 0, 4);
        let div = Node::new_binary(node_val(mult), num_lit(5, 0, 12), NodeType::Div, 0, 4);
        let add = Node::new_binary(num_lit(3, 0, 0), node_val(div), NodeType::Add, 0, 0);
        let sub = Node::new_binary(node_val(add), num_lit(1, 0, 16), NodeType::Sub, 0, 0);
        sub
    }

    fn bitwise_node() -> Node {
        // 0b1100 ^ 0b0101 & !0b0100 | 0b0100;
        let not = Node::new_unary(binary_lit("0100", 0, 19), NodeType::BitNot, 0, 18);
        let and = Node::new_binary(
            binary_lit("0101", 0, 9),
            node_val(not),
            NodeType::BitAnd,
            0,
            9,
        );
        let or = Node::new_binary(
            node_val(and),
            binary_lit("0100", 0, 28),
            NodeType::BitOr,
            0,
            9,
        );
        let xor = Node::new_binary(
            binary_lit("1100", 0, 0),
            node_val(or),
            NodeType::BitXor,
            0,
            0,
        );
        xor
    }

    fn order_node() -> Node {
        // 3 + 5 >> 6 / 3;
        let add = Node::new_binary(num_lit(3, 0, 0), num_lit(5, 0, 4), NodeType::Add, 0, 0);
        let div = Node::new_binary(num_lit(6, 0, 9), num_lit(3, 0, 13), NodeType::Div, 0, 9);
        let shift = Node::new_binary(node_val(add), node_val(div), NodeType::ShiftRight, 0, 0);
        shift
    }

    fn num_lit(num: u64, line: usize, chr: usize) -> Val {
        let val = Val::Literal(ToppleType::ByteTable(num.into()));
        let node = Node::new_unary(val, NodeType::Literal, line, chr);
        Val::Node(Box::new(node))
    }

    fn binary_lit(s: &str, line: usize, chr: usize) -> Val {
        let val = Val::Literal(ToppleType::BitTable(BitTable::from_str(s)));
        let node = Node::new_unary(val, NodeType::Literal, line, chr);
        Val::Node(Box::new(node))
    }

    fn ident(s: &str, line: usize, chr: usize) -> Val {
        let val = Val::Ident(s.into());
        let node = Node::new_unary(val, NodeType::Literal, line, chr);
        Val::Node(Box::new(node))
    }

    fn node_val(node: Node) -> Val {
        Val::Node(Box::new(node))
    }
}
