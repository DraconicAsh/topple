use crate::error::*;
use crate::lexer::*;
use crate::types::ByteTable;
use crate::types::ToppleType;
use std::fmt::{Binary, Display};

pub type AST = Vec<Node>;

#[derive(Debug, Clone)]
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

impl std::cmp::PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        self.left == other.left && self.right == other.right && self.node_type == other.node_type
    }
}

// Ordered from tightest to loosest bind
#[derive(Debug, PartialEq, Clone, Copy)]
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
    CmpGT,
    CmpLT,
    CmpGTE,
    CmpLTE,
    CmpEQ,
    CmpNE,
    Assign,
    Def,
    Block,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Val {
    Literal(ToppleType),
    Ident(String),
    Keyword(Keyword),
    Node(Box<Node>),
    Table(Vec<Node>),
    Block(AST),
}

impl std::cmp::PartialEq<NodeType> for Node {
    fn eq(&self, other: &NodeType) -> bool {
        self.node_type == *other
    }
}

pub fn parse_tokens(stream: TokenStream) -> ToppleResult<AST> {
    parse_tokens_raw(&stream[..])
}

fn parse_tokens_raw(stream: TokenStreamSlice) -> ToppleResult<AST> {
    let mut ast = Vec::new();
    let mut expr_start = 0;
    let len = stream.len();
    while expr_start < len {
        let mut expr_end = expr_start + 1;
        let mut open_blocks = 0;
        loop {
            let (token, line, chr) = &stream[expr_end];
            if *token == Token::SemiColon && open_blocks == 0 {
                break;
            }
            if *token == Token::LeftCurly {
                open_blocks += 1;
            } else if *token == Token::RightCurly {
                if open_blocks == 0 {
                    return Err(ToppleError::UnexpectedToken(Token::RightCurly, *line, *chr));
                }
                open_blocks -= 1;
            }
            expr_end += 1;
            if expr_end >= len {
                return Err(ToppleError::OpenExprError(*line, *chr));
            }
        }
        let node = parse_expr(&stream[expr_start..expr_end])?;
        ast.push(node);
        expr_start = expr_end + 1;
    }
    Ok(ast)
}

fn parse_table(
    slice: TokenStreamSlice,
    idx: &mut usize,
    start_token: Token,
    end_token: Token,
) -> ToppleResult<Node> {
    let mut table = Vec::new();
    let mut expr_start = *idx + 1;
    let mut expr_end = expr_start;
    if expr_start >= slice.len() || expr_end >= slice.len() {
        let (_, line, chr) = &slice[*idx];
        return Err(ToppleError::OpenBracketError(*line, *chr));
    }
    let mut inner_sets = 0;
    let (_, mut line, mut chr) = &slice[expr_end];
    loop {
        if expr_start >= slice.len() || expr_end >= slice.len() {
            return Err(ToppleError::OpenBracketError(line, chr));
        }
        let (t, l, c) = &slice[expr_end];
        line = *l;
        chr = *c;
        if *t == Token::Comma {
            let e = parse_expr(&slice[expr_start..expr_end])?;
            expr_end += 1;
            expr_start = expr_end;
            table.push(e);
        } else if *t == end_token {
            if expr_end == expr_start {
                break;
            }
            if inner_sets > 0 {
                inner_sets -= 1;
                expr_end += 1;
                continue;
            }
            let e = parse_expr(&slice[expr_start..expr_end])?;
            table.push(e);
            break;
        } else {
            if *t == start_token {
                inner_sets += 1;
            }
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
    let mut type_table = true;
    let mut byte_table = Vec::new();
    let mut simple_table = Vec::new();
    for i in 0..table.len() {
        let node = simplify_expr(&table[i]);
        if type_table {
            if let Val::Literal(t) = &node.left {
                byte_table.push(t.clone());
            } else {
                type_table = false;
                byte_table.clear();
            }
        }
        simple_table.push(node);
    }
    let v = if type_table {
        match ByteTable::try_from_type_vec(byte_table) {
            Some(b) => Val::Literal(ToppleType::ByteTable(b)),
            None => Val::Table(simple_table),
        }
    } else {
        Val::Table(simple_table)
    };
    let n = Node::new_unary(v, NodeType::Literal, line, chr);
    Ok(n)
}

fn parse_def(slice: TokenStreamSlice, idx: &mut usize) -> ToppleResult<Node> {
    let (_, line, chr) = &slice[*idx];
    *idx += 1;
    if *idx >= slice.len() {
        return Err(ToppleError::HangingLetError(*line, *chr));
    }
    let left = parse_assign(slice, idx)?;
    if left != NodeType::Assign {
        if left == NodeType::Literal {
            match left.left {
                Val::Ident(_) => (),
                _ => return Err(ToppleError::HangingLetError(*line, *chr)),
            }
        } else if left != NodeType::Index {
            return Err(ToppleError::HangingLetError(*line, *chr));
        }
    }
    let left = Val::Node(Box::new(left));
    let node = Node::new_unary(left, NodeType::Def, *line, *chr);
    Ok(node)
}

fn parse_literal(slice: TokenStreamSlice, idx: &mut usize) -> ToppleResult<Node> {
    let (token, line, chr) = &slice[*idx];
    let left = match token {
        Token::Num(n) => {
            *idx += 1;
            match n {
                Num::Imm(i) => Val::Literal(ToppleType::ByteTable(i.into())),
                Num::Bits(s) => Val::Literal(ToppleType::ByteTable(ByteTable::from_bit_str(&s))),
            }
        }
        Token::Str(s) => {
            *idx += 1;
            Val::Literal(ToppleType::ByteTable(ByteTable::from_str(&s)))
        }
        Token::Ident(s) => {
            *idx += 1;
            Val::Ident(s.to_string())
        }
        Token::LeftBracket => {
            return parse_table(slice, idx, Token::LeftBracket, Token::RightBracket)
        }
        Token::LeftCurly => {
            let mut block_end = *idx + 1;
            if block_end >= slice.len() {
                return Err(ToppleError::OpenBlockError(*line, *chr));
            }
            let mut inner_blocks = 0;
            let (_, mut line, mut chr) = &slice[block_end];
            loop {
                if block_end >= slice.len() {
                    return Err(ToppleError::OpenBlockError(line, chr));
                }
                let (t, l, c) = &slice[block_end];
                line = *l;
                chr = *c;
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
                *idx = block_end + 1;
                Val::Block(Vec::new())
            } else {
                let res = Val::Block(parse_tokens_raw(&slice[(*idx + 1)..block_end])?);
                *idx = block_end + 1;
                res
            }
        }
        Token::LeftParen => {
            let mut expr_end = *idx + 1;
            if expr_end >= slice.len() {
                return Err(ToppleError::OpenParenError(*line, *chr));
            }
            let mut inner_expr = 0;
            let (_, mut line, mut chr) = &slice[expr_end];
            loop {
                if expr_end >= slice.len() {
                    return Err(ToppleError::OpenParenError(line, chr));
                }
                let (t, l, c) = &slice[expr_end];
                line = *l;
                chr = *c;
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
                *idx = expr_end + 1;
                Val::Block(Vec::new())
            } else {
                let res = parse_expr(&slice[(*idx + 1)..expr_end]);
                *idx = expr_end + 1;
                return res;
            }
        }
        Token::Keyword(k) => match k {
            Keyword::Let => {
                return parse_def(slice, idx);
            }
            _ => return Err(ToppleError::UnexpectedToken(token.clone(), *line, *chr)),
        },
        _ => return Err(ToppleError::UnexpectedToken(token.clone(), *line, *chr)),
    };
    let n = Node::new_unary(left, NodeType::Literal, *line, *chr);
    Ok(n)
}

fn parse_index(slice: TokenStreamSlice, idx: &mut usize) -> ToppleResult<Node> {
    let lit = parse_literal(slice, idx)?;
    if *idx >= slice.len() {
        return Ok(lit);
    }
    let left = match lit.left {
        Val::Ident(_) => lit.left.clone(),
        _ => return Ok(lit),
    };
    let (token, line, chr) = &slice[*idx];
    let right = match token {
        Token::LeftBracket => {
            let mut expr_end = *idx + 1;
            if expr_end >= slice.len() {
                return Err(ToppleError::EmptyIndex(*line, *chr));
            }
            let mut inner_expr = 0;
            let (_, mut line, mut chr) = &slice[expr_end];
            loop {
                if expr_end >= slice.len() {
                    return Err(ToppleError::EmptyIndex(line, chr));
                }
                let (t, l, c) = &slice[expr_end];
                line = *l;
                chr = *c;
                if *t == Token::RightBracket {
                    if inner_expr > 0 {
                        inner_expr -= 1;
                    } else {
                        break;
                    }
                } else if *t == Token::LeftBracket {
                    inner_expr += 1;
                }
                expr_end += 1;
            }
            if expr_end == *idx + 1 {
                return Err(ToppleError::EmptyIndex(line, chr));
            } else {
                let res = parse_expr(&slice[(*idx + 1)..expr_end])?;
                *idx = expr_end + 1;
                res
            }
        }
        Token::Dot => {
            *idx += 1;
            parse_literal(slice, idx)?
        }
        _ => return Ok(lit),
    };
    let v = Val::Node(Box::new(right));
    let n = Node::new_binary(left, v, NodeType::Index, *line, *chr);
    Ok(n)
}

fn parse_keyword(slice: TokenStreamSlice, idx: &mut usize) -> ToppleResult<Node> {
    let (token, line, chr) = &slice[*idx];
    let keyword = if let Token::Keyword(k) = token {
        k
    } else {
        unreachable!()
    };
    let left = Val::Keyword(*keyword);
    *idx += 1;
    let right = match slice[*idx].0 {
        Token::LeftParen => {
            let args = match parse_table(slice, idx, Token::RightParen, Token::LeftParen) {
                Ok(r) => r,
                Err(e) => {
                    if let ToppleError::OpenBracketError(l, c) = e {
                        return Err(ToppleError::OpenParenError(l, c));
                    } else {
                        return Err(e);
                    }
                }
            };
            Val::Node(Box::new(args))
        }
        _ => return Err(ToppleError::KeywordIsCall(*keyword, *line, *chr)),
    };
    let node = Node::new_binary(left, right, NodeType::Call, *line, *chr);
    Ok(node)
}

fn parse_call(slice: TokenStreamSlice, idx: &mut usize) -> ToppleResult<Node> {
    match slice[*idx].0 {
        Token::Keyword(k) => match k {
            Keyword::Let => (),
            _ => return parse_keyword(slice, idx),
        },
        _ => (),
    }
    let index = parse_index(slice, idx)?;
    if *idx >= slice.len() {
        return Ok(index);
    }
    let left = if index == NodeType::Index {
        Val::Node(Box::new(index.clone()))
    } else {
        match index.left {
            Val::Ident(_) => index.left.clone(),
            _ => return Ok(index),
        }
    };
    let (token, line, chr) = &slice[*idx];
    let right = match token {
        Token::LeftParen => {
            let n = match parse_table(slice, idx, Token::LeftParen, Token::RightParen) {
                Ok(r) => r,
                Err(e) => {
                    if let ToppleError::OpenBracketError(l, c) = e {
                        return Err(ToppleError::OpenParenError(l, c));
                    } else {
                        return Err(e);
                    }
                }
            };
            Val::Node(Box::new(n))
        }
        _ => return Ok(index),
    };
    let n = Node::new_binary(left, right, NodeType::Call, *line, *chr);
    Ok(n)
}

fn parse_not(slice: TokenStreamSlice, idx: &mut usize) -> ToppleResult<Node> {
    let (token, line, chr) = &slice[*idx];
    match token {
        Token::Op(Op::BitNot) => {
            *idx += 1;
            let n = parse_call(slice, idx)?;
            let v = Val::Node(Box::new(n));
            let node = Node::new_unary(v, NodeType::BitNot, *line, *chr);
            Ok(node)
        }
        _ => parse_call(slice, idx),
    }
}

fn parse_mult(slice: TokenStreamSlice, idx: &mut usize) -> ToppleResult<Node> {
    let left = parse_not(slice, idx)?;
    if *idx >= slice.len() {
        return Ok(left);
    }
    let (token, line, chr) = &slice[*idx];
    if *token != Token::Op(Op::Mult) {
        return Ok(left);
    }
    *idx += 1;
    let right = parse_mult(slice, idx)?;
    let left = Val::Node(Box::new(left));
    let right = Val::Node(Box::new(right));
    let node = Node::new_binary(left, right, NodeType::Mult, *line, *chr);
    Ok(node)
}

fn parse_div(slice: TokenStreamSlice, idx: &mut usize) -> ToppleResult<Node> {
    let left = parse_mult(slice, idx)?;
    if *idx >= slice.len() {
        return Ok(left);
    }
    let (token, line, chr) = &slice[*idx];
    if *token != Token::Op(Op::Div) {
        return Ok(left);
    }
    *idx += 1;
    let right = parse_div(slice, idx)?;
    let left = Val::Node(Box::new(left));
    let right = Val::Node(Box::new(right));
    let node = Node::new_binary(left, right, NodeType::Div, *line, *chr);
    Ok(node)
}

fn parse_mod(slice: TokenStreamSlice, idx: &mut usize) -> ToppleResult<Node> {
    let left = parse_div(slice, idx)?;
    if *idx >= slice.len() {
        return Ok(left);
    }
    let (token, line, chr) = &slice[*idx];
    if *token != Token::Op(Op::Mod) {
        return Ok(left);
    }
    *idx += 1;
    let right = parse_mod(slice, idx)?;
    let left = Val::Node(Box::new(left));
    let right = Val::Node(Box::new(right));
    let node = Node::new_binary(left, right, NodeType::Mod, *line, *chr);
    Ok(node)
}

fn parse_add(slice: TokenStreamSlice, idx: &mut usize) -> ToppleResult<Node> {
    let left = parse_mod(slice, idx)?;
    if *idx >= slice.len() {
        return Ok(left);
    }
    let (token, line, chr) = &slice[*idx];
    if *token != Token::Op(Op::Add) {
        return Ok(left);
    }
    *idx += 1;
    let right = parse_add(slice, idx)?;
    let left = Val::Node(Box::new(left));
    let right = Val::Node(Box::new(right));
    let node = Node::new_binary(left, right, NodeType::Add, *line, *chr);
    Ok(node)
}

fn parse_sub(slice: TokenStreamSlice, idx: &mut usize) -> ToppleResult<Node> {
    let left = parse_add(slice, idx)?;
    if *idx >= slice.len() {
        return Ok(left);
    }
    let (token, line, chr) = &slice[*idx];
    if *token != Token::Op(Op::Sub) {
        return Ok(left);
    }
    *idx += 1;
    let right = parse_sub(slice, idx)?;
    let left = Val::Node(Box::new(left));
    let right = Val::Node(Box::new(right));
    let node = Node::new_binary(left, right, NodeType::Sub, *line, *chr);
    Ok(node)
}

fn parse_and(slice: TokenStreamSlice, idx: &mut usize) -> ToppleResult<Node> {
    let left = parse_sub(slice, idx)?;
    if *idx >= slice.len() {
        return Ok(left);
    }
    let (token, line, chr) = &slice[*idx];
    if *token != Token::Op(Op::BitAnd) {
        return Ok(left);
    }
    *idx += 1;
    let right = parse_and(slice, idx)?;
    let left = Val::Node(Box::new(left));
    let right = Val::Node(Box::new(right));
    let node = Node::new_binary(left, right, NodeType::BitAnd, *line, *chr);
    Ok(node)
}

fn parse_or(slice: TokenStreamSlice, idx: &mut usize) -> ToppleResult<Node> {
    let left = parse_and(slice, idx)?;
    if *idx >= slice.len() {
        return Ok(left);
    }
    let (token, line, chr) = &slice[*idx];
    if *token != Token::Op(Op::BitOr) {
        return Ok(left);
    }
    *idx += 1;
    let right = parse_or(slice, idx)?;
    let left = Val::Node(Box::new(left));
    let right = Val::Node(Box::new(right));
    let node = Node::new_binary(left, right, NodeType::BitOr, *line, *chr);
    Ok(node)
}

fn parse_xor(slice: TokenStreamSlice, idx: &mut usize) -> ToppleResult<Node> {
    let left = parse_or(slice, idx)?;
    if *idx >= slice.len() {
        return Ok(left);
    }
    let (token, line, chr) = &slice[*idx];
    if *token != Token::Op(Op::BitXor) {
        return Ok(left);
    }
    *idx += 1;
    let right = parse_xor(slice, idx)?;
    let left = Val::Node(Box::new(left));
    let right = Val::Node(Box::new(right));
    let node = Node::new_binary(left, right, NodeType::BitXor, *line, *chr);
    Ok(node)
}

fn parse_shift_left(slice: TokenStreamSlice, idx: &mut usize) -> ToppleResult<Node> {
    let left = parse_xor(slice, idx)?;
    if *idx >= slice.len() {
        return Ok(left);
    }
    let (token, line, chr) = &slice[*idx];
    if *token != Token::Op(Op::ShiftLeft) {
        return Ok(left);
    }
    *idx += 1;
    let right = parse_shift_left(slice, idx)?;
    let left = Val::Node(Box::new(left));
    let right = Val::Node(Box::new(right));
    let node = Node::new_binary(left, right, NodeType::ShiftLeft, *line, *chr);
    Ok(node)
}

fn parse_shift_right(slice: TokenStreamSlice, idx: &mut usize) -> ToppleResult<Node> {
    let left = parse_shift_left(slice, idx)?;
    if *idx >= slice.len() {
        return Ok(left);
    }
    let (token, line, chr) = &slice[*idx];
    if *token != Token::Op(Op::ShiftRight) {
        return Ok(left);
    }
    *idx += 1;
    let right = parse_shift_right(slice, idx)?;
    let left = Val::Node(Box::new(left));
    let right = Val::Node(Box::new(right));
    let node = Node::new_binary(left, right, NodeType::ShiftRight, *line, *chr);
    Ok(node)
}

fn parse_pop(slice: TokenStreamSlice, idx: &mut usize) -> ToppleResult<Node> {
    let left = parse_shift_right(slice, idx)?;
    if *idx >= slice.len() {
        return Ok(left);
    }
    let (token, line, chr) = &slice[*idx];
    if *token != Token::Op(Op::Pop) {
        return Ok(left);
    }
    *idx += 1;
    let v = Val::Node(Box::new(left));
    let n = Node::new_unary(v, NodeType::Pop, *line, *chr);
    Ok(n)
}

fn parse_push(slice: TokenStreamSlice, idx: &mut usize) -> ToppleResult<Node> {
    let left = parse_pop(slice, idx)?;
    if *idx >= slice.len() {
        return Ok(left);
    }
    let (token, line, chr) = &slice[*idx];
    if *token != Token::Op(Op::Push) {
        return Ok(left);
    }
    *idx += 1;
    let right = parse_push(slice, idx)?;
    let left = Val::Node(Box::new(left));
    let right = Val::Node(Box::new(right));
    let node = Node::new_binary(left, right, NodeType::Push, *line, *chr);
    Ok(node)
}

fn parse_gt(slice: TokenStreamSlice, idx: &mut usize) -> ToppleResult<Node> {
    let left = parse_push(slice, idx)?;
    if *idx >= slice.len() {
        return Ok(left);
    }
    let (token, line, chr) = &slice[*idx];
    if *token != Token::Op(Op::Greater) {
        return Ok(left);
    }
    *idx += 1;
    let right = parse_gt(slice, idx)?;
    let left = Val::Node(Box::new(left));
    let right = Val::Node(Box::new(right));
    let node = Node::new_binary(left, right, NodeType::CmpGT, *line, *chr);
    Ok(node)
}

fn parse_lt(slice: TokenStreamSlice, idx: &mut usize) -> ToppleResult<Node> {
    let left = parse_gt(slice, idx)?;
    if *idx >= slice.len() {
        return Ok(left);
    }
    let (token, line, chr) = &slice[*idx];
    if *token != Token::Op(Op::Less) {
        return Ok(left);
    }
    *idx += 1;
    let right = parse_lt(slice, idx)?;
    let left = Val::Node(Box::new(left));
    let right = Val::Node(Box::new(right));
    let node = Node::new_binary(left, right, NodeType::CmpLT, *line, *chr);
    Ok(node)
}

fn parse_gte(slice: TokenStreamSlice, idx: &mut usize) -> ToppleResult<Node> {
    let left = parse_lt(slice, idx)?;
    if *idx >= slice.len() {
        return Ok(left);
    }
    let (token, line, chr) = &slice[*idx];
    if *token != Token::Op(Op::GreaterEq) {
        return Ok(left);
    }
    *idx += 1;
    let right = parse_gte(slice, idx)?;
    let left = Val::Node(Box::new(left));
    let right = Val::Node(Box::new(right));
    let node = Node::new_binary(left, right, NodeType::CmpGTE, *line, *chr);
    Ok(node)
}

fn parse_lte(slice: TokenStreamSlice, idx: &mut usize) -> ToppleResult<Node> {
    let left = parse_gte(slice, idx)?;
    if *idx >= slice.len() {
        return Ok(left);
    }
    let (token, line, chr) = &slice[*idx];
    if *token != Token::Op(Op::LessEq) {
        return Ok(left);
    }
    *idx += 1;
    let right = parse_lte(slice, idx)?;
    let left = Val::Node(Box::new(left));
    let right = Val::Node(Box::new(right));
    let node = Node::new_binary(left, right, NodeType::CmpLTE, *line, *chr);
    Ok(node)
}

fn parse_eq(slice: TokenStreamSlice, idx: &mut usize) -> ToppleResult<Node> {
    let left = parse_lte(slice, idx)?;
    if *idx >= slice.len() {
        return Ok(left);
    }
    let (token, line, chr) = &slice[*idx];
    if *token != Token::Op(Op::Eq) {
        return Ok(left);
    }
    *idx += 1;
    let right = parse_eq(slice, idx)?;
    let left = Val::Node(Box::new(left));
    let right = Val::Node(Box::new(right));
    let node = Node::new_binary(left, right, NodeType::CmpEQ, *line, *chr);
    Ok(node)
}

fn parse_ne(slice: TokenStreamSlice, idx: &mut usize) -> ToppleResult<Node> {
    let left = parse_eq(slice, idx)?;
    if *idx >= slice.len() {
        return Ok(left);
    }
    let (token, line, chr) = &slice[*idx];
    if *token != Token::Op(Op::NotEq) {
        return Ok(left);
    }
    *idx += 1;
    let right = parse_ne(slice, idx)?;
    let left = Val::Node(Box::new(left));
    let right = Val::Node(Box::new(right));
    let node = Node::new_binary(left, right, NodeType::CmpNE, *line, *chr);
    Ok(node)
}

fn parse_assign(slice: TokenStreamSlice, idx: &mut usize) -> ToppleResult<Node> {
    let idx_save = *idx;
    let index = parse_index(slice, idx)?;
    if *idx >= slice.len() {
        return Ok(index);
    }
    let left = if index == NodeType::Index {
        Val::Node(Box::new(index))
    } else {
        match index.left {
            Val::Ident(_) => index.left,
            _ => {
                *idx = idx_save;
                return parse_ne(slice, idx);
            }
        }
    };
    let (token, _, _) = &slice[*idx];
    if *token != Token::Op(Op::Assign) {
        *idx = idx_save;
        return parse_ne(slice, idx);
    }
    *idx += 1;
    let right = parse_expr(&slice[*idx..])?;
    *idx = slice.len();
    let right = Val::Node(Box::new(right));
    let (_, line, chr) = &slice[idx_save];
    let node = Node::new_binary(left, right, NodeType::Assign, *line, *chr);
    Ok(node)
}

fn parse_expr(expr: TokenStreamSlice) -> ToppleResult<Node> {
    let mut idx = 0;
    let node = parse_assign(expr, &mut idx)?;
    if idx < expr.len() {
        let (_, line, chr) = &expr[0];
        let (_, end_l, end_c) = &expr[idx];
        return Err(ToppleError::ExprPartialParse(*line, *chr, *end_l, *end_c));
    }
    Ok(node)
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
            Self::Keyword(k) => write!(f, "{k}"),
            Self::Node(n) => write!(f, "{n}"),
            Self::Block(b) => write!(f, "{}", block_print(&b, 0)),
            Self::Table(t) => write!(f, "{t:?}"),
        }
    }
}

fn block_print(block: &AST, depth: usize) -> String {
    let curly_indent = depth * 4;
    let indent = (depth + 1) * 4;
    let mut s = String::new();
    s += &format!("{:1$}{{\n", "", curly_indent);
    for n in block.iter() {
        if let Val::Block(b) = &n.left {
            s += &block_print(b, depth + 1);
            s.push('\n');
        } else {
            s += &format!("{1:2$}{0}\n", n, "", indent);
        }
    }
    s += &format!("{:1$}}}", "", curly_indent);
    s
}

fn block_print_binary(block: &AST, depth: usize) -> String {
    let curly_indent = depth * 4;
    let indent = (depth + 1) * 4;
    let mut s = String::new();
    s += &format!("{:1$}{{\n", "", curly_indent);
    for n in block.iter() {
        if let Val::Block(b) = &n.left {
            s += &block_print_binary(b, depth + 1);
            s.push('\n');
        } else {
            s += &format!("{1:2$}{0:b}\n", n, "", indent);
        }
    }
    s += &format!("{:1$}}}", "", curly_indent);
    s
}

impl Binary for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(l) => write!(f, "{l}"),
            Self::Ident(s) => write!(f, "{s}"),
            Self::Keyword(k) => write!(f, "{k}"),
            Self::Node(n) => write!(f, "{n}"),
            Self::Block(b) => write!(f, "{}", block_print_binary(&b, 0)),
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
        let s = match self.node_type {
            NodeType::Literal => return write!(f, "{}", self.left),
            NodeType::Index => format!("{}.{}", self.left, right),
            NodeType::Call => format!("{}({})", self.left, right),
            NodeType::BitNot => format!("!{}", self.left),
            NodeType::Mult => format!("{} * {}", self.left, right),
            NodeType::Div => format!("{} / {}", self.left, right),
            NodeType::Mod => format!("{} % {}", self.left, right),
            NodeType::Add => format!("{} + {}", self.left, right),
            NodeType::Sub => format!("{} - {}", self.left, right),
            NodeType::BitAnd => format!("{} & {}", self.left, right),
            NodeType::BitOr => format!("{} | {}", self.left, right),
            NodeType::BitXor => format!("{} ^ {}", self.left, right),
            NodeType::ShiftLeft => format!("{} << {}", self.left, right),
            NodeType::ShiftRight => format!("{} >> {}", self.left, right),
            NodeType::Pop => format!("{}--", self.left),
            NodeType::Push => format!("{} ++ {}", self.left, right),
            NodeType::CmpGT => format!("{} > {}", self.left, right),
            NodeType::CmpLT => format!("{} < {}", self.left, right),
            NodeType::CmpGTE => format!("{} >= {}", self.left, right),
            NodeType::CmpLTE => format!("{} <= {}", self.left, right),
            NodeType::CmpEQ => format!("{} == {}", self.left, right),
            NodeType::CmpNE => format!("{} != {}", self.left, right),
            NodeType::Assign => format!("{} = {}", self.left, right),
            NodeType::Def => format!("let {}", self.left),
            NodeType::Block => format!("{}", self.left),
        };
        write!(f, "({s})")
    }
}

impl Binary for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let right = match &self.right {
            Some(v) => format!("{v:b}"),
            None => "WARNING: Right side empty".into(),
        };
        let s = match self.node_type {
            NodeType::Literal => return write!(f, "{:b}", self.left),
            NodeType::Index => format!("{:b}.{}", self.left, right),
            NodeType::Call => format!("{:b}({})", self.left, right),
            NodeType::BitNot => format!("!{:b}", self.left),
            NodeType::Mult => format!("{:b} * {}", self.left, right),
            NodeType::Div => format!("{:b} / {}", self.left, right),
            NodeType::Mod => format!("{:b} % {}", self.left, right),
            NodeType::Add => format!("{:b} + {}", self.left, right),
            NodeType::Sub => format!("{:b} - {}", self.left, right),
            NodeType::BitAnd => format!("{:b} & {}", self.left, right),
            NodeType::BitOr => format!("{:b} | {}", self.left, right),
            NodeType::BitXor => format!("{:b} ^ {}", self.left, right),
            NodeType::ShiftLeft => format!("{:b} << {}", self.left, right),
            NodeType::ShiftRight => format!("{:b} >> {}", self.left, right),
            NodeType::Pop => format!("{:b}--", self.left),
            NodeType::Push => format!("{:b} ++ {}", self.left, right),
            NodeType::CmpGT => format!("{:b} > {}", self.left, right),
            NodeType::CmpLT => format!("{:b} < {}", self.left, right),
            NodeType::CmpGTE => format!("{:b} >= {}", self.left, right),
            NodeType::CmpLTE => format!("{:b} <= {}", self.left, right),
            NodeType::CmpEQ => format!("{:b} == {}", self.left, right),
            NodeType::CmpNE => format!("{:b} != {}", self.left, right),
            NodeType::Assign => format!("{:b} = {}", self.left, right),
            NodeType::Def => format!("let {:b}", self.left),
            NodeType::Block => format!("{:b}", self.left),
        };
        write!(f, "({s})")
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
    fn bitwise_ops() {
        let buf = "0b1100 ^ 0b0101 & !0b0100 | 0b0100;";
        let stream = lex(buf.as_bytes()).unwrap();
        let out = parse_tokens(stream).unwrap();
        assert_eq!(bitwise_node(), out[0]);
    }

    #[test]
    fn op_order() {
        let buf = "3 + 5 >> 6 / 3;";
        let stream = lex(buf.as_bytes()).unwrap();
        let out = parse_tokens(stream).unwrap();
        assert_eq!(order_node(), out[0]);
    }

    #[test]
    fn def_statement() {
        let buf = "let a; let b = 3;";
        let stream = lex(buf.as_bytes()).unwrap();
        let out = parse_tokens(stream).unwrap();

        let def = Node::new_unary(ident("a", 0, 4), NodeType::Def, 0, 0);
        assert_eq!(def, out[0]);

        let assign = Node::new_binary(
            Val::Ident("b".into()),
            num_lit(3, 0, 15),
            NodeType::Assign,
            0,
            11,
        );
        let def_assign = Node::new_unary(node_val(assign), NodeType::Def, 0, 7);
        assert_eq!(def_assign, out[1]);
    }

    #[test]
    fn def_immediate() {
        let buf = "let 5 = 4;";
        let stream = lex(buf.as_bytes()).unwrap();
        let out = parse_tokens(stream);
        assert_eq!(Err(ToppleError::HangingLetError(0, 0)), out);
    }

    #[test]
    fn paren_expr() {
        let buf = "3 * (2 + 4) - 4;";
        let stream = lex(buf.as_bytes()).unwrap();
        let out = parse_tokens(stream).unwrap();

        let paren = Node::new_binary(num_lit(2, 0, 5), num_lit(4, 0, 9), NodeType::Add, 0, 5);
        let mult = Node::new_binary(num_lit(3, 0, 0), node_val(paren), NodeType::Mult, 0, 0);
        let sub = Node::new_binary(node_val(mult), num_lit(4, 0, 14), NodeType::Sub, 0, 0);
        assert_eq!(sub, out[0]);
    }

    #[test]
    fn print_node() {
        let node = math_node();
        let s = "((3 + ((2 * 5) / 5)) - 1)";
        assert_eq!(s, format!("{node}"));
    }

    #[test]
    fn cmp_ops() {
        let buf = "test_val == 3 > 0 != 0;";
        let stream = lex(buf.as_bytes()).unwrap();
        let out = parse_tokens(stream).unwrap();
        assert_eq!(cmp_node(), out[0]);
    }

    #[test]
    fn blocked_ast() {
        let buf = "let a = {let b = 2; b + 3;};";
        let stream = lex(buf.as_bytes()).unwrap();
        let out = parse_tokens(stream).unwrap();
        assert_eq!(block_node(), out[0]);
    }

    fn block_node() -> Node {
        // let a = {let b = 2; b + 3;};
        let mut block = Vec::new();
        let b_assign = Node::new_binary(
            Val::Ident("b".into()),
            num_lit(2, 0, 17),
            NodeType::Assign,
            0,
            9,
        );
        let b_def = Node::new_unary(node_val(b_assign), NodeType::Def, 0, 9);
        block.push(b_def);
        let add = Node::new_binary(ident("b", 0, 20), num_lit(3, 0, 24), NodeType::Add, 0, 20);
        block.push(add);
        let block = Node::new_unary(Val::Block(block), NodeType::Literal, 0, 8);
        let a_assign = Node::new_binary(
            Val::Ident("a".into()),
            node_val(block),
            NodeType::Assign,
            0,
            0,
        );
        let a_def = Node::new_unary(node_val(a_assign), NodeType::Def, 0, 0);
        a_def
    }

    fn cmp_node() -> Node {
        // test_val == 3 > 0 != 0;
        let gt = Node::new_binary(num_lit(3, 0, 12), num_lit(0, 0, 16), NodeType::CmpGT, 0, 12);
        let eq = Node::new_binary(ident("test_val", 0, 0), node_val(gt), NodeType::CmpEQ, 0, 0);
        let ne = Node::new_binary(node_val(eq), num_lit(0, 0, 21), NodeType::CmpNE, 0, 0);
        ne
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
        let val = Val::Literal(ToppleType::ByteTable(ByteTable::from_bit_str(s)));
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
