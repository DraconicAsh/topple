use crate::error::*;
use crate::lexer::*;
use crate::types::ByteTable;
use std::fmt::Display;

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
    pub fn new_unary(left: Val, node_type: NodeType, line: usize, chr: usize) -> Self {
        Self {
            left,
            right: None,
            node_type,
            line,
            chr,
        }
    }

    pub fn new_binary(left: Val, right: Val, node_type: NodeType, line: usize, chr: usize) -> Self {
        Self {
            left,
            right: Some(right),
            node_type,
            line,
            chr,
        }
    }

    pub fn left(&self) -> &Val {
        &self.left
    }

    pub fn right(&self) -> Option<&Val> {
        self.right.as_ref()
    }

    pub fn node_type(&self) -> NodeType {
        self.node_type
    }

    pub fn pos(&self) -> (usize, usize) {
        (self.line, self.chr)
    }

    pub(crate) fn destructure(self) -> (Val, Option<Val>, NodeType, usize, usize) {
        let Self {
            left: l,
            right: r,
            node_type: t,
            line: ln,
            chr: c,
        } = self;
        (l, r, t, ln, c)
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
    For,
    Block,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Val {
    ByteTable(ByteTable),
    Ident(Ident),
    Keyword(Keyword),
    Node(Box<Node>),
    Table(Vec<Node>),
    Block(AST),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Ident {
    Var(String),
    Keyword(Keyword),
}

impl std::convert::From<&str> for Ident {
    fn from(value: &str) -> Self {
        Self::Var(value.to_string())
    }
}

impl std::fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Var(s) => write!(f, "{s}"),
            Self::Keyword(k) => write!(f, "{k}"),
        }
    }
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
        let mut open_blocks = match &stream[expr_start].0 {
            Token::LeftCurly => 1,
            _ => 0,
        };
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
    let v = Val::Table(table);
    let n = Node::new_unary(v, NodeType::Literal, line, chr);
    Ok(n)
}

fn parse_for(slice: TokenStreamSlice, idx: &mut usize) -> ToppleResult<Node> {
    let (_, for_line, for_chr) = &slice[*idx];
    *idx += 1;
    let (token, line, chr) = &slice[*idx];
    let table = parse_index(slice, idx)?;
    let left = if table == NodeType::Index {
        Val::Node(Box::new(table))
    } else {
        if table == NodeType::Literal {
            match table.left {
                Val::Ident(_) => table.left,
                _ => return Err(ToppleError::UnexpectedToken(token.clone(), *line, *chr)),
            }
        } else {
            return Err(ToppleError::UnexpectedToken(token.clone(), *line, *chr));
        }
    };
    let (token, line, chr) = &slice[*idx];
    let block = parse_literal(slice, idx)?;
    let right = if block == NodeType::Index {
        Val::Node(Box::new(block))
    } else {
        if block == NodeType::Literal {
            match block.left {
                Val::Ident(_) | Val::Block(_) => block.left,
                _ => return Err(ToppleError::UnexpectedToken(token.clone(), *line, *chr)),
            }
        } else {
            return Err(ToppleError::UnexpectedToken(token.clone(), *line, *chr));
        }
    };
    let node = Node::new_binary(left, right, NodeType::For, *for_line, *for_chr);
    Ok(node)
}

fn parse_literal(slice: TokenStreamSlice, idx: &mut usize) -> ToppleResult<Node> {
    let (token, line, chr) = &slice[*idx];
    let left = match token {
        Token::Num(n) => {
            *idx += 1;
            match n {
                Num::Imm(i) => Val::ByteTable(Into::into(*i)),
                Num::Bits(s) => Val::ByteTable(ByteTable::from_bit_str(&s)),
            }
        }
        Token::Str(s) => {
            *idx += 1;
            Val::ByteTable(ByteTable::from_str(&s))
        }
        Token::Ident(s) => {
            *idx += 1;
            Val::Ident(Ident::Var(s.to_string()))
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
            Keyword::For => {
                return parse_for(slice, idx);
            }
            Keyword::Args | Keyword::SelfK => {
                *idx += 1;
                Val::Ident(Ident::Keyword(*k))
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
            let args = match parse_table(slice, idx, Token::LeftParen, Token::RightParen) {
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
            Keyword::Args | Keyword::For | Keyword::SelfK => (),
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
            Val::Ident(_) | Val::Block(_) => index.left.clone(),
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
    let index = match parse_index(slice, idx) {
        Ok(i) => i,
        Err(_) => {
            *idx = idx_save;
            return parse_ne(slice, idx);
        }
    };
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
    let mut node = parse_assign(expr, &mut idx)?;
    if idx < expr.len() {
        let (_, line, chr) = &expr[0];
        let (_, end_l, end_c) = &expr[idx];
        return Err(ToppleError::ExprPartialParse(*line, *chr, *end_l, *end_c));
    }
    make_literals_direct(&mut node);
    Ok(node)
}

fn make_literals_direct(node: &mut Node) {
    match node.left {
        Val::Node(ref mut n) => {
            if **n == NodeType::Literal {
                node.left = n.left.clone();
            } else {
                make_literals_direct(&mut *n);
            }
        }
        _ => (),
    }
    if let Some(ref mut v) = node.right {
        match v {
            Val::Node(ref mut n) => {
                if **n == NodeType::Literal {
                    *v = n.left.clone();
                } else {
                    make_literals_direct(&mut *n);
                }
            }
            _ => (),
        }
    }
}

impl Display for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ByteTable(t) => match t.as_unsigned() {
                Some(n) => write!(f, "{n}"),
                None => write!(f, "{t}"),
            },
            Self::Ident(s) => write!(f, "{s}"),
            Self::Keyword(k) => write!(f, "{k}"),
            Self::Node(n) => write!(f, "{n}"),
            Self::Block(b) => write!(f, "{}", block_print(&b, 0)),
            Self::Table(t) => write!(f, "{t:?}"),
        }
    }
}

pub(crate) fn block_print(block: &AST, depth: usize) -> String {
    let curly_indent = depth * 4;
    let indent = (depth + 1) * 4;
    let mut s = String::new();
    s.push('{');
    for n in block.iter() {
        if let Val::Block(b) = &n.left {
            s += &block_print(b, depth + 1);
        } else {
            s += &format!("\n{1:2$}{0}", n, "", indent);
        }
    }
    s += &format!("\n{:1$}}}", "", curly_indent);
    s
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
            NodeType::For => format!("for {} {}", self.left, right),
            NodeType::Block => return write!(f, "{}", self.left),
        };
        write!(f, "({s})")
    }
}

#[cfg(test)]
mod parser_tests {
    use super::*;
    use std::convert::From;

    impl From<u64> for Val {
        fn from(value: u64) -> Self {
            Val::ByteTable(value.into())
        }
    }

    impl From<&str> for Val {
        fn from(value: &str) -> Self {
            Val::Ident(value.into())
        }
    }

    impl From<Node> for Val {
        fn from(value: Node) -> Self {
            Val::Node(Box::new(value))
        }
    }

    impl From<Keyword> for Val {
        fn from(value: Keyword) -> Self {
            Val::Keyword(value)
        }
    }

    #[test]
    fn math_ops() {
        let buf = "3 + 2 * 5 / 5 - 1;";
        let out = parse_test_input(buf);
        assert_eq!(math_node(), out[0]);
    }

    fn math_node() -> Node {
        let mult = Node::new_binary(2.into(), 5.into(), NodeType::Mult, 0, 4);
        let div = Node::new_binary(mult.into(), 5.into(), NodeType::Div, 0, 4);
        let add = Node::new_binary(3.into(), div.into(), NodeType::Add, 0, 0);
        let sub = Node::new_binary(add.into(), 1.into(), NodeType::Sub, 0, 0);
        sub
    }

    #[test]
    fn bitwise_ops() {
        let buf = "0b1100 ^ 0b0101 & !0b0100 | 0b0100;";
        let out = parse_test_input(buf);
        assert_eq!(bitwise_node(), out[0]);
    }

    fn bitwise_node() -> Node {
        let not = Node::new_unary(binary_lit("0100"), NodeType::BitNot, 0, 18);
        let and = Node::new_binary(binary_lit("0101"), not.into(), NodeType::BitAnd, 0, 9);
        let or = Node::new_binary(and.into(), binary_lit("0100"), NodeType::BitOr, 0, 9);
        let xor = Node::new_binary(binary_lit("1100"), or.into(), NodeType::BitXor, 0, 0);
        xor
    }

    #[test]
    fn op_order() {
        let buf = "3 + 5 >> 6 / 3;";
        let out = parse_test_input(buf);
        assert_eq!(order_node(), out[0]);
    }

    fn order_node() -> Node {
        let div = Node::new_binary(6.into(), 3.into(), NodeType::Div, 0, 9);
        let add = Node::new_binary(3.into(), 5.into(), NodeType::Add, 0, 0);
        let shift_r = Node::new_binary(add.into(), div.into(), NodeType::ShiftRight, 0, 0);
        shift_r
    }

    #[test]
    fn paren_expr() {
        let buf = "3 * (2 + 4) - 4;";
        let out = parse_test_input(buf);

        let paren = Node::new_binary(2.into(), 4.into(), NodeType::Add, 0, 5);
        let mult = Node::new_binary(3.into(), paren.into(), NodeType::Mult, 0, 0);
        let sub = Node::new_binary(mult.into(), 4.into(), NodeType::Sub, 0, 0);
        assert_eq!(sub, out[0]);
    }

    #[test]
    fn print_node() {
        let node = math_node();
        let s = "((3 + ((2 * 5) / 5)) - 1)";
        assert_eq!(s, format!("{node}"));
    }

    #[test]
    fn print_block() {
        let buf = r"{a * 2;{b / 3;};};";
        let out = parse_test_input(buf);
        let s = "{\n    (a * 2){\n        (b / 3)\n    }\n}";
        assert_eq!(s, format!("{}", out[0]));
    }

    #[test]
    fn cmp_ops() {
        let buf = "test_val == 3 > 0 != 0;";
        let out = parse_test_input(buf);
        assert_eq!(cmp_node(), out[0]);
    }

    fn cmp_node() -> Node {
        let gt = Node::new_binary(3.into(), 0.into(), NodeType::CmpGT, 0, 12);
        let eq = Node::new_binary("test_val".into(), gt.into(), NodeType::CmpEQ, 0, 0);
        let ne = Node::new_binary(eq.into(), 0.into(), NodeType::CmpNE, 0, 0);
        ne
    }

    #[test]
    fn blocked_ast() {
        let buf = "a = {b = 2; b + 3;};";
        let out = parse_test_input(buf);
        assert_eq!(block_node(), out[0]);
    }

    fn block_node() -> Node {
        let mut block = Vec::with_capacity(2);
        let b_assign = Node::new_binary("b".into(), 2.into(), NodeType::Assign, 0, 13);
        block.push(b_assign);
        let add = Node::new_binary("b".into(), 3.into(), NodeType::Add, 0, 20);
        block.push(add);
        let a_assign = Node::new_binary("a".into(), Val::Block(block), NodeType::Assign, 0, 4);
        a_assign
    }

    #[test]
    fn table_def() {
        let buf = "[0, 3 + 2, x, y,];";
        let out = parse_test_input(buf);
        assert_eq!(table_node(), out[0]);
    }

    fn table_node() -> Node {
        let mut table = Vec::with_capacity(4);
        let imm = Node::new_unary(0.into(), NodeType::Literal, 0, 1);
        table.push(imm);
        let add = Node::new_binary(3.into(), 2.into(), NodeType::Add, 0, 4);
        table.push(add);
        let ident = Node::new_unary("x".into(), NodeType::Literal, 0, 11);
        table.push(ident);
        let trailing_comma = Node::new_unary("y".into(), NodeType::Literal, 0, 14);
        table.push(trailing_comma);
        let table = Node::new_unary(Val::Table(table), NodeType::Literal, 0, 0);
        table
    }

    #[test]
    fn index_ops() {
        let buf = "a.0;\nb[1];\nc.d;\ne[f];";
        let out = parse_test_input(buf);
        let test_nodes = index_nodes();
        assert_eq!(test_nodes[0], out[0]);
        assert_eq!(test_nodes[1], out[1]);
        assert_eq!(test_nodes[2], out[2]);
        assert_eq!(test_nodes[3], out[3]);
    }

    fn index_nodes() -> AST {
        let mut res = Vec::with_capacity(4);
        let dot_imm = Node::new_binary("a".into(), 0.into(), NodeType::Index, 0, 0);
        res.push(dot_imm);
        let bracket_imm = Node::new_binary("b".into(), 1.into(), NodeType::Index, 1, 0);
        res.push(bracket_imm);
        let dot_ident = Node::new_binary("c".into(), "d".into(), NodeType::Index, 2, 0);
        res.push(dot_ident);
        let bracket_ident = Node::new_binary("e".into(), "f".into(), NodeType::Index, 3, 0);
        res.push(bracket_ident);
        res
    }

    #[test]
    fn call_ident() {
        let buf = "func(0, 3 + 4, a.0, b,);";
        let out = parse_test_input(buf);
        assert_eq!(call_node(), out[0]);
    }

    fn call_node() -> Node {
        let mut table = Vec::with_capacity(4);
        let imm = Node::new_unary(0.into(), NodeType::Literal, 0, 5);
        table.push(imm);
        let add = Node::new_binary(3.into(), 4.into(), NodeType::Add, 0, 8);
        table.push(add);
        let idx = Node::new_binary("a".into(), 0.into(), NodeType::Index, 0, 15);
        table.push(idx);
        let ident = Node::new_unary("b".into(), NodeType::Literal, 0, 20);
        table.push(ident);
        let call = Node::new_binary("func".into(), Val::Table(table), NodeType::Call, 0, 0);
        call
    }

    #[test]
    fn call_anonymous() {
        let buf = "{x = args[0] * 3; x + args[1];}(3, 1);";
        let out = parse_test_input(buf);
        assert_eq!(anonymous_node(), out[0]);
    }

    fn anonymous_node() -> Node {
        let mut block = Vec::with_capacity(2);
        let args = Val::Ident(Ident::Keyword(Keyword::Args));
        let index = Node::new_binary(args.clone(), 0.into(), NodeType::Index, 0, 9);
        let mult = Node::new_binary(index.into(), 3.into(), NodeType::Mult, 0, 9);
        let assign = Node::new_binary("x".into(), mult.into(), NodeType::Assign, 0, 5);
        block.push(assign);
        let index = Node::new_binary(args, 1.into(), NodeType::Index, 0, 26);
        let add = Node::new_binary("x".into(), index.into(), NodeType::Add, 0, 22);
        block.push(add);
        let block = Val::Block(block);

        let arg0 = Node::new_unary(3.into(), NodeType::Literal, 0, 36);
        let arg1 = Node::new_unary(1.into(), NodeType::Literal, 0, 39);
        let table = Val::Table(vec![arg0, arg1]);

        Node::new_binary(block, table, NodeType::Call, 0, 0)
    }

    #[test]
    fn call_built_in() {
        let buf = r#"print("Hello!");"#;
        let out = parse_test_input(buf);
        assert_eq!(built_in_node(), out[0]);
    }

    fn built_in_node() -> Node {
        let s = ByteTable::from_str("Hello!");
        let v = Val::ByteTable(s);
        let n = Node::new_unary(v, NodeType::Literal, 0, 6);
        let table: AST = vec![n];
        let call = Node::new_binary(
            Keyword::Print.into(),
            Val::Table(table),
            NodeType::Call,
            0,
            0,
        );
        call
    }

    #[test]
    fn pop_and_push() {
        let buf = "tab--;\ntab ++ 2010;";
        let out = parse_test_input(buf);

        let pop = Node::new_unary("tab".into(), NodeType::Pop, 0, 0);
        assert_eq!(pop, out[0]);

        let push = Node::new_binary("tab".into(), 2010.into(), NodeType::Push, 1, 0);
        assert_eq!(push, out[1]);
    }

    #[test]
    fn for_loop() {
        let buf = "for tab {other_tab ++ args[0];};";
        let out = parse_test_input(buf);
        assert_eq!(for_node(), out[0]);
    }

    fn for_node() -> Node {
        let index = Node::new_binary(
            Val::Ident(Ident::Keyword(Keyword::Args)),
            0.into(),
            NodeType::Index,
            0,
            22,
        );
        let push = Node::new_binary("other_tab".into(), index.into(), NodeType::Push, 0, 9);
        let block = vec![push];
        let for_loop = Node::new_binary("tab".into(), Val::Block(block), NodeType::For, 0, 0);
        for_loop
    }

    #[test]
    fn args_and_self() {
        let buf = "args[0] ++ self;";
        let out = parse_test_input(buf);

        let args = Node::new_binary(
            Val::Ident(Ident::Keyword(Keyword::Args)),
            0.into(),
            NodeType::Index,
            0,
            0,
        );
        let push = Node::new_binary(
            args.into(),
            Val::Ident(Ident::Keyword(Keyword::SelfK)),
            NodeType::Push,
            0,
            0,
        );
        assert_eq!(push, out[0]);
    }

    #[test]
    fn recursive_self() {
        let buf = "self(args[0] - 1);";
        let out = parse_test_input(buf);
        assert_eq!(recursive_node(), out[0]);
    }

    fn recursive_node() -> Node {
        let args = Node::new_binary(
            Val::Ident(Ident::Keyword(Keyword::Args)),
            0.into(),
            NodeType::Index,
            0,
            5,
        );
        let sub = Node::new_binary(args.into(), 1.into(), NodeType::Sub, 0, 5);
        let call = Node::new_binary(
            Val::Ident(Ident::Keyword(Keyword::SelfK)),
            Val::Table(vec![sub]),
            NodeType::Call,
            0,
            0,
        );
        call
    }

    fn parse_test_input(input: &str) -> AST {
        let stream = match lex(input.as_bytes()) {
            Ok(s) => s,
            Err(e) => panic!("{e}"),
        };
        match parse_tokens(stream) {
            Ok(t) => t,
            Err(e) => panic!("{e}"),
        }
    }

    fn binary_lit(s: &str) -> Val {
        Val::ByteTable(ByteTable::from_bit_str(s))
    }
}
