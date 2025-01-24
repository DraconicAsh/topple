use super::*;
use crate::error::*;
use std::collections::HashMap;

pub struct SSAConverter {
    ast: crate::parser::AST,
    scopes: Vec<HashMap<String, usize>>,
}

impl SSAConverter {
    pub fn new(ast: crate::parser::AST) -> Self {
        Self {
            ast,
            scopes: Vec::new(),
        }
    }

    pub fn convert(self) -> ToppleResult<SlimAST> {
        todo!()
    }
}

#[cfg(test)]
mod ssa_tests {
    use super::*;
    use crate::lexer::lex;
    use crate::parser::*;

    #[test]
    fn basic_ssa() {
        let data = test_input("x = 0;\nx = x + 3;\ny = x + 2;");

        let mut test = Vec::with_capacity(3);
        let x0_ass = SlimNode::new_binary(ident("x0"), num(0), NodeType::Assign);
        test.push(x0_ass);
        let add = SlimNode::new_binary(ident("x0"), num(3), NodeType::Add);
        let x1_ass = SlimNode::new_binary(ident("x1"), node(add), NodeType::Assign);
        test.push(x1_ass);
        let add = SlimNode::new_binary(ident("x1"), num(2), NodeType::Add);
        let y0_ass = SlimNode::new_binary(ident("y0"), node(add), NodeType::Assign);
        test.push(y0_ass);

        assert_eq!(test, data);
    }

    #[test]
    fn implicit_args() {
        let data = test_input("x = 23;\ny = {x = x / 2; y = x + 5; y;}();\nx + y;");

        let mut test = Vec::with_capacity(3);
        let x0_ass = SlimNode::new_binary(ident("x0"), num(23), NodeType::Assign);
        test.push(x0_ass);
        let y0_ass = SlimNode::new_binary(ident("y0"), implicit_block(), NodeType::Assign);
        test.push(y0_ass);
        let add = SlimNode::new_binary(ident("x0"), ident("y0"), NodeType::Add);
        test.push(add);

        assert_eq!(test, data);
    }

    fn implicit_block() -> SlimVal {
        let mut block = Vec::with_capacity(3);
        let div = SlimNode::new_binary(ident("*x0"), num(2), NodeType::Div);
        let block_x0 = SlimNode::new_binary(ident("x0"), node(div), NodeType::Assign);
        block.push(block_x0);
        let add = SlimNode::new_binary(ident("x0"), num(5), NodeType::Add);
        let block_y0 = SlimNode::new_binary(ident("y0"), node(add), NodeType::Assign);
        block.push(block_y0);
        let return_y0 = SlimNode::new_unary(ident("y0"), NodeType::Literal);
        block.push(return_y0);
        let block = Block::with_args(block, vec!["x0".into()]);
        SlimVal::Block(block)
    }

    #[test]
    #[should_panic(expected = "before it has a value assigned")]
    fn unassigned_var() {
        test_input("x = 2 * 2; y = a * 2;");
    }

    fn node(node: SlimNode) -> SlimVal {
        SlimVal::Node(Box::new(node))
    }

    fn num(n: u64) -> SlimVal {
        SlimVal::ByteTable(n.into())
    }

    fn ident(s: &str) -> SlimVal {
        SlimVal::Ident(Ident::Var(s.into()))
    }

    fn test_input(input: &str) -> SlimAST {
        let stream = match lex(input.as_bytes()) {
            Ok(s) => s,
            Err(e) => panic!("{e}"),
        };
        let ast = match parse_tokens(stream) {
            Ok(a) => a,
            Err(e) => panic!("{e}"),
        };
        match SSAConverter::new(ast).convert() {
            Ok(s) => s,
            Err(e) => panic!("{e}"),
        }
    }
}
