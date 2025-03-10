use super::*;
use crate::error::*;
use crate::parser::{Ident, NodeType, Val};
use std::collections::HashMap;

type Scope = HashMap<String, usize>;

pub fn ssa_conversion(ast: crate::parser::AST) -> ToppleResult<SlimAST> {
    let mut scope: Scope = HashMap::new();
    convert_ast(&mut scope, ast)
}

fn convert_ast(scope: &mut Scope, ast: crate::parser::AST) -> ToppleResult<SlimAST> {
    let mut res: SlimAST = Vec::with_capacity(ast.len());
    for node in ast.into_iter() {
        println!("{node}");
        let n = convert_node(scope, node)?;
        res.push(n);
    }
    Ok(res)
}

fn convert_node(scope: &mut Scope, node: Node) -> ToppleResult<SlimNode> {
    let (left, right, node_type, line, chr) = node.destructure();
    let right = match right {
        Some(v) => Some(match v {
            Val::Node(n) => SlimVal::Node(Box::new(convert_node(scope, *n)?)),
            Val::Block(b) => SlimVal::Block({
                let mut scope = HashMap::new();
                convert_ast(&mut scope, b)?
            }),
            Val::Ident(i) => SlimVal::Ident(convert_ident(scope, i, node_type, line, chr, false)?),
            Val::Table(t) => SlimVal::Table(convert_ast(scope, t)?),
            Val::Keyword(k) => SlimVal::Keyword(k),
            Val::ByteTable(t) => SlimVal::ByteTable(t),
        }),
        None => None,
    };
    let left = match left {
        Val::Node(n) => SlimVal::Node(Box::new(convert_node(scope, *n)?)),
        Val::Block(b) => SlimVal::Block({
            let mut scope = HashMap::new();
            convert_ast(&mut scope, b)?
        }),
        Val::Ident(i) => SlimVal::Ident(convert_ident(scope, i, node_type, line, chr, true)?),
        Val::Table(t) => SlimVal::Table(convert_ast(scope, t)?),
        Val::Keyword(k) => SlimVal::Keyword(k),
        Val::ByteTable(t) => SlimVal::ByteTable(t),
    };
    Ok(SlimNode {
        left,
        right,
        node_type,
    })
}

fn convert_ident(
    scope: &mut Scope,
    ident: Ident,
    node_type: NodeType,
    line: usize,
    chr: usize,
    is_left: bool,
) -> ToppleResult<Ident> {
    let ident = match ident {
        Ident::Var(s) => s,
        Ident::Keyword(k) => return Ok(Ident::Keyword(k)),
    };
    if node_type == NodeType::Assign && is_left {
        let count = scope
            .entry(ident.clone())
            .and_modify(|e| *e += 1)
            .or_insert(0);
        let i = count.to_string() + "_" + &ident;
        Ok(Ident::Var(i))
    } else {
        match scope.get(&ident) {
            Some(c) => {
                let i = c.to_string() + "_" + &ident;
                Ok(Ident::Var(i))
            }
            None => Err(ToppleError::UseBeforeAssign(ident, line, chr)),
        }
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
        let x0_ass = SlimNode::new_binary(ident("0_x"), num(0), NodeType::Assign);
        test.push(x0_ass);
        let add = SlimNode::new_binary(ident("0_x"), num(3), NodeType::Add);
        let x1_ass = SlimNode::new_binary(ident("1_x"), node(add), NodeType::Assign);
        test.push(x1_ass);
        let add = SlimNode::new_binary(ident("1_x"), num(2), NodeType::Add);
        let y0_ass = SlimNode::new_binary(ident("0_y"), node(add), NodeType::Assign);
        test.push(y0_ass);

        assert_eq!(test, data);
    }

    #[test]
    fn inner_scope() {
        let data = test_input("x = 23;\ny = {x = args.0 / 2; y = x + 5; y;}(x);\nx + y;");

        let mut test = Vec::with_capacity(3);
        let x0_ass = SlimNode::new_binary(ident("0_x"), num(23), NodeType::Assign);
        test.push(x0_ass);
        let y0_ass = SlimNode::new_binary(ident("0_y"), anonymous_call(), NodeType::Assign);
        test.push(y0_ass);
        let add = SlimNode::new_binary(ident("0_x"), ident("0_y"), NodeType::Add);
        test.push(add);

        assert_eq!(test, data);
    }

    fn anonymous_call() -> SlimVal {
        let mut block = Vec::with_capacity(3);
        let args = SlimVal::Ident(Ident::Keyword(Keyword::Args));
        let args_idx = SlimNode::new_binary(args, num(0), NodeType::Index);
        let div = SlimNode::new_binary(node(args_idx), num(2), NodeType::Div);
        let block_x0 = SlimNode::new_binary(ident("0_x"), node(div), NodeType::Assign);
        block.push(block_x0);
        let add = SlimNode::new_binary(ident("0_x"), num(5), NodeType::Add);
        let block_y0 = SlimNode::new_binary(ident("0_y"), node(add), NodeType::Assign);
        block.push(block_y0);
        let return_y0 = SlimNode::new_unary(ident("0_y"), NodeType::Literal);
        block.push(return_y0);
        let block = SlimVal::Block(block);

        let x_pass = SlimVal::Table(vec![SlimNode::new_unary(ident("0_x"), NodeType::Literal)]);
        let node = SlimNode::new_binary(block, x_pass, NodeType::Call);
        SlimVal::Node(Box::new(node))
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
        match ssa_conversion(ast) {
            Ok(s) => s,
            Err(e) => panic!("{e}"),
        }
    }
}
