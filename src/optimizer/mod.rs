use crate::parser::{Node, Val, AST};

pub fn optimize_ast(ast: AST) -> AST {
    todo!()
}

fn ssa_conversion(ast: AST) -> AST {
    let mut res = Vec::with_capacity(ast.len());
    for mut node in ast {
        todo!()
    }
    res
}

fn trim_variables(ast: AST) -> AST {
    let mut res = Vec::new();
    for mut node in ast {
        todo!()
    }
    res
}

fn simplify_consts(ast: AST) -> AST {
    let mut res = Vec::new();
    for mut node in ast {
        todo!()
    }
    res
}

#[cfg(test)]
mod optimizer_tests {
    use super::*;
    use crate::lexer::lex;
    use crate::parser::{parse_tokens, Ident, NodeType};

    #[test]
    fn ssa() {
        let buf = "let x = 0;\nlet y;\nx = x + 3;\ny = x + 1;";
        let ast = parse_test_input(buf);
        let out = ssa_conversion(ast);
        assert_eq!(ssa_output(), out);
    }

    fn ssa_output() -> AST {
        // x0 = 0; let y0; x1 = x0 + 3; y0 = x1 + 1;
        let mut ast = Vec::new();
        let x0_ass = Node::new_binary(ident("x0"), num(0), NodeType::Assign, 0, 4);
        ast.push(x0_ass);
        let y0_def = Node::new_unary(ident("y0"), NodeType::Def, 1, 0);
        ast.push(y0_def);
        let add = Node::new_binary(ident("x0"), num(3), NodeType::Add, 2, 4);
        let x1_ass = Node::new_binary(ident("x1"), node(add), NodeType::Assign, 2, 0);
        ast.push(x1_ass);
        let add = Node::new_binary(ident("x1"), num(1), NodeType::Add, 3, 4);
        let y0_ass = Node::new_binary(ident("y0"), node(add), NodeType::Assign, 3, 0);
        ast.push(y0_ass);
        ast
    }

    #[test]
    fn var_trim() {
        let buf = "let x = 0;\nlet y;\nx = x + 3;\ny = x + 1;";
        let ast = parse_test_input(buf);
        let ssa = ssa_conversion(ast);
        let out = trim_variables(ssa);
        assert_eq!(trim_output(), out);
    }

    fn trim_output() -> AST {
        // y0 = (0 + 3) + 1;
        let paren = Node::new_binary(num(0), num(3), NodeType::Add, 2, 4);
        let add = Node::new_binary(node(paren), num(1), NodeType::Add, 3, 4);
        vec![add]
    }

    #[test]
    fn consts_simplifying() {
        let buf = "let x = 0;\nlet y;\nx = x + 3;\ny = x + 1;";
        let ast = parse_test_input(buf);
        let ssa = ssa_conversion(ast);
        let trim = trim_variables(ssa);
        let out = simplify_consts(trim);
        assert_eq!(consts_output(), out);
    }

    fn consts_output() -> AST {
        // y0 = 4;
        let assign = Node::new_binary(ident("y0"), num(4), NodeType::Assign, 3, 0);
        vec![assign]
    }

    #[test]
    fn full_reduction() {
        let buf = "let x = 0;\nlet y;\nx = x + 3;\ny = x + 1;";
        let ast = parse_test_input(buf);
        let out = optimize_ast(ast);
        assert_eq!(full_reduce_out(), out);
    }

    fn full_reduce_out() -> AST {
        // 4
        let num = Node::new_unary(num(4), NodeType::Literal, 3, 4);
        vec![num]
    }

    fn num(n: u64) -> Val {
        Val::ByteTable(n.into())
    }

    fn ident(s: &str) -> Val {
        Val::Ident(Ident::Var(s.into()))
    }

    fn node(n: Node) -> Val {
        Val::Node(Box::new(n))
    }

    fn parse_test_input(input: &str) -> AST {
        let stream = match lex(input.as_bytes()) {
            Ok(s) => s,
            Err(e) => panic!("{e}"),
        };
        match parse_tokens(stream) {
            Ok(a) => a,
            Err(e) => panic!("{e}"),
        }
    }
}
