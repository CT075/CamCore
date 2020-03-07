use super::ast::*;
use super::*;
use crate::types::*;

fn id(s: &str) -> Identifier {
    Identifier(s.to_string())
}

fn s(sy: &str) -> ParsedExpr {
    ParsedExpr::Symbol(id(sy))
}

#[test]
fn basic_expr_test() {
    use ExprNode::*;
    assert_eq!(
        prec0("b / c / d"),
        Ok((
            "",
            Div(
                Box::new(Div(Box::new(s("b")), Box::new(s("c")))),
                Box::new(s("d"))
            )
        ))
    )
}
