use crate::ast::{Binop, Expression};

#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    Mistmatched {
        expected: Expression,
        actual: Expression,
        detail: Option<String>
    },
    NotAFunction {
        expr: Expression,
        detail: Option<String>
    },
    BinaryOperator {
        op: Binop,
        lhs: Expression,
        rhs: Expression,
        detail: Option<String>
    }
}