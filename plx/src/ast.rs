#[derive(Clone, Debug, Hash, PartialEq)]
pub enum Literal {
    Unit,
    Bool(bool),
    Int(i32),
}

#[derive(Clone, Debug, Hash, PartialEq)]
pub enum Type {
    Unit,
    Bool,
    Int,
    Type
}

#[derive(Clone, Copy, Debug, Hash, PartialEq)]
pub enum Binop {
    Add,
    Eq
}

#[derive(Clone, Debug, Hash, PartialEq)]
pub struct Lvalue {
    pub ident: String,
    pub annotation: Option<Box<Expression>>
}

impl Lvalue {
    pub fn new(ident: String, annotation: Option<Expression>) -> Self {
        Self { ident, annotation: annotation.map(|x| Box::new(x)) }
    }
}

#[derive(Clone, Debug, Hash, PartialEq)]
pub enum Expression {
    Literal(Literal),
    Type(Type),
    Ident(String),
    Binop {
        op: Binop,
        lhs: Box<Self>,
        rhs: Box<Self>
    },
    IfThenElse {
        if_: Box<Self>,
        then: Box<Self>,
        else_: Box<Self>
    },
    Abs {
        lvalue: Lvalue,
        body: Box<Self>
    },
    App {
        func: Box<Self>,
        arg: Box<Self>
    }
}

#[derive(Clone, Debug, Hash, PartialEq)]
pub enum Statement {
    Assign {
        ident: String,
        annotation: Option<Expression>,
        expr: Expression
    },
    Expression(Expression)
}