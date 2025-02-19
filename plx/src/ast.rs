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
pub enum Expression {
    Literal(Literal),
    Type(Type),
    Ident(String),
    Binop {
        op: Binop,
        lhs: Box<Self>,
        rhs: Box<Self>
    },
    Abs {
        ident: Box<Self>,
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