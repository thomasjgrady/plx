use std::fmt::Display;

#[derive(Clone, Debug, Hash, PartialEq)]
pub enum Literal {
    Unit,
    Bool(bool),
    Int(i32)
}

#[derive(Clone, Debug, Hash, PartialEq)]
pub enum Type {
    Unit,
    Bool,
    Int,
    Type
}

#[derive(Clone, Debug, Hash, PartialEq)]
pub enum Binop {
    Add,
    Mul,
    Eq
}

#[derive(Clone, Debug, Hash, PartialEq)]
pub struct Lvalue {
    pub ident: String,
    pub annotation: Option<Box<Expression>>
}

impl Lvalue {
    pub fn new<S: Into<String>>(ident: S, annotation: Option<Expression>) -> Self {
        Self { ident: ident.into(), annotation: annotation.map(|x| Box::new(x)) }
    }
}

#[derive(Clone, Debug, Hash, PartialEq)]
pub enum Pattern {
    Literal(Literal),
    Any
}

impl Pattern {
    pub fn matches(&self, expr: &Expression) -> bool {
        match (self, expr) {
            (Self::Any, _) => true,
            (Self::Literal(x), Expression::Literal(y)) => x == y,
            (Self::Literal(_), _) => false
        }
    }
}

impl Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(x) => write!(f, "{}", x),
            Self::Any => write!(f, "_")
        }
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
    Abs {
        arg: Lvalue,
        body: Box<Self>
    },
    App {
        func: Box<Self>,
        arg: Box<Self>
    },
    Match {
        expr: Box<Self>,
        cases: Vec<(Pattern, Self)>
    }
}

#[derive(Clone, Debug, Hash, PartialEq)]
pub enum Statement {
    Assign {
        lv: Lvalue,
        expr: Box<Expression>
    },
    Expr(Expression)
}

impl Expression {
    pub fn subs(&self, replace: &Self, with: &Self) -> Self {
        if self == replace {
            return with.clone();
        }
        match self {
            Self::Literal(_) | Self::Ident(_) | Self::Type(_) => self.clone(),
            Self::Binop { op, lhs, rhs } => Self::Binop {
                op: op.clone(),
                lhs: Box::new(lhs.subs(replace, with)),
                rhs: Box::new(rhs.subs(replace, with))
            },
            Self::Abs { arg, body } => {
                // here we need to avoid clobbering the substituted value by
                // not performing the replacement in the body if the argument has the
                // same identifier as the replace value
                //
                // however, because the language is dependently typed, we must
                // still apply the replacement to the annotation of the lvalue if it exists
                //
                // depending on the semantics of how we will allow recursion or self reference
                // in dependent arguments in the future, this may have to change
                let arg_out = Lvalue::new(
                    arg.ident.clone(), 
                    arg.annotation.clone().map(|x| x.subs(replace, with))
                );
                match (replace, &arg.ident) {
                    (Self::Ident(x), y) if x == y => Self::Abs {
                        arg: arg_out,
                        body: body.clone()
                    },
                    _ => Self::Abs {
                        arg: arg_out,
                        body: Box::new(body.subs(replace, with))
                    }
                }
            },
            Self::App { func, arg } => Self::App {
                func: Box::new(func.subs(replace, with)),
                arg: Box::new(arg.subs(replace, with))
            },
            Self::Match { expr, cases } => Self::Match {
                expr: Box::new(expr.subs(replace, with)),
                cases: cases.iter()
                    .map(|(p, x)| (p.clone(), x.subs(replace, with)))
                    .collect::<Vec<_>>()
            }
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unit => write!(f, "()"),
            Self::Bool(x) => x.fmt(f),
            Self::Int(x) => x.fmt(f)
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unit => write!(f, "unit"),
            Self::Bool => write!(f, "bool"),
            Self::Int => write!(f, "int"),
            Self::Type => write!(f, "type"),
        }
    }
}

impl Display for Binop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Mul => write!(f, "*"),
            Self::Eq => write!(f, "=="),
        }
    }
}

impl Display for Lvalue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.annotation {
            Some(x) => write!(f, "{}: {}", self.ident, x),
            None => write!(f, "{}", self.ident)
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(x) => write!(f, "{}", x),
            Self::Type(x) => write!(f, "{}", x),
            Self::Ident(x) => write!(f, "{}", x),
            Self::Binop { op, lhs, rhs } => write!(
                f,
                "({} {} {})",
                lhs,
                op,
                rhs
            ),
            Self::Abs { arg, body } => {
                match &arg.annotation {
                    Some(_) => write!(f, "({}) -> {}", arg, body),
                    None => write!(f, "{} -> {}", arg, body),
                }
            },
            Self::App { func, arg } => write!(f, "{} ({})", func, arg),
            Self::Match { expr, cases } => {
                write!(f, "match {} {{\n", expr)?;
                for i in 0..cases.len() {
                    let (p, x) = cases.get(i).unwrap();
                    write!(f, "  {} -> {}", p, x)?;
                    if i < cases.len()-1 {
                        write!(f, ",")?;
                    }
                    write!(f, "\n")?;
                }
                write!(f, "}}")
            }
        }
    }
}