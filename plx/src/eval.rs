use std::collections::HashSet;
use crate::{ast::{Binop, Expression, Literal, Statement}, context::{Context, Error as ContextError}, typing::TypeError};


#[derive(Clone, Debug, PartialEq)]
pub enum Error<E> {
    Context(ContextError),
    Runtime(E)
}

pub trait Eval<C, T, E> {
    fn eval(&self, ctx: &mut Context<C>) -> Result<T, Error<E>>;
}

#[derive(Clone, Debug, PartialEq)]
pub enum RuntimeError {
    Type(TypeError<Expression>)
}

impl Eval<Self, Self, RuntimeError> for Expression {
    fn eval(&self, ctx: &mut Context<Expression>) -> Result<Expression, Error<RuntimeError>> {
        match self {
            Self::Literal(_) => Ok(self.clone()),
            Self::Type(_) => Ok(self.clone()),
            Self::Ident(x) => ctx.lookup(x)
                .cloned()
                .map_err(Error::Context),
            Self::Binop { op, lhs, rhs } => {
                let x = lhs.eval(ctx)?;
                let y = rhs.eval(ctx)?;
                match (op, &x, &y) {
                    (
                        Binop::Add,
                        Expression::Literal(Literal::Int(a)),
                        Expression::Literal(Literal::Int(b))
                    ) => Ok(Expression::Literal(Literal::Int(a + b))),
                    (
                        Binop::Mul,
                        Expression::Literal(Literal::Int(a)),
                        Expression::Literal(Literal::Int(b))
                    ) => Ok(Expression::Literal(Literal::Int(a * b))),
                    (Binop::Eq, _, _) => Ok(Expression::Literal(Literal::Bool(x == y))),
                    _ => {
                        // TODO: match on (op, x, y) to get this information
                        let expected = None;
                        let actual = self.clone();
                        let detail = Some(format!("Invalid inputs for operator {:?}", op));
                        Err(Error::Runtime(RuntimeError::Type(TypeError::Mismatched { expected, actual, detail })))
                    }
                }
            }
            Self::Abs { arg: _, body: _ } => Ok(self.clone()),
            Self::App { func, arg } => {
                let f = func.eval(ctx)?;
                let (lv, body) = match f {
                    Self::Abs { arg, body } => (arg, body),
                    _ => {
                        return Err(Error::Runtime(RuntimeError::Type(TypeError::Mismatched {
                            expected: None,
                            actual: f,
                            detail: Some("Left hand side of function application must evaluate to function".to_string())
                        })))
                    }
                };
                let x = arg.eval(ctx)?;
                let body_subs = body.subs(
                    &Self::Ident(lv.ident.clone()),
                    &x
                );
                let existing = ctx.bindings.keys().cloned().collect::<HashSet<_>>();
                let shadowed = ctx.remove(&lv.ident);
                let res = body_subs.eval(ctx)?;
                if let Some(s) = shadowed {
                    ctx.bind(lv.ident.clone(), s);
                }
                ctx.bindings.retain(|k, _| existing.contains(k));
                Ok(res)
            }
        }
    }
}

impl Eval<Expression, Expression, RuntimeError> for Statement {
    fn eval(&self, ctx: &mut Context<Expression>) -> Result<Expression, Error<RuntimeError>> {
        match self {
            Self::Assign { lv, expr } => {
                // forward declare to allow recursive functions to work
                ctx.bind_unique(lv.ident.clone(), *expr.clone())
                    .map_err(|e| Error::Context(e))?;
                let x = expr.eval(ctx)?;
                ctx.bind(lv.ident.clone(), x);
                Ok(Expression::Literal(Literal::Unit))
            },
            Self::Expr(expr) => {
                expr.eval(ctx)?;
                Ok(Expression::Literal(Literal::Unit))
            }
        }
    }
}