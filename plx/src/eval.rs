use crate::{ast::{Binop, Expression, Literal, Statement}, context::{Context, Error as ContextError}};
use crate::typing::TypeError as TypeError;

#[derive(Clone, Debug, PartialEq)]
pub enum Error<E> {
    Context(ContextError),
    Runtime(E)
}

pub trait Eval<T, E> {
    fn eval(&self, ctx: &mut Context<T>) -> Result<T, Error<E>>;
}

#[derive(Clone, Debug, PartialEq)]
pub enum RuntimeError {
    Type(TypeError)
}

fn into_abs(e: Expression) -> Result<(String, Box<Expression>), RuntimeError> {
    match e {
        Expression::Abs { lvalue, body } => Ok((lvalue.ident, body)),
        _ => Err(RuntimeError::Type(TypeError::NotAFunction { expr: e, detail: None }))
    }
}

impl Eval<Expression, RuntimeError> for Expression {
    fn eval(&self, ctx: &mut Context<Expression>) -> Result<Expression, Error<RuntimeError>> {
        match self {
            Self::Literal(_) => Ok(self.clone()),
            Self::Type(_) => Ok(self.clone()),
            Self::Ident(x) => ctx.lookup(x)
                .cloned()
                .map_err(|e| Error::Context(e)),
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
                        Binop::Eq,
                        Expression::Literal(_),
                        Expression::Literal(_)
                    ) => {
                        Ok(Expression::Literal(Literal::Bool(x == y)))
                    },
                    _ => Err(Error::Runtime(RuntimeError::Type(TypeError::BinaryOperator {
                        op: *op,
                        lhs: *lhs.clone(),
                        rhs: *rhs.clone(),
                        detail: None
                    })))
                }
            },
            Self::IfThenElse { if_, then, else_ } => {
                let cond_expr = if_.eval(ctx)?;
                let cond = match cond_expr {
                    Self::Literal(Literal::Bool(b)) => b,
                    _ => {
                        return Err(Error::Runtime(RuntimeError::Type(TypeError::NotABoolean {
                            expr: cond_expr,
                            detail: Some("If condition must evaluate to boolean".to_string())
                        })));
                    }
                };
                if cond {
                    then.eval(ctx)
                } else {
                    else_.eval(ctx)
                }
            }
            Self::Abs { lvalue: _, body: _ } => Ok(self.clone()),
            Self::App { func, arg } => {

                if let Expression::Ident(s) = *func.clone() {
                    if &s == "print" {
                        let output = arg.eval(ctx)?;
                        println!("{:?}", output);
                        return Ok(Expression::Literal(Literal::Unit));
                    }
                }
                
                let f = func.eval(ctx)?;
                let (ident, body) = into_abs(f).map_err(|e| Error::Runtime(e))?;
                let shadowed = ctx.bind(ident.clone(), *arg.to_owned());
                let output = body.eval(ctx)?;
                if let Some(s) = shadowed {
                    ctx.bind(ident.clone(), s);
                }
                Ok(output)
            }
        }
    }
}

impl Eval<Expression, RuntimeError> for Statement {
    fn eval(&self, ctx: &mut Context<Expression>) -> Result<Expression, Error<RuntimeError>> {
        match self {
            Self::Assign { ident, annotation: _, expr } => {
                // forward bind for recursive functions
                ctx.bind_unique(ident.clone(), expr.clone()).map_err(|e| Error::Context(e))?;
                let x = expr.eval(ctx)?;
                ctx.bind(ident.to_owned(), x.clone());
                Ok(x)
            },
            Self::Expression(e) => e.eval(ctx)
        }
    }
}