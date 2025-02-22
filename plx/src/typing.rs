use std::{collections::HashSet, fmt};

use crate::{ast::{Binop, Expression, Literal, Lvalue, Statement, Type}, context::{Context, Error as ContextError}, eval::{Error as EvalError, Eval, RuntimeError}};

#[derive(Clone, Debug, PartialEq)]
pub enum TypeError<T> {
    Mismatched {
        expected: Option<T>,
        actual: T,
        detail: Option<String>
    },
    InvalidType {
        got: T,
        detail: Option<String>
    },
    MissingType {
        detail: Option<String>
    }
}

impl<T: fmt::Display> fmt::Display for TypeError<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeError::Mismatched { expected, actual, detail } => {
                write!(f, "Type mismatch: got {}", actual)?;
                if let Some(exp) = expected {
                    write!(f, ", expected {}", exp)?;
                }
                if let Some(det) = detail {
                    write!(f, " ({})", det)?;
                }
                Ok(())
            }
            TypeError::InvalidType { got, detail } => {
                write!(f, "Invalid type: {}", got)?;
                if let Some(det) = detail {
                    write!(f, " ({})", det)?;
                }
                Ok(())
            }
            TypeError::MissingType { detail } => {
                write!(f, "Missing type")?;
                if let Some(det) = detail {
                    write!(f, " ({})", det)?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Error<E> {
    Context(ContextError),
    Type(E),
    Runtime(RuntimeError)
}

pub trait IsType {
    fn is_type(&self) -> bool;
}

pub trait IsSubtype {
    fn is_subtype(&self, of: &Self) -> bool;
}

pub trait TypeCheck<C, T, E> {
    fn check(&self, ctx: &mut Context<C>, expected: &T) -> Result<(), Error<E>>;
}

pub trait TypeInfer<C, T, E> {
    fn infer(&self, ctx: &mut Context<C>) -> Result<T, Error<E>>;
}

impl IsType for Expression {
    fn is_type(&self) -> bool {
        match self {
            Self::Type(_) => true,
            Self::Abs { arg, body } => match &arg.annotation {
                Some(x) => x.is_type() && body.is_type(),
                None => false
            },
            _ => false
        }
    }
}

impl IsSubtype for Expression {
    fn is_subtype(&self, of: &Self) -> bool {
        match (self, of) {
            (Self::Type(x), Self::Type(y)) if x == y => true,
            (Self::Type(_), Self::Type(Type::Type)) => true,
            (
                Self::Abs { arg: lv1, body: b1 },
                Self::Abs { arg: lv2, body: b2 }
            ) => {
                let (a1, a2) = match (&lv1.annotation, &lv2.annotation) {
                    (Some(a1), Some(a2)) => (a1, a2),
                    _ => return false
                };
                a2.is_subtype(a1) && b1.is_subtype(b2)
            },
            _ => false
        }
    }
}

impl TypeCheck<Self, Self, TypeError<Self>> for Expression {
    fn check(&self, ctx: &mut Context<Self>, expected: &Self) -> Result<(), Error<TypeError<Self>>> {
        let t_self = self.infer(ctx)?;
        if !t_self.is_type() {
            return Err(Error::Type(TypeError::InvalidType { got: t_self, detail: None }));
        }
        if !expected.is_type() {
            return Err(Error::Type(TypeError::InvalidType { got: expected.clone(), detail: None }));
        }
        if !t_self.is_subtype(expected) {
            return Err(Error::Type(TypeError::Mismatched {
                expected: Some(expected.clone()),
                actual: t_self,
                detail: None
            }));
        }
        Ok(())
    }
}

impl TypeInfer<Self, Self, TypeError<Self>> for Expression {
    fn infer(&self, ctx: &mut Context<Self>) -> Result<Self, Error<TypeError<Self>>> {
        match self {
            Self::Literal(x) => match x {
                Literal::Unit => Ok(Self::Type(Type::Unit)),
                Literal::Bool(_) => Ok(Self::Type(Type::Bool)),
                Literal::Int(_) => Ok(Self::Type(Type::Int))
            },
            Self::Type(_) => Ok(self.clone()),
            Self::Ident(x) => ctx.lookup(x)
                .cloned()
                .map_err(|e| Error::Context(e)),
            Self::Binop { op, lhs, rhs } => {
                let x = lhs.infer(ctx)?;
                let y = rhs.infer(ctx)?;
                match op {
                    Binop::Add | Binop::Mul => match (&x, &y) {
                        (Self::Type(Type::Int), Self::Type(Type::Int)) => Ok(Self::Type(Type::Int)),
                        (Self::Type(Type::Int), t) | (t, Self::Type(Type::Int)) | (t, _) =>
                            Err(Error::Type(TypeError::Mismatched {
                                expected: Some(Self::Type(Type::Int)),
                                actual: t.clone(),
                                detail: Some(format!("Invalid type for operator `{}`", op))
                            }))
                    },
                    Binop::Eq => match (&x, &y) {
                        (Self::Literal(_), Self::Literal(_)) => Ok(Self::Type(Type::Bool)),
                        (Self::Type(_), Self::Type(_)) => Ok(Self::Type(Type::Bool)),
                        (Self::Literal(_), t) | (t, Self::Literal(_)) => Err(Error::Type(TypeError::Mismatched {
                            expected: None,
                            actual: t.clone(),
                            detail: Some(format!("Invalid comparison in operator `{}`: expected literal", op))
                        })),
                        (Self::Type(_), t) | (t, Self::Type(_)) => Err(Error::Type(TypeError::Mismatched {
                            expected: None,
                            actual: t.clone(),
                            detail: Some(format!("Invalid comparison in operator `{}`: expected type", op))
                        })),
                        _ => Err(Error::Type(TypeError::Mismatched {
                            expected: None,
                            actual: x.clone(),
                            detail: Some(format!("Invalid type for operator `{}`", op))
                        })),
                    }
                }
            },
            Self::Abs { arg, body } => {
                
                let an = match &arg.annotation {
                    Some(x) => x,
                    None => return Err(Error::Type(TypeError::MissingType {
                        detail: Some("Function type infernece is unimplemented".to_string())
                    }))
                };
                an.check(ctx, &Self::Type(Type::Type))?;
                let t_an = an.eval(ctx)
                    .map_err(|e| match e {
                        EvalError::Context(c) => Error::Context(c),
                        EvalError::Runtime(r) => Error::Runtime(r)
                    })?;
                    
                let body_subs = body.subs(
                    &Self::Ident(arg.ident.clone()),
                    &t_an
                );
                let existing = ctx.bindings.keys().cloned().collect::<HashSet<_>>();
                let shadowed = ctx.remove(&arg.ident);
                let t_ret = body_subs.infer(ctx)?;
                if let Some(s) = shadowed {
                    ctx.bind(arg.ident.clone(), s);
                }
                ctx.bindings.retain(|k, _| existing.contains(k));
                Ok(Self::Abs {
                    arg: Lvalue::new(arg.ident.clone(), Some(t_an)),
                    body: Box::new(t_ret)
                })
            },
            Self::App { func, arg } => {
                let f = func.infer(ctx)?;
                let x = arg.infer(ctx)?;
                let (arg, body) = match f {
                    Self::Abs { arg, body } => (arg, body),
                    _ => {
                        return Err(Error::Type(TypeError::Mismatched {
                            expected: None,
                            actual: f,
                            detail: Some("Expected function".to_string())
                        }));
                    }
                };
                let an = match arg.annotation {
                    Some(x) => x,
                    None => {
                        return Err(Error::Type(TypeError::MissingType { detail: Some("Function arguments must be annotated".to_string()) }));
                    }
                };
                x.check(ctx, &an)?;
                Ok(*body)
            },
            Self::Match { expr, cases } => {
                let _ = expr.infer(ctx)?;
                // TODO: exhaustiveness check
                // TODO: type check vs input
                // TODO: destructuring
                if cases.len() == 0 {
                    return Ok(Expression::Type(Type::Unit)); // correct?
                }
                let (_, e0) = cases.get(0).unwrap();
                let t = e0.infer(ctx)?;
                for (_, e) in &cases[1..] {
                    e.check(ctx, &t)?;
                }
                Ok(t)
            }
        }
    }
}

impl TypeInfer<Expression, Self, TypeError<Expression>> for Statement {
    fn infer(&self, ctx: &mut Context<Expression>) -> Result<Self, Error<TypeError<Expression>>> {
        match self {
            Self::Assign { lv, expr } => {

                // TODO: deal with forward declaration of functions properly
                if let Some(x) = &lv.annotation {
                    ctx.bind_unique(lv.ident.clone(), *x.clone())
                        .map_err(|c| Error::Context(c))?;
                }

                let t_expr = expr.infer(ctx)?;
                let t_an = match &lv.annotation {
                    Some(t_an) => {
                        t_expr.check(ctx, t_an)?;
                        *t_an.clone()
                    },
                    None => t_expr.clone()
                };

                ctx.bind(lv.ident.clone(), t_expr.clone());

                Ok(Self::Assign { lv: Lvalue::new(lv.ident.clone(), Some(t_an)), expr: Box::new(t_expr) })
            },
            Self::Expr(expr) => {
                expr.infer(ctx)?;
                Ok(Self::Expr(Expression::Type(Type::Unit)))
            }
        }
    }
}