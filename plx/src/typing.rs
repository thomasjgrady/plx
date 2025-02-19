use crate::{ast::{Binop, Expression, Literal, Lvalue, Statement, Type}, context::{Context, Error as ContextError}, eval::{Eval, RuntimeError}};

#[derive(Clone, Debug, PartialEq)]
pub enum TypeError {
    Mistmatched {
        expected: Expression,
        actual: Expression,
        detail: Option<String>
    },
    NotAFunction {
        expr: Expression,
        detail: Option<String>
    },
    NotAType {
        expr: Expression,
        detail: Option<String>
    },
    BinaryOperator {
        op: Binop,
        lhs: Expression,
        rhs: Expression,
        detail: Option<String>
    },
    AnnotationRequired {
        lvalue: Lvalue,
        detail: Option<String>
    }
}

pub trait IsType {
    fn is_type(&self) -> bool;
}

impl IsType for Expression {
    fn is_type(&self) -> bool {
        match self {
            Self::Type(_) => true,
            Self::Abs { lvalue, body } =>
                lvalue.annotation.as_ref().map_or(false, |x| x.is_type()) && body.is_type(),
            _ => false
        }
    }
}

pub trait IsSubtype<T> {
    fn is_subtype(&self, t: &T) -> bool;
}

impl IsSubtype<Expression> for Expression {
    fn is_subtype(&self, t: &Expression) -> bool {
        match (self, t) {
            (Self::Type(x), Self::Type(y)) if x == y => true,
            (Self::Type(_), Self::Type(Type::Type)) => true,
            (
                Self::Abs { lvalue: lv1, body: b1 },
                Self::Abs { lvalue: lv2, body: b2 }
            ) => {
                let a1 = lv1.annotation.as_ref().unwrap();
                let a2 = lv2.annotation.as_ref().unwrap();
                a2.is_subtype(a1) && b1.is_subtype(b2)
            },
            _ => false
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Error<E> {
    Context(ContextError),
    Runtime(RuntimeError),
    Type(E)
}

pub trait TypeCheck<T, E> {
    fn check(&self, ctx: &mut Context<T>, expected: &T) -> Result<(), Error<E>>;
}

impl TypeCheck<Expression, TypeError> for Expression {
    fn check(&self, ctx: &mut Context<Expression>, expected: &Expression) -> Result<(), Error<TypeError>> {
        let t_self = self.infer(ctx)?;
        if !t_self.is_type() {
            return Err(Error::Type(TypeError::NotAType { expr: t_self.clone(), detail: None }));
        }
        if !expected.is_type() {
            return Err(Error::Type(TypeError::NotAType { expr: expected.clone(), detail: None }));
        }
        if !t_self.is_subtype(expected) {
            return Err(Error::Type(TypeError::Mistmatched { expected: expected.clone(), actual: t_self, detail: None }));
        }
        Ok(())
    }
}

pub trait TypeInfer<T, E, R = T> {
    fn infer(&self, ctx: &mut Context<T>) -> Result<R, Error<E>>;
}

impl TypeInfer<Expression, TypeError> for Expression {

    fn infer(&self, ctx: &mut Context<Expression>) -> Result<Expression, Error<TypeError>> {
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
                let t_lhs = lhs.infer(ctx)?;
                match (op, &t_lhs) {
                    (Binop::Add, Self::Type(Type::Int)) => {
                        rhs.check(ctx, &t_lhs)?;
                        Ok(t_lhs)
                    },
                    (Binop::Eq, Self::Type(_)) => {
                        rhs.check(ctx, &t_lhs)?;
                        Ok(t_lhs)
                    },
                    _ => {
                        let t_rhs = rhs.infer(ctx)?;
                        Err(Error::Type(TypeError::BinaryOperator { op: *op, lhs: t_lhs, rhs: t_rhs, detail: None }))
                    }
                }
            },
            Self::Abs { lvalue, body } => {
                let an = match &lvalue.annotation {
                    Some(x) => {
                        // evaluate the given expression (things are dependently typed)
                        x.check(ctx, &Self::Type(Type::Type))?;
                        x.eval(ctx)
                            .map_err(|e| match e {
                                crate::eval::Error::Context(c) => Error::Context(c),
                                crate::eval::Error::Runtime(r) => Error::Runtime(r)
                            })?
                    },
                    None => return Err(Error::Type(TypeError::AnnotationRequired {
                        lvalue: lvalue.clone(),
                        detail: Some("Function type parameters must be annotated".to_string())
                    }))
                };
                let shadowed = ctx.bind(lvalue.ident.clone(), an.clone());
                let t_ret = body.infer(ctx)?;
                if let Some(s) = shadowed {
                    ctx.bind(lvalue.ident.clone(), s);
                }
                Ok(Self::Abs {
                    lvalue: Lvalue::new(lvalue.ident.clone(), Some(an)),
                    body: Box::new(t_ret)
                })
            },
            Self::App { func, arg } => {
                // TODO: remove this special case
                if let Expression::Ident(s) = *func.clone() {
                    if &s == "print" {
                        return Ok(Expression::Type(Type::Unit));
                    }
                }

                let (arg_type, ret_type) = match func.infer(ctx)? {
                    Self::Abs { lvalue, body } => match lvalue.annotation {
                        Some(x) => (x, body),
                        _ => return Err(Error::Type(TypeError::AnnotationRequired {
                            lvalue: lvalue.clone(),
                            detail: Some("Function type parameters must be annotated".to_string())
                        }))
                    },
                    _ => {
                        return Err(Error::Type(TypeError::NotAFunction {
                            expr: *func.clone(),
                            detail: None
                        }))
                    }
                };

                arg.check(ctx, &arg_type)?;
                Ok(*ret_type)
            }
        }
    }
}

impl TypeInfer<Expression, TypeError, Statement> for Statement {
    fn infer(&self, ctx: &mut Context<Expression>) -> Result<Statement, Error<TypeError>> {
        match self {
            Self::Assign { ident, annotation, expr } => {
                // TODO: allow self reference in evaluation of annotation
                let an = annotation.as_ref().map(|x| {
                    x.check(ctx, &Expression::Type(Type::Type))?;
                    x.eval(ctx).map_err(|e| match e {
                        crate::eval::Error::Context(c) => Error::Context(c),
                        crate::eval::Error::Runtime(r) => Error::Runtime(r)
                    })
                }).transpose()?;

                // TODO: allow left and right typing of functions here. I.e. copy
                // annotation to lvalue annotations or vice-versa to allow type
                // checking of recursive functions
                
                let t_expr = expr.infer(ctx)?;
                let t_an = match an {
                    Some(x) => x,
                    None => t_expr.clone()
                };
                t_expr.check(ctx, &t_an)?;
                ctx.bind_unique(ident.clone(), t_expr.clone())
                    .map_err(|e| Error::Context(e))?;
                Ok(Self::Assign {
                    ident: ident.clone(),
                    annotation: Some(t_an),
                    expr: t_expr
                })
            },
            Self::Expression(e) => Ok(Self::Expression(e.infer(ctx)?))
        }
    }
}