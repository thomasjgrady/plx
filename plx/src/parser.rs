use crate::ast::{Binop, Expression, Lvalue};
use crate::lexer::{Keyword, Side, Syntax};
use crate::{ast::Statement, lexer::Token};
use chumsky::prelude::*;
use chumsky::pratt::*;

pub fn parser<'a>() -> impl Parser<
    'a,
    &'a [Token],
    Vec<Statement>,
    extra::Err<Rich<'a, Token>>
> {

    let literal = select! { Token::Literal(x) => x };
    let r#type = select! { Token::Type(x) => x };
    let ident = select! { Token::Ident(x) => x };

    let keyword = |x| just(Token::Keyword(x));
    let syntax = |x| just(Token::Syntax(x));

    let mut expr = Recursive::declare();
    let mut stmt = Recursive::declare();

    let atom = choice((
        literal.clone().map(|x| Expression::Literal(x)),
        r#type.clone().map(|x| Expression::Type(x)),
        ident.clone().map(|x| Expression::Ident(x)),
        expr.clone().delimited_by(
        syntax(Syntax::Paren(Side::Left)),
            syntax(Syntax::Paren(Side::Right))
        )
    ));

    let arg = choice((
        ident.clone().map(|x| Lvalue::new(x, None)),
        ident.clone()
            .then(syntax(Syntax::Colon).ignore_then(expr.clone()))
            .delimited_by(
                syntax(Syntax::Paren(Side::Left)),
                syntax(Syntax::Paren(Side::Right))
            )
            .map(|(ident, annotation): (String, Expression)| Lvalue::new(ident, Some(annotation.clone())))
    ));

    let lvalue = ident.clone()
        .then((syntax(Syntax::Colon).ignore_then(expr.clone())).or_not())
        .map(|(ident, annotation)| Lvalue::new(ident, annotation));

    let func = arg.clone()
        .then_ignore(syntax(Syntax::Arrow))
        .then(expr.clone())
        .map(|(lv, body)| Expression::Abs { arg: lv, body: Box::new(body) });

    let ops = atom.pratt(vec![
        infix(left(3), empty(), |x, _, y, _| {
            Expression::App { func: Box::new(x), arg: Box::new(y) }
        }).boxed(),
        infix(left(2), just(Token::Binop(Binop::Mul)), |x, _, y, _| {
            Expression::Binop { op: Binop::Mul, lhs: Box::new(x), rhs: Box::new(y) }
        }).boxed(),
        infix(left(1), just(Token::Binop(Binop::Add)), |x, _, y, _| {
            Expression::Binop { op: Binop::Add, lhs: Box::new(x), rhs: Box::new(y) }
        }).boxed(),
        infix(left(0), just(Token::Binop(Binop::Eq)), |x, _, y, _| {
            Expression::Binop { op: Binop::Eq, lhs: Box::new(x), rhs: Box::new(y) }
        }).boxed()
    ]);

    expr.define(choice((
        func,
        ops
    )));

    let assign = keyword(Keyword::Let)
        .ignore_then(lvalue.clone())
        .then_ignore(syntax(Syntax::Equals))
        .then(expr.clone())
        .then_ignore(syntax(Syntax::Semicolon))
        .map(|(lv, e)| Statement::Assign { lv, expr: Box::new(e) });
        

    let expr_ = expr.clone()
        .then_ignore(syntax(Syntax::Semicolon))
        .map(|e| Statement::Expr(e));

    stmt.define(choice((
        assign,
        expr_
    )));

    stmt.repeated().collect()

}