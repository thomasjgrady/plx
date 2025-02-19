use chumsky::prelude::*;
use chumsky::pratt::*;

use crate::ast::Binop;
use crate::ast::Statement;
use crate::lexer::Keyword;
use crate::lexer::Side;
use crate::lexer::Syntax;
use crate::{ast::Expression, lexer::Token};

pub fn parser<'tokens>() -> impl Parser<
    'tokens,
    &'tokens [Token],
    Vec<Statement>,
    extra::Err<Rich<'tokens, Token>>
> {

    let literal = select! { Token::Literal(x) => x };
    let r#type = select! { Token::Type(x) => x };
    let ident = select! { Token::Ident(x) => x };

    let keyword = |x| just(Token::Keyword(x));
    let syntax = |x| just(Token::Syntax(x));

    let mut expr = Recursive::declare();
    let mut stmt = Recursive::declare();

    let paren = expr.clone().delimited_by(
        syntax(Syntax::Paren(Side::Left)),
        syntax(Syntax::Paren(Side::Right))
    );

    let atom = choice((
        literal.map(Expression::Literal),
        r#type.map(Expression::Type),
        ident.map(Expression::Ident),
        paren
    ));

    expr.define(atom.pratt(vec![
        infix(
            left(7),
            just(Token::Binop(Binop::Eq)),
            |x, _, y, _e| Expression::Binop { op: Binop::Eq, lhs: Box::new(x), rhs: Box::new(y) }
        ).boxed(),
        infix(
            left(4),
            just(Token::Binop(Binop::Add)),
            |x, _, y, _e| Expression::Binop { op: Binop::Add, lhs: Box::new(x), rhs: Box::new(y) }
        ).boxed(),
        infix(
            left(1),
            empty(),
            |x, _, y, _e| Expression::App { func: Box::new(x), arg: Box::new(y) }
        ).boxed(),
        infix(
            right(0),
            just(Token::Syntax(Syntax::Arrow)),
            |x, _, y, _e| Expression::Abs { ident: Box::new(x), body: Box::new(y) }
        ).boxed()
    ]));

    let assign = keyword(Keyword::Let)
        .ignore_then(ident.clone())
        .then((syntax(Syntax::Colon).ignore_then(expr.clone())).or_not())
        .then_ignore(syntax(Syntax::Equals))
        .then(expr.clone())
        .map(|((ident, annotation), expr)| Statement::Assign {
            ident,
            annotation,
            expr
        });

    stmt.define(
choice((
            assign,
            expr.clone().map(|e| Statement::Expression(e))
        ))
        .then_ignore(syntax(Syntax::Semicolon))
    );

    stmt.repeated().collect()

}