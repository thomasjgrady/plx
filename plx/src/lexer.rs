use crate::ast::{Binop, Literal, Type};
use chumsky::prelude::*;

#[derive(Clone, Debug, Hash, PartialEq)]
pub enum Keyword {
    Let,
    Match
}

#[derive(Clone, Debug, Hash, PartialEq)]
pub enum Side {
    Left,
    Right
}

#[derive(Clone, Debug, Hash, PartialEq)]
pub enum Syntax {
    Equals,
    Colon,
    Semicolon,
    Arrow,
    Comma,
    Paren(Side),
    Brace(Side)
}

#[derive(Clone, Debug, Hash, PartialEq)]
pub enum Token {
    Literal(Literal),
    Type(Type),
    Ident(String),
    Binop(Binop),
    Keyword(Keyword),
    Syntax(Syntax)
}

pub fn lexer<'src>() -> impl Parser<
    'src,
    &'src str,
    Vec<Token>,
    extra::Err<Rich<'src, char>>
> {

    choice((

        just("()").to(Token::Literal(Literal::Unit)),
        just("true").to(Token::Literal(Literal::Bool(true))),
        just("false").to(Token::Literal(Literal::Bool(false))),
        text::int(10).from_str::<i32>().unwrapped().map(|x| Token::Literal(Literal::Int(x))),

        just("unit").to(Token::Type(Type::Unit)),
        just("bool").to(Token::Type(Type::Bool)),
        just("int").to(Token::Type(Type::Int)),
        just("type").to(Token::Type(Type::Type)),

        just("+").to(Token::Binop(Binop::Add)),
        just("*").to(Token::Binop(Binop::Mul)),
        just("==").to(Token::Binop(Binop::Eq)),

        just("let").to(Token::Keyword(Keyword::Let)),
        just("match").to(Token::Keyword(Keyword::Match)),

        just("=").to(Token::Syntax(Syntax::Equals)),
        just(":").to(Token::Syntax(Syntax::Colon)),
        just(";").to(Token::Syntax(Syntax::Semicolon)),
        just("->").to(Token::Syntax(Syntax::Arrow)),
        just(",").to(Token::Syntax(Syntax::Comma)),

        just("(").to(Token::Syntax(Syntax::Paren(Side::Left))),
        just(")").to(Token::Syntax(Syntax::Paren(Side::Right))),
        just("{").to(Token::Syntax(Syntax::Brace(Side::Left))),
        just("}").to(Token::Syntax(Syntax::Brace(Side::Right))),

        text::ident().map(|x: &str| Token::Ident(x.to_string()))
    ))
        .padded()
        .repeated()
        .collect()
}