use chumsky::Parser;
use plx::{ast::{Expression, Literal}, context::Context, eval::Eval, lexer::lexer, parser::parser};

#[test]
fn test_currying_1() {
    let src = r#"
        let add = x -> y -> x + y;
        let add_five = add 5;
        let eq_six = x -> add_five 1 == x;
        let six_is_six = eq_six 6;
    "#;
    let lex = lexer();
    let tokens = lex.parse(&src).unwrap();
    let parse = parser();
    let ast = parse.parse(&tokens).unwrap();
    let mut ctx = Context::new();
    for s in &ast {
        s.eval(&mut ctx).unwrap();
    }
    assert_eq!(ctx.lookup(&"six_is_six".to_string()).unwrap(), &Expression::Literal(Literal::Bool(true)));
}

#[test]
fn test_currying_2() {
    let src = r#"
        let x = 5;
        let y = 6;
        let add = x -> y -> x + y;
        let res = add 1 2;
    "#;
    let lex = lexer();
    let tokens = lex.parse(&src).unwrap();
    let parse = parser();
    let ast = parse.parse(&tokens).unwrap();
    let mut ctx = Context::new();
    for s in &ast {
        s.eval(&mut ctx).unwrap();
    }
    assert_eq!(ctx.lookup(&"res".to_string()).unwrap(), &Expression::Literal(Literal::Int(3)));
}

#[test]
fn test_precedence_1() {
    let src = r#"
        let f = x -> x * 2;
        let res = f 1 * 3 + 1;
    "#;
    let lex = lexer();
    let tokens = lex.parse(&src).unwrap();
    let parse = parser();
    let ast = parse.parse(&tokens).unwrap();
    let mut ctx = Context::new();
    for s in &ast {
        s.eval(&mut ctx).unwrap();
    }
    assert_eq!(
        ctx.lookup(&"res".to_string()).unwrap(),
        &Expression::Literal(Literal::Int(7))
    );
}

#[test]
fn test_precedence_2() {
    let src = r#"
        let f = x -> x * 2;
        let res = f 1 * 3 + 1 == 7;
    "#;
    let lex = lexer();
    let tokens = lex.parse(&src).unwrap();
    let parse = parser();
    let ast = parse.parse(&tokens).unwrap();
    let mut ctx = Context::new();
    for s in &ast {
        s.eval(&mut ctx).unwrap();
    }
    assert_eq!(
        ctx.lookup(&"res".to_string()).unwrap(),
        &Expression::Literal(Literal::Bool(true))
    );
}

#[test]
fn test_multi_argument_1() {
    let src = r#"
        let f = (x y z : int) -> x + y * z;
        let res = f 1 2 3;
    "#;
    let lex = lexer();
    let tokens = lex.parse(&src).unwrap();
    let parse = parser();
    let ast = parse.parse(&tokens).unwrap();
    let mut ctx = Context::new();
    for s in &ast {
        s.eval(&mut ctx).unwrap();
    }
    assert_eq!(
        ctx.lookup(&"res".to_string()).unwrap(),
        &Expression::Literal(Literal::Int(7))
    );
}