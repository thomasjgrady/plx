use chumsky::Parser;
use plx::{ast::{Expression, Type}, context::Context, lexer::lexer, parser::parser, typing::{Error, TypeError, TypeInfer}};

fn compile_and_infer<'a>(src: &'a str) -> Result<
    Context<Expression>,
    Error<TypeError<Expression>>
> {
    let lex = lexer();
    let tokens = lex.parse(src).unwrap();
    let parse = parser();
    let ast = parse.parse(&tokens).unwrap();
    let mut ctx = Context::new();
    for s in &ast {
        let _ = s.infer(&mut ctx)?;
    }
    Ok(ctx)
}

#[test]
fn test_typing_literal() {
    let ctx = compile_and_infer(r#"
        let x = 5;
    "#).unwrap();
    assert_eq!(
        ctx.lookup(&"x".to_string()).unwrap(),
        &Expression::Type(Type::Int)
    );
}

#[test]
fn test_typing_binop() {
    let ctx = compile_and_infer(r#"
        let x = 2;
        let y = 3;
        let b = x * y == y + x;
    "#).unwrap();
    assert_eq!(
        ctx.lookup(&"b".to_string()).unwrap(),
        &Expression::Type(Type::Bool)
    );
}

#[test]
fn test_typing_function() {
    let ctx = compile_and_infer(r#"
        let eq_six = (x: int) -> x == 6;
        let b = eq_six 6;
    "#).unwrap();
    assert_eq!(
        ctx.lookup(&"b".to_string()).unwrap(),
        &Expression::Type(Type::Bool)
    );
}

#[test]
fn test_typing_match_1() {
    let ctx = compile_and_infer(r#"
        let y = match 5 {
            5 -> true,
            _ -> false
        };
    "#).unwrap();
    assert_eq!(
        ctx.lookup(&"y".to_string()).unwrap(),
        &Expression::Type(Type::Bool)
    );
}

#[test]
fn test_typing_match_invalid_output_types() {
    let ctx = compile_and_infer(r#"
        let y = match 5 {
            5 -> true,
            _ -> 1
        };
    "#);
    assert!(ctx.is_err());
}

#[test]
fn test_typing_invalid_1() {
    let ctx = compile_and_infer(r#"
        let eq_six = (x: int) -> x == 6;
        let b: int = eq_six 6;
    "#);
    assert!(ctx.is_err());
}