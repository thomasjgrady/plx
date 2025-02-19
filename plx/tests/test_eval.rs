use chumsky::Parser;
use plx::ast::{Expression, Literal};
use plx::context::Context;
use plx::eval::Eval;
use plx::lexer::lexer;
use plx::parser::parser;

fn untyped_compile_and_eval<'a>(src: &'a str) -> Context<Expression> {
    let tokens = lexer().parse(src).unwrap();
    let program = parser().parse(&tokens).unwrap();
    let mut ctx = Context::new();
    for s in &program {
        s.eval(&mut ctx).unwrap();
    }
    ctx
}

#[test]
fn test_eval_literal() {
    {
        let ctx = untyped_compile_and_eval("let x = ();");
        assert_eq!(ctx.lookup(&"x".to_string()).unwrap(), &Expression::Literal(Literal::Unit));
    }
    {
        let ctx = untyped_compile_and_eval("let x = 5;");
        assert_eq!(ctx.lookup(&"x".to_string()).unwrap(), &Expression::Literal(Literal::Int(5)));
    }
    {
        let ctx = untyped_compile_and_eval("let x = true;");
        assert_eq!(ctx.lookup(&"x".to_string()).unwrap(), &Expression::Literal(Literal::Bool(true)));
    }
    {
        let ctx = untyped_compile_and_eval("let x = false;");
        assert_eq!(ctx.lookup(&"x".to_string()).unwrap(), &Expression::Literal(Literal::Bool(false)));
    }
}

#[test]
fn test_eval_function() {
    let ctx = untyped_compile_and_eval("let f = x -> x + 1; let y = f 1;");
    assert_eq!(ctx.lookup(&"y".to_string()).unwrap(), &Expression::Literal(Literal::Int(2)));
}

#[test]
fn test_eval_function_currying() {
    let ctx = untyped_compile_and_eval(r#"
        let add = x -> y -> x + y;
        let add_five = add 5;
        let six = add_five 1;
    "#);
    assert_eq!(ctx.lookup(&"six".to_string()).unwrap(), &Expression::Literal(Literal::Int(6)));
}