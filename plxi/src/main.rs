use std::io::{self, Write};
use ariadne::{Report, ReportKind, Source};
use chumsky::Parser;
use colored::Colorize;
use plx::{ast::Statement, context::Context, eval::Eval, lexer::lexer, parser::parser, typing::TypeInfer};

fn main() {
    println!("( plx v0.1.0 )\n");
    let mut eval_ctx = Context::new();
    let mut type_ctx = Context::new();
    loop {
        print!("{} ", "λ".yellow());
        io::stdout().flush().unwrap();
        
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();
        let trimmed = input.trim();
        if trimmed == "clear" {
            clearscreen::clear().unwrap();
            continue;
        }
        let mut s = trimmed.to_string();
        if !s.ends_with(";") {
            s = s + ";";
        }
        let input = s.as_ref();
        
        let lex = lexer();
        let tokens = match lex.parse(input).into_result() {
            Ok(x) => x,
            Err(errors) => {
                for e in errors {
                    Report::build(ReportKind::Error, (input, e.span().into_range()))
                        .with_message(format!("{:?}", e.reason()))
                        .finish()
                        .eprint(("", Source::from(input)))
                        .unwrap();
                }
                continue;
            }
        };

        let parse = parser();
        let ast = match parse.parse(&tokens).into_result() {
            Ok(x) => x,
            Err(errors) => {
                for e in errors {
                    Report::build(ReportKind::Error, (input, e.span().into_range()))
                        .with_message(format!("{:?}", e.reason()))
                        .finish()
                        .eprint(("", Source::from(input)))
                        .unwrap();
                }
                continue;
            }
        };
        
        for s in &ast {
            let type_res = match s {
                Statement::Expr(e) => e.infer(&mut type_ctx),
                Statement::Assign { lv: _, expr: _ } => {
                    let s_res = s.infer(&mut type_ctx);
                    s_res.map(|s_typed| match s_typed {
                        Statement::Assign { lv: _, expr: t } => *t,
                        _ => panic!("impossible")
                    })
                }
            };
            let t= match type_res {
                Ok(t) => t,
                Err(e) => {
                    match e {
                        plx::typing::Error::Type(te) => eprintln!("{} {}\n", "Type Error: ".red(), te),
                        _ => eprintln!("{:?}\n", e)
                    }
                    break;
                }
            };
            let eval_res = match s {
                Statement::Expr(e) => e.eval(&mut eval_ctx),
                _ => s.eval(&mut eval_ctx)
            };
            match eval_res {
                Ok(x) => match s {
                    Statement::Assign { lv, expr: _ } => println!("{} : {}\n", lv.ident, t),
                    Statement::Expr(_) => println!("{} : {}\n", x, t)
                },
                Err(e) => {
                    eprintln!("Evaluation Error: {:?}", e);
                    break;
                }
            }
        }
    }
}