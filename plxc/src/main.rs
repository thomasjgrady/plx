use std::fs;
use std::process::exit;

use ariadne::{Color, Label, Report, ReportKind, Source};
use clap::Parser as _;
use plx::context::Context;
use plx::eval::Eval;
use plx::lexer::lexer;
use plx::parser::parser;
use chumsky::prelude::*;


#[derive(clap::Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg()]
    filename: String
}

fn main() {

    let args = Args::parse();
    let src= fs::read_to_string(&args.filename)
        .expect("Failed to read source file");

    let lex = lexer();
    let lex_res = lex.parse(&src).into_result();

    if let Err(errors) = &lex_res {
        for e in errors {
            Report::build(ReportKind::Error, (&args.filename, e.span().into_range()))
                .with_message("Lexer Error")
                .with_label(
                    Label::new((&args.filename, e.span().into_range()))
                        .with_message(e.reason())
                        .with_color(Color::Red)
                )
                .finish()
                .eprint((&args.filename, Source::from(&src)))
                .expect("Failed to display error report");
        }
        exit(1);
    }

    let tokens = lex_res.unwrap();
    let parse = parser();
    let parse_res = parse.parse(&tokens).into_result();

    if let Err(errors) = &parse_res {
        for e in errors {
            Report::build(ReportKind::Error, (&args.filename, e.span().into_range()))
                .with_message("Parser Error")
                .with_label(
                    Label::new((&args.filename, e.span().into_range()))
                        .with_message(format!("{:?}", e.reason()))
                        .with_color(Color::Red)
                )
                .finish()
                .eprint((&args.filename, Source::from(&src)))
                .expect("Failed to display error report");
        }
        exit(1);
    }

    let program = parse_res.unwrap();
    let mut ctx = Context::new();
    
    for s in &program {
        // TODO: source maps for expressions
        let eval_res = s.eval(&mut ctx);
        if let Err(e) = eval_res {
            eprintln!("{:?}", e);
            exit(1);
        }
    }
}