mod util;
use clap::Parser;
use colored::*;
use mltalk_core::{ok, some, Block, Engine, Expr, Fault, Func, Mode, Node, Op, Stmt, Type, Value};
use reqwest::blocking;
use rustyline::{
    config::Configurer, error::ReadlineError, Cmd, DefaultEditor, EventHandler, KeyEvent, Modifiers,
};
use std::fs::read_to_string;
use std::io::{self, Write};
use std::{process::exit, thread::sleep, time::Duration};
use util::{ABOUT, NAME, VERSION};

#[derive(Parser)]
#[command(name = NAME, version = VERSION, about = ABOUT)]
struct Cli {
    /// Source file to evaluate
    #[arg(index = 1)]
    file: Option<String>,

    /// Command-line arguments to pass the script
    #[arg(index = 2, value_name = "ARGS", num_args = 0..)]
    args_position: Option<Vec<String>>,

    /// Optional command-line arguments
    #[arg(short = 'a', long = "args", value_name = "ARGS", num_args = 0..)]
    args_option: Option<Vec<String>>,
}

fn main() {
    let cli = Cli::parse();
    let mut engine = Engine::new();
    customize_distribution_function(&mut engine);

    if let (Some(args), _) | (_, Some(args)) = (cli.args_position, cli.args_option) {
        crash!(engine.alloc(
            &"cmdLineArgs".to_string(),
            &Value::List(args.iter().map(|i| Value::Str(i.to_owned())).collect()),
        ));
    }

    if let Some(file) = cli.file {
        crash!(
            Stmt::Effect(Box::new(Stmt::Expr(Expr::Infix(Box::new(Op::Apply(
                Expr::Refer("load".to_string()),
                false,
                Expr::Value(Value::Str(file)),
            ))))))
            .eval(&mut engine)
        );
    } else {
        println!(
            "{title} programming language",
            title = NAME.blue().bold().underline()
        );
        println!("(c) 2025 梶塚太智. All rights reserved");

        let (mut session, mut line, mut code) = (1, 0, String::new());
        let mut rl = DefaultEditor::new().unwrap();
        rl.set_auto_add_history(true);
        rl.bind_sequence(
            KeyEvent::new('\t', Modifiers::NONE),
            EventHandler::Simple(Cmd::Insert(1, " ".repeat(2))),
        );

        loop {
            let prompt = &format!("[{session:0>3}:{line}]> ");
            match rl.readline(prompt) {
                Ok(entered) => {
                    if entered.is_empty() {
                        match Block::parse(&code) {
                            Ok(ast) => match &ast.eval(&mut engine) {
                                Ok(result) => repl_print!(green, result),
                                Err(e) => fault!(e),
                            },
                            Err(e) => fault!(e),
                        }
                        code = String::new();
                        session += 1;
                        line = 0;
                    } else {
                        code.push_str(&format!("{entered}\n"));
                        line += 1
                    }
                }
                Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => return,
                _ => {}
            }
        }
    }
}

fn customize_distribution_function(engine: &mut Engine) {
    let _ = engine.alloc(
        &"print".to_string(),
        &Value::Func(Func::BuiltIn(|expr, _| {
            print!("{}", expr.cast(&Type::Str)?.get_str()?);
            Ok(Value::Null)
        })),
    );
    engine.set_effect("print");

    let _ = engine.alloc(
        &"input".to_string(),
        &Value::Func(Func::BuiltIn(|expr, _| {
            let prompt = expr.get_str()?;
            print!("{prompt}");
            io::stdout().flush().unwrap();
            let mut buffer = String::new();
            if io::stdin().read_line(&mut buffer).is_ok() {
                Ok(Value::Str(buffer.trim().to_string()))
            } else {
                Err(Fault::IO)
            }
        })),
    );
    engine.set_effect("input");

    let _ = engine.alloc(
        &"stdin".to_string(),
        &Value::Func(Func::UserDefined(
            "_".to_string(),
            Box::new(Expr::Block(Block(vec![Stmt::Effect(Box::new(
                Stmt::Expr(Expr::Infix(Box::new(Op::Apply(
                    Expr::Refer("input".to_string()),
                    false,
                    Expr::Value(Value::Str(String::new())),
                )))),
            ))]))),
            Type::Func(None, Mode::Pure),
        )),
    );
    engine.set_effect("stdin");

    let _ = engine.alloc(
        &"readFile".to_string(),
        &Value::Func(Func::BuiltIn(|i, _| {
            Ok(Value::Str(ok!(
                some!(read_to_string(i.get_str()?)),
                Fault::IO
            )?))
        })),
    );
    engine.set_effect("readFile");

    let _ = engine.alloc(
        &"load".to_string(),
        &Value::Func(Func::BuiltIn(|expr, engine| {
            let name = expr.get_str()?;
            if let Ok(module) = read_to_string(&name) {
                let ast = Block::parse(&module)?;
                ast.eval(engine)
            } else if let Ok(module) = blocking::get(name) {
                if let Ok(code) = module.text() {
                    let ast = Block::parse(&code)?;
                    ast.eval(engine)
                } else {
                    Err(Fault::IO)
                }
            } else {
                Err(Fault::IO)
            }
        })),
    );
    engine.set_effect("load");

    let _ = engine.alloc(
        &"sleep".to_string(),
        &Value::Func(Func::BuiltIn(|i, _| {
            sleep(Duration::from_secs_f64(i.get_number()?));
            Ok(Value::Null)
        })),
    );
    engine.set_effect("sleep");

    let _ = engine.alloc(
        &"exit".to_string(),
        &Value::Func(Func::BuiltIn(|arg, _| exit(arg.get_number()? as i32))),
    );
    engine.set_effect("exit");
}
