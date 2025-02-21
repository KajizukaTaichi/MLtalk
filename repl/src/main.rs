mod util;
use clap::Parser;
use colored::*;
use mltalk_core::{
    ok, some, Block, Engine, Expr, Fault, Func, IndexMap, Mode, Node, Op, Stmt, Type, Value,
};
use reqwest::blocking;
use rustyline::{
    config::Configurer, error::ReadlineError, Cmd, DefaultEditor, EventHandler, KeyEvent, Modifiers,
};
use std::{
    fs::read_to_string,
    io::{self, Read, Write},
    net::TcpListener,
    path::PathBuf,
    process::{exit, Command},
};
use urlencoding::decode;
use util::{ABOUT, NAME, VERSION};

#[derive(Parser)]
#[command(name = NAME, version = VERSION, about = ABOUT)]
struct Cli {
    /// Source file to evaluate
    #[arg(index = 1)]
    file: Option<PathBuf>,

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
        crash!(engine.malloc(
            &"cmdLineArgs".to_string(),
            &Value::List(args.iter().map(|i| Value::Str(i.to_owned())).collect()),
        ));
    }

    if let Some(file) = cli.file {
        crash!(
            Stmt::Effect(Box::new(Stmt::Expr(Expr::Infix(Box::new(Op::Call(
                Expr::Refer("load".to_string()),
                Expr::Value(Value::Str(file.to_string_lossy().to_string())),
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
    let _ = engine.malloc(
        &"print".to_string(),
        &Value::Func(Func::BuiltIn(
            |expr, _| {
                print!("{}", expr.cast(&Type::Str)?.get_str()?);
                Ok(Value::Null)
            },
            Type::Func(Some(Box::new((Type::Str, Type::Any))), Mode::Effect),
        )),
    );

    let _ = engine.malloc(
        &"input".to_string(),
        &Value::Func(Func::BuiltIn(
            |expr, _| {
                let prompt = expr.get_str()?;
                print!("{prompt}");
                io::stdout().flush().unwrap();
                let mut buffer = String::new();
                if io::stdin().read_line(&mut buffer).is_ok() {
                    Ok(Value::Str(buffer.trim().to_string()))
                } else {
                    Err(Fault::IO)
                }
            },
            Type::Func(Some(Box::new((Type::Str, Type::Str))), Mode::Effect),
        )),
    );

    let _ = engine.malloc(
        &"stdin".to_string(),
        &Value::Func(Func::UserDefined(
            "_".to_string(),
            Box::new(Expr::Block(Block(vec![Stmt::Effect(Box::new(
                Stmt::Expr(Expr::Infix(Box::new(Op::Call(
                    Expr::Refer("input".to_string()),
                    Expr::Value(Value::Str(String::new())),
                )))),
            ))]))),
            Type::Func(None, Mode::Effect),
        )),
    );

    let _ = engine.malloc(
        &"load".to_string(),
        &Value::Func(Func::BuiltIn(
            |expr, engine| {
                let name = expr.get_str()?;
                if let Ok(module) = read_to_string(&name) {
                    let ast = Block::parse(&module)?;
                    engine.mode = Mode::Pure;
                    ast.eval(engine)
                } else if let Ok(module) = blocking::get(name) {
                    if let Ok(code) = module.text() {
                        let ast = Block::parse(&code)?;
                        engine.mode = Mode::Pure;
                        ast.eval(engine)
                    } else {
                        Err(Fault::IO)
                    }
                } else {
                    Err(Fault::IO)
                }
            },
            Type::Func(Some(Box::new((Type::Str, Type::Any))), Mode::Effect),
        )),
    );

    let _ = engine.malloc(
        &"exit".to_string(),
        &Value::Func(Func::BuiltIn(
            |arg, _| exit(arg.get_number()? as i32),
            Type::Func(Some(Box::new((Type::Str, Type::Str))), Mode::Effect),
        )),
    );

    let _ = engine.malloc(
        &"system".to_string(),
        &Value::Dict(IndexMap::from([
            (
                "launchApp".to_string(),
                Value::Func(Func::BuiltIn(
                    |arg, _| {
                        let binding = arg.get_str()?;
                        let cmd: Vec<&str> = binding.split_whitespace().collect();
                        let name = ok!(cmd.first(), Fault::Index(Value::Num(0.0), arg))?;
                        ok!(some!(ok!(some!(Command::new(name)
                            .args(cmd.get(1..).unwrap_or(&[]))
                            .spawn()))?
                        .wait()))?;
                        Ok(Value::Null)
                    },
                    Type::Func(Some(Box::new((Type::Str, Type::Any))), Mode::Effect),
                )),
            ),
            (
                "shell".to_string(),
                Value::Func(Func::BuiltIn(
                    |arg, _| {
                        let binding = arg.get_str()?;
                        let cmd: Vec<&str> = binding.split_whitespace().collect();
                        let name = ok!(cmd.first(), Fault::Index(Value::Num(0.0), arg))?;
                        Ok(Value::Str(ok!(some!(String::from_utf8(
                            ok!(
                                some!(Command::new(name)
                                    .args(cmd.get(1..).unwrap_or(&[]))
                                    .output()),
                                Fault::IO
                            )?
                            .stdout
                        )))?))
                    },
                    Type::Func(Some(Box::new((Type::Str, Type::Str))), Mode::Effect),
                )),
            ),
            (
                "httpServer".to_string(),
                Value::Func(Func::BuiltIn(
                    |arg, engine| {
                        let listener = TcpListener::bind("127.0.0.1:8080").expect("Failed to bind");
                        for stream in listener.incoming() {
                            match stream {
                                Ok(mut stream) => {
                                    let mut buffer = [0; 1024];
                                    match stream.read(&mut buffer) {
                                        Ok(_) => {
                                            let request =
                                                String::from_utf8_lossy(&buffer).to_string();
                                            let response = Op::Call(
                                                Expr::Value(arg.clone()),
                                                Expr::Value(Value::Str(
                                                    ok!(some!(decode(&request)))?.to_string(),
                                                )),
                                            )
                                            .eval(engine)?
                                            .get_str()?;
                                            stream
                                                .write_all(response.as_bytes())
                                                .expect("Failed to write");
                                            stream.flush().expect("Failed to flush");
                                        }
                                        Err(_) => return Err(Fault::IO),
                                    }
                                }
                                Err(_) => return Err(Fault::IO),
                            }
                        }
                        Ok(Value::Null)
                    },
                    Type::Func(
                        Some(Box::new((
                            Type::Func(Some(Box::new((Type::Str, Type::Str))), Mode::Pure),
                            Type::Any,
                        ))),
                        Mode::Effect,
                    ),
                )),
            ),
        ])),
    );
}
