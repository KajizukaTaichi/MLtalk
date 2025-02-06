mod util;
use clap::Parser;
use colored::*;
use mltalk_core::{Block, Engine, Expr, Node, Op, Stmt, Value};
use rustyline::{
    config::Configurer, error::ReadlineError, Cmd, DefaultEditor, EventHandler, KeyEvent, Modifiers,
};
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
