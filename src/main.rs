mod utils;
use clap::Parser;
use colored::*;
use indexmap::{IndexMap, IndexSet};
use reqwest::blocking;
use rustyline::{error::ReadlineError, DefaultEditor};
use std::{
    fmt::{self, Debug, Display, Formatter},
    fs::read_to_string,
    io::{self, Write},
    process::exit,
    thread::sleep,
    time::Duration,
};
use thiserror::Error;
use unicode_xid::UnicodeXID;
use utils::{ABOUT, BEGIN, BUILTIN, END, NAME, RESERVED, SPACE, VERSION};

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
        crash!(Operator::Apply(
            Expr::Refer("load".to_string()),
            false,
            Expr::Value(Value::Str(file)),
        )
        .eval(&mut engine));
    } else {
        println!(
            "{title} programming language",
            title = NAME.blue().bold().underline()
        );
        let mut rl = DefaultEditor::new().unwrap();
        let (mut session, mut line, mut code) = (1, 0, String::new());

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

type Scope = IndexMap<String, Value>;
#[derive(Debug, Clone)]
struct Engine {
    env: Scope,
    protect: IndexSet<String>,
}

impl Engine {
    fn new() -> Engine {
        Engine {
            protect: BUILTIN.to_vec().iter().map(|i| i.to_string()).collect(),
            env: IndexMap::from([
                (
                    "type".to_string(),
                    Value::Func(Func::BuiltIn(|expr, _| Ok(Value::Type(expr.type_of())))),
                ),
                (
                    "alphaConvert".to_string(),
                    Value::Func(Func::BuiltIn(|args, _| {
                        let args = args.get_list()?;
                        let func = ok!(args.get(0), Fault::ArgLen)?;
                        let new_name = ok!(args.get(1), Fault::ArgLen)?.get_str()?;
                        let Value::Func(Func::UserDefined(old_name, body)) = func else {
                            return Err(Fault::Value(func.to_owned(), Type::Func));
                        };
                        Ok(Value::Func(Func::UserDefined(
                            new_name.clone(),
                            Box::new(body.replace(
                                &Expr::Refer(old_name.to_owned()),
                                &Expr::Refer(new_name),
                            )),
                        )))
                    })),
                ),
                (
                    "input".to_string(),
                    Value::Func(Func::BuiltIn(|expr, _| {
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
                ),
                (
                    "readFile".to_string(),
                    Value::Func(Func::BuiltIn(|i, _| {
                        Ok(Value::Str(ok!(
                            some!(read_to_string(i.get_str()?)),
                            Fault::IO
                        )?))
                    })),
                ),
                (
                    "load".to_string(),
                    Value::Func(Func::BuiltIn(|expr, engine| {
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
                ),
                (
                    "sleep".to_string(),
                    Value::Func(Func::BuiltIn(|i, _| {
                        sleep(Duration::from_secs_f64(i.get_number()?));
                        Ok(Value::Null)
                    })),
                ),
                (
                    "exit".to_string(),
                    Value::Func(Func::BuiltIn(|arg, _| exit(arg.get_number()? as i32))),
                ),
            ]),
        }
    }

    fn alloc(&mut self, name: &String, value: &Value) -> Result<(), Fault> {
        if self.is_protect(name) {
            return Err(Fault::AccessDenied);
        }
        if is_identifier(name) {
            if name != "_" {
                self.env.insert(name.clone(), value.clone());
            }
            Ok(())
        } else {
            Err(Fault::Syntax)
        }
    }

    fn add_protect(&mut self, name: &str) {
        self.protect.insert(name.to_string());
    }

    fn is_protect(&mut self, name: &str) -> bool {
        self.protect.contains(&name.to_string())
    }
}

#[derive(Debug, Clone)]
struct Block(Vec<Statement>);

impl Block {
    fn parse(source: &str) -> Result<Block, Fault> {
        let mut program = Vec::new();
        for line in tokenize(source, &[";"])? {
            let line = line.trim();
            // Ignore empty line and comment
            if line.is_empty() || line.starts_with("//") {
                continue;
            }
            program.push(Statement::parse(line)?);
        }
        Ok(Block(program))
    }

    fn eval(&self, engine: &mut Engine) -> Result<Value, Fault> {
        let mut result = Value::Null;
        for code in &self.0 {
            result = code.eval(engine)?
        }
        Ok(result)
    }

    fn replace(&self, from: &Expr, to: &Expr) -> Self {
        Block(
            self.0
                .iter()
                .map(|i| Statement::replace(i, from, to))
                .collect(),
        )
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            format!(
                "begin {} end",
                self.0
                    .iter()
                    .map(|i| format!("{i}"))
                    .collect::<Vec<String>>()
                    .join("; ")
            ),
        )
    }
}

#[derive(Debug, Clone)]
enum Statement {
    Print(Vec<Expr>),
    Let(Expr, bool, Expr),
    If(Box<Statement>, Expr, Option<Expr>),
    For(Expr, Expr, Expr),
    While(Box<Statement>, Expr),
    Fault(Option<Expr>),
    Expr(Expr),
}

impl Statement {
    fn eval(&self, engine: &mut Engine) -> Result<Value, Fault> {
        Ok(match self {
            Statement::Print(expr) => {
                for i in expr {
                    print!("{}", i.eval(engine)?.cast(&Type::Str)?.get_str()?);
                }
                io::stdout().flush().unwrap();
                Value::Null
            }
            Statement::Let(name, is_protect, expr) => {
                if let Expr::Refer(name) = name {
                    let val = expr.eval(engine)?;
                    engine.alloc(name, &val)?;
                    if *is_protect {
                        engine.add_protect(name);
                    }
                    val
                } else if let Expr::List(list) = name {
                    let val = expr.eval(engine)?;
                    let val = val.get_list()?;
                    if list.len() == list.len() {
                        for (name, val) in list.iter().zip(&val) {
                            Statement::Let(name.to_owned(), *is_protect, Expr::Value(val.clone()))
                                .eval(engine)?;
                        }
                        Value::List(val)
                    } else {
                        return Err(Fault::Syntax);
                    }
                } else if let Expr::Dict(dict) = name {
                    let val = expr.eval(engine)?;
                    let val = val.get_dict()?;
                    for (key, name) in dict {
                        let val = Expr::Value(
                            ok!(
                                &val.get(key).clone(),
                                Fault::Key(Value::Str(key.to_owned()), Value::Dict(val.clone()))
                            )?
                            .to_owned()
                            .clone(),
                        )
                        .clone();
                        Statement::Let(name.to_owned(), *is_protect, val).eval(engine)?;
                    }
                    Value::Dict(val)
                } else if let Expr::Infix(infix) = name {
                    let infix = *infix.clone();
                    if let Operator::Access(accessor, key) = infix {
                        let val = expr.eval(engine)?;
                        let obj = accessor.eval(engine)?;
                        let key = key.eval(engine)?;
                        let updated_obj = obj.modify_inside(&key, &Some(val.clone()), engine)?;
                        Statement::Let(accessor, *is_protect, Expr::Value(updated_obj.clone()))
                            .eval(engine)?
                    } else if let Operator::As(name, sig) = infix {
                        let val = expr.eval(engine)?;
                        let sig = sig.eval(engine)?.get_type()?;
                        if val.type_of() != sig {
                            return Err(Fault::Value(val, sig));
                        }
                        Statement::Let(name, *is_protect, Expr::Value(val.clone())).eval(engine)?
                    } else if let Operator::Apply(name, false, arg) = infix {
                        return Statement::Let(
                            name,
                            *is_protect,
                            Expr::Value(Value::Func(Func::UserDefined(
                                arg.to_string(),
                                Box::new(expr.to_owned()),
                            ))),
                        )
                        .eval(engine);
                    } else {
                        let name = name.eval(engine)?;
                        let val = expr.eval(engine)?;
                        if name != val {
                            return Err(Fault::Syntax);
                        }
                        val
                    }
                } else {
                    let name = name.eval(engine)?;
                    let val = expr.eval(engine)?;
                    if name != val {
                        return Err(Fault::Syntax);
                    }
                    val
                }
            }
            Statement::If(expr, then, r#else) => {
                if expr.eval(engine).is_ok() {
                    then.eval(engine)?
                } else {
                    if let Some(r#else) = r#else {
                        r#else.clone().eval(engine)?
                    } else {
                        Value::Null
                    }
                }
            }
            Statement::For(counter, expr, code) => {
                let mut result = Value::Null;
                for i in expr.eval(engine)?.cast(&Type::List)?.get_list()? {
                    Statement::Let(counter.clone(), false, Expr::Value(i)).eval(engine)?;
                    result = code.eval(engine)?;
                }
                result
            }
            Statement::While(expr, code) => {
                let mut result = Value::Null;
                while expr.eval(engine).is_ok() {
                    result = code.eval(engine)?;
                }
                result
            }
            Statement::Fault(Some(msg)) => {
                return Err(Fault::General(Some(msg.eval(engine)?.get_str()?)))
            }
            Statement::Fault(None) => return Err(Fault::General(None)),
            Statement::Expr(expr) => expr.eval(engine)?,
        })
    }

    fn parse(code: &str) -> Result<Statement, Fault> {
        let code = code.trim();
        if let Some(code) = code.strip_prefix("print") {
            let mut exprs = vec![];
            for i in tokenize(code, &[","])? {
                exprs.push(Expr::Block(Block::parse(&i)?).optimize())
            }
            Ok(Statement::Print(exprs))
        } else if let (_, Some(codes)) | (Some(codes), _) =
            (code.strip_prefix("let"), code.strip_prefix("const"))
        {
            let splited = tokenize(codes, &["="])?;
            let (name, codes) = (ok!(splited.get(0))?, join!(ok!(splited.get(1..))?, "="));
            Ok(Statement::Let(
                Expr::parse(name)?,
                code.starts_with("const"),
                Expr::Block(Block::parse(&codes)?).optimize(),
            ))
        } else if let Some(code) = code.strip_prefix("if") {
            let code = tokenize(code, SPACE.as_ref())?;
            let pos_then = ok!(code.iter().position(|i| i == "then"))?;
            if let Some(pos_else) = code.iter().position(|i| i == "else") {
                Ok(Statement::If(
                    Box::new(Statement::parse(&join!(ok!(code.get(0..pos_then))?))?),
                    Expr::Block(Block::parse(&join!(
                        ok!(code.get(pos_then + 1..pos_else))?
                    ))?),
                    Some(
                        Expr::Block(Block::parse(&join!(ok!(code.get(pos_else + 1..))?))?)
                            .optimize(),
                    ),
                ))
            } else {
                Ok(Statement::If(
                    Box::new(Statement::parse(&join!(ok!(code.get(0..pos_then))?))?),
                    Expr::Block(Block::parse(&join!(ok!(code.get(pos_then + 1..))?))?).optimize(),
                    None,
                ))
            }
        } else if let Some(code) = code.strip_prefix("for") {
            let code = tokenize(code, SPACE.as_ref())?;
            let pos_in = ok!(code.iter().position(|i| i == "in"))?;
            let pos_do = ok!(code.iter().position(|i| i == "do"))?;
            Ok(Statement::For(
                Expr::parse(&join!(ok!(code.get(0..pos_in))?))?,
                Expr::Block(Block::parse(&join!(ok!(code.get(pos_in + 1..pos_do))?))?).optimize(),
                Expr::Block(Block::parse(&join!(ok!(code.get(pos_do + 1..))?))?).optimize(),
            ))
        } else if let Some(code) = code.strip_prefix("while") {
            let code = tokenize(code, SPACE.as_ref())?;
            let pos_loop = ok!(code.iter().position(|i| i == "loop"))?;
            Ok(Statement::While(
                Box::new(Statement::parse(&join!(ok!(code.get(0..pos_loop))?))?),
                Expr::Block(Block::parse(&join!(ok!(code.get(pos_loop + 1..))?))?).optimize(),
            ))
        } else if let Some(code) = code.strip_prefix("fault") {
            Ok(Statement::Fault(some!(Expr::parse(code))))
        } else {
            Ok(Statement::Expr(Expr::parse(code)?))
        }
    }

    fn replace(&self, from: &Expr, to: &Expr) -> Self {
        match self {
            Statement::Print(vals) => {
                Statement::Print(vals.iter().map(|j| j.replace(from, to)).collect())
            }
            Statement::Let(name, is_protect, val) => {
                Statement::Let(name.clone(), *is_protect, val.replace(from, to))
            }
            Statement::If(cond, then, r#else) => Statement::If(
                Box::new(cond.replace(from, to)),
                then.replace(from, to),
                r#else.clone().map(|j| j.replace(from, to)),
            ),
            Statement::For(counter, iter, code) => Statement::For(
                counter.clone(),
                iter.replace(from, to),
                code.replace(from, to),
            ),
            Statement::While(cond, code) => {
                Statement::While(Box::new(cond.replace(from, to)), code.replace(from, to))
            }
            Statement::Fault(Some(msg)) => Statement::Fault(Some(msg.replace(from, to))),
            Statement::Fault(None) => Statement::Fault(None),
            Statement::Expr(val) => Statement::Expr(val.replace(from, to)),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Statement::Print(exprs) => format!(
                    "print {}",
                    exprs
                        .iter()
                        .map(|i| format!("{i}"))
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
                Statement::Let(name, false, val) => format!("let {name} = {val}"),
                Statement::Let(name, true, val) => format!("const {name} = {val}"),
                Statement::If(cond, then, r#else) =>
                    if let Some(r#else) = r#else {
                        format!("if {cond} then {then} else {}", r#else)
                    } else {
                        format!("if {cond} then {then}")
                    },
                Statement::For(counter, iterator, code) => {
                    format!("for {counter} in {iterator} do {code}")
                }
                Statement::While(cond, code) => {
                    format!("while {cond} loop {code}")
                }
                Statement::Fault(Some(msg)) => format!("fault {msg}"),
                Statement::Fault(None) => "fault".to_string(),
                Statement::Expr(expr) => format!("{expr}"),
            }
        )
    }
}

#[derive(Debug, Clone)]
enum Expr {
    Refer(String),
    Infix(Box<Operator>),
    List(Vec<Expr>),
    Dict(Vec<(String, Expr)>),
    Block(Block),
    Value(Value),
}

impl Expr {
    fn eval(&self, engine: &mut Engine) -> Result<Value, Fault> {
        Ok(match self {
            Expr::Refer(name) => {
                if name == "_" {
                    Value::Null
                } else if let Some(refer) = engine.env.get(name.as_str()) {
                    refer.clone()
                } else {
                    return Err(Fault::Refer(name.to_string()));
                }
            }
            Expr::Infix(infix) => (*infix).eval(engine)?,
            Expr::Block(block) => block.clone().eval(engine)?,
            Expr::List(list) => {
                let mut result = vec![];
                for i in list {
                    result.push(i.eval(engine)?)
                }
                Value::List(result)
            }
            Expr::Dict(st) => {
                let mut result = IndexMap::new();
                for (k, x) in st {
                    result.insert(k.to_string(), x.eval(engine)?);
                }
                Value::Dict(result)
            }
            Expr::Value(value) => value.clone(),
        })
    }

    fn parse(source: &str) -> Result<Expr, Fault> {
        let token_list: Vec<String> = tokenize(source, SPACE.as_ref())?;
        if token_list.len() >= 2 {
            Ok(Expr::Infix(Box::new(Operator::parse(source)?)))
        } else {
            let token = ok!(token_list.last())?.trim().to_string();
            Ok(if let Ok(n) = token.parse::<f64>() {
                Expr::Value(Value::Num(n))
            } else if let Ok(sig) = Type::parse(&token) {
                Expr::Value(Value::Type(sig))
            // Prefix operators
            } else if token.starts_with("!") {
                let token = remove!(token, "!");
                Expr::Infix(Box::new(Operator::Not(Expr::parse(&token)?)))
            } else if token.starts_with("-") {
                let token = remove!(token, "-");
                Expr::Infix(Box::new(Operator::Sub(
                    Expr::Value(Value::Num(0.0)),
                    Expr::parse(&token)?,
                )))
            } else if token.starts_with("(") && token.ends_with(")") {
                let token = trim!(token, "(", ")");
                Expr::parse(token)?
            } else if token.starts_with(BEGIN) && token.ends_with(END) {
                let token = trim!(token, BEGIN, END);
                Expr::Block(Block::parse(token)?).optimize()
            } else if token.starts_with("{") && token.ends_with("}") {
                let token = trim!(token, "{", "}");
                let mut result = Vec::new();
                for i in tokenize(token, &[","])? {
                    let splited = tokenize(&i, &[":"])?;
                    let key = ok!(splited.get(0))?.trim().to_string();
                    if !is_identifier(&key) {
                        return Err(Fault::Syntax);
                    }
                    let value = if splited.len() >= 2 {
                        join!(ok!(splited.get(1..))?, ":")
                    } else {
                        key.clone()
                    };
                    result.push((key, Expr::parse(&value)?));
                }
                Expr::Dict(result)
            } else if token.starts_with("[") && token.ends_with("]") {
                let token = trim!(token, "[", "]");
                let mut list = vec![];
                for elm in tokenize(token, &[","])? {
                    list.push(Expr::parse(&elm)?);
                }
                Expr::List(list)
            } else if token.starts_with("\"") && token.ends_with("\"") {
                let str = trim!(token, "\"", "\"");
                Expr::Value(Value::Str(str_escape(str)))
            // Text formatting
            } else if token.starts_with("f\"") && token.ends_with('"') {
                let str = trim!(token, "f\"", "\"");
                let str = str_format(str)?;
                let mut result = Expr::Value(Value::Str(String::new()));
                for elm in str {
                    if elm.starts_with("{") && elm.ends_with("}") {
                        let elm = trim!(elm, "{", "}");
                        result = Expr::Infix(Box::new(Operator::Add(
                            result,
                            Expr::Infix(Box::new(Operator::As(
                                Expr::Block(Block::parse(elm)?).optimize(),
                                Expr::Value(Value::Type(Type::Str)),
                            ))),
                        )));
                    } else {
                        result = Expr::Infix(Box::new(Operator::Add(
                            result,
                            Expr::Value(Value::Str(str_escape(&elm))),
                        )));
                    }
                }
                result
            // Funcize operator
            } else if token.starts_with("`") && token.ends_with("`") {
                let token = trim!(token, "`", "`");
                let source = format!("λx.λy.(x {token} y)");
                let expr = Expr::parse(&source)?;
                if format!("{expr}") != source {
                    return Err(Fault::Syntax);
                }
                expr
            // Lambda abstract that original formula in the theory
            } else if token.starts_with("λ") && token.contains(".") {
                let token = remove!(token, "λ");
                let (arg, body) = ok!(token.split_once("."))?;
                Expr::Value(Value::Func(Func::UserDefined(
                    arg.to_string(),
                    Box::new(Expr::parse(body)?),
                )))
            // Lambda abstract using back-slash instead of lambda mark
            } else if token.starts_with("\\") && token.contains(".") {
                let token = remove!(token, "\\");
                let (arg, body) = ok!(token.split_once("."))?;
                Expr::Value(Value::Func(Func::UserDefined(
                    arg.to_string(),
                    Box::new(Expr::parse(body)?),
                )))
            // Imperative style syntactic sugar of lambda abstract
            } else if token.starts_with("fn(") && token.contains("->") && token.ends_with(")") {
                let token = trim!(token, "fn(", ")");
                let (args, body) = ok!(token.split_once("->"))?;
                let mut args: Vec<&str> = args.split(",").collect();
                args.reverse();
                let mut func = Expr::Value(Value::Func(Func::UserDefined(
                    ok!(args.first())?.trim().to_string(),
                    Box::new(Expr::Block(Block::parse(body)?).optimize()),
                )));
                // Currying
                for arg in ok!(args.get(1..))? {
                    func = Expr::Value(Value::Func(Func::UserDefined(
                        arg.trim().to_string(),
                        Box::new(func),
                    )));
                }
                func
            // Imperative style syntactic sugar of list access by index
            } else if token.contains('[') && token.ends_with(']') {
                let token = trim!(token, "", "]");
                let (name, args) = ok!(token.split_once("["))?;
                Expr::Infix(Box::new(Operator::Access(
                    Expr::parse(name.trim())?,
                    Expr::parse(args)?,
                )))
            // Imperative style syntactic sugar of Function application
            } else if token.contains('(') && token.ends_with(')') {
                let token = trim!(token, "", ")");
                let (name, args) = ok!(token.split_once("("))?;
                let is_lazy = name.ends_with("?");
                let args = tokenize(args, &[","])?;
                let mut call = Expr::Infix(Box::new(Operator::Apply(
                    Expr::parse(if is_lazy { trim!(name, "", "?") } else { name })?,
                    is_lazy,
                    Expr::parse(ok!(args.first())?)?,
                )));
                for arg in ok!(args.get(1..))? {
                    call = Expr::Infix(Box::new(Operator::Apply(call, is_lazy, Expr::parse(arg)?)));
                }
                call
            // Object-oriented style syntactic sugar of access operator
            } else if token.matches(".").count() >= 1 {
                let (obj, key) = ok!(token.rsplit_once("."))?;
                Expr::Infix(Box::new(Operator::Access(
                    Expr::parse(obj)?,
                    Expr::Value(Value::Str(key.trim().to_string())),
                )))
            } else if token == "null" {
                Expr::Value(Value::Null)
            } else if is_identifier(&token) {
                Expr::Refer(token)
            } else {
                return Err(Fault::Syntax);
            }
            .optimize())
        }
    }

    /// Beta reduction of constant arguments when apply Function
    fn replace(&self, from: &Expr, to: &Expr) -> Expr {
        match self {
            Expr::List(list) => Expr::List(
                list.iter()
                    .map(|i| i.replace(from, to))
                    .collect::<Vec<Expr>>(),
            ),
            Expr::Dict(st) => Expr::Dict(
                st.iter()
                    .map(|(k, x)| (k.to_owned(), x.replace(from, to)))
                    .collect::<Vec<(String, Expr)>>(),
            ),
            Expr::Infix(infix) => Expr::Infix(Box::new(infix.replace(from, to))),
            Expr::Block(block) => Expr::Block(block.replace(from, to)),
            Expr::Refer(val) => {
                if let Expr::Refer(from) = from {
                    if val == from {
                        to.clone()
                    } else {
                        self.clone()
                    }
                } else {
                    self.clone()
                }
            }
            Expr::Value(Value::Func(Func::UserDefined(arg, func))) => {
                Expr::Value(Value::Func(Func::UserDefined(
                    arg.to_string(),
                    // Protect from duplicate replacing
                    if format!("{from}") == "self" || format!("{from}") == *arg {
                        func.clone()
                    } else {
                        Box::new(func.replace(from, to))
                    },
                )))
            }
            Expr::Value(val) => Expr::Value(val.clone()),
        }
    }

    fn optimize_mut(&mut self) {
        if let Expr::Block(Block(vec)) = self {
            if vec.len() == 1 {
                if let Statement::Expr(expr) = vec[0].clone() {
                    *self = expr;
                }
            }
        }
    }

    fn optimize(&self) -> Self {
        let mut expr = self.clone();
        expr.optimize_mut();
        expr
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Expr::Refer(refer) => refer.to_string(),
                Expr::List(list) => format!(
                    "[{}]",
                    list.iter()
                        .map(|i| format!("{i}"))
                        .collect::<Vec<String>>()
                        .join(", "),
                ),
                Expr::Infix(infix) => format!("({infix})"),
                Expr::Value(val) => format!("{val}"),
                Expr::Block(block) => format!("{block}"),
                Expr::Dict(st) => format!(
                    "{{ {} }}",
                    st.iter()
                        .map(|(k, v)| format!("{k}: {v}"))
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
            }
        )
    }
}

#[derive(Clone, Debug)]
enum Func {
    BuiltIn(fn(Value, &mut Engine) -> Result<Value, Fault>),
    UserDefined(String, Box<Expr>),
}

#[derive(Debug, Clone)]
enum Operator {
    Add(Expr, Expr),
    Sub(Expr, Expr),
    Mul(Expr, Expr),
    Div(Expr, Expr),
    Mod(Expr, Expr),
    Pow(Expr, Expr),
    Equal(Expr, Expr),
    NotEq(Expr, Expr),
    LessThan(Expr, Expr),
    LessThanEq(Expr, Expr),
    GreaterThan(Expr, Expr),
    GreaterThanEq(Expr, Expr),
    And(Expr, Expr),
    Or(Expr, Expr),
    Not(Expr),
    Access(Expr, Expr),
    As(Expr, Expr),
    Apply(Expr, bool, Expr),
    PipeLine(Expr, Expr),
    Assign(Expr, Expr),
    AssignAdd(Expr, Expr),
    AssignSub(Expr, Expr),
    AssignMul(Expr, Expr),
    AssignDiv(Expr, Expr),
    AssignMod(Expr, Expr),
    AssignPow(Expr, Expr),
    To(Expr, Expr),
}

impl Operator {
    fn eval(&self, engine: &mut Engine) -> Result<Value, Fault> {
        Ok(match self {
            Operator::Add(lhs, rhs) => {
                let lhs = lhs.eval(engine)?;
                let rhs = rhs.eval(engine)?;
                if let (Value::Num(lhs), Value::Num(rhs)) = (&lhs, &rhs) {
                    Value::Num(lhs + rhs)
                } else if let (Value::Str(lhs), Value::Str(rhs)) = (&lhs, &rhs) {
                    Value::Str(lhs.clone() + rhs)
                } else if let (Value::List(lhs), Value::List(rhs)) = (&lhs, &rhs) {
                    Value::List([lhs.clone(), rhs.clone()].concat())
                } else if let (Value::Dict(mut lhs), Value::Dict(rhs)) = (lhs.clone(), &rhs) {
                    lhs.extend(rhs.clone());
                    Value::Dict(lhs)
                } else {
                    return Err(Fault::Infix(self.clone()));
                }
            }
            Operator::Sub(lhs, rhs) => {
                let lhs = lhs.eval(engine)?;
                let rhs = rhs.eval(engine)?;
                if let (Value::Num(lhs), Value::Num(rhs)) = (&lhs, &rhs) {
                    Value::Num(lhs - rhs)
                } else {
                    lhs.modify_inside(&rhs, &None, engine)?
                }
            }
            Operator::Mul(lhs, rhs) => {
                let lhs = lhs.eval(engine)?;
                let rhs = rhs.eval(engine)?;
                if let (Value::Num(lhs), Value::Num(rhs)) = (&lhs, &rhs) {
                    Value::Num(lhs * rhs)
                } else if let (Value::Str(lhs), Value::Num(rhs)) = (&lhs, &rhs) {
                    Value::Str(lhs.repeat(*rhs as usize))
                } else if let (Value::List(lhs), Value::Num(rhs)) = (lhs, rhs) {
                    Value::List((0..rhs as usize).flat_map(|_| lhs.clone()).collect())
                } else {
                    return Err(Fault::Infix(self.clone()));
                }
            }
            Operator::Div(lhs, rhs) => {
                let lhs = lhs.eval(engine)?;
                let rhs = rhs.eval(engine)?;
                if let (Value::Num(lhs), Value::Num(rhs)) = (&lhs, &rhs) {
                    Value::Num(lhs / rhs)
                } else if let (Value::Str(lhs), Value::Str(rhs)) = (&lhs, &rhs) {
                    Value::List(lhs.split(rhs).map(|i| Value::Str(i.to_string())).collect())
                } else {
                    return Err(Fault::Infix(self.clone()));
                }
            }
            Operator::Mod(lhs, rhs) => {
                Value::Num(lhs.eval(engine)?.get_number()? % rhs.eval(engine)?.get_number()?)
            }
            Operator::Pow(lhs, rhs) => Value::Num(
                lhs.eval(engine)?
                    .get_number()?
                    .powf(rhs.eval(engine)?.get_number()?),
            ),
            Operator::Equal(lhs, rhs) => {
                let lhs = lhs.eval(engine)?;
                let rhs = rhs.eval(engine)?;
                if rhs.is_match(&lhs) {
                    rhs
                } else {
                    return Err(Fault::Logic(self.clone()));
                }
            }
            Operator::NotEq(lhs, rhs) => {
                let lhs = lhs.eval(engine)?;
                let rhs = rhs.eval(engine)?;
                if !rhs.is_match(&lhs) {
                    rhs
                } else {
                    return Err(Fault::Logic(self.clone()));
                }
            }
            Operator::LessThan(lhs, rhs) => {
                let rhs = rhs.eval(engine)?;
                if lhs.eval(engine)?.get_number()? < rhs.get_number()? {
                    rhs
                } else {
                    return Err(Fault::Logic(self.clone()));
                }
            }
            Operator::LessThanEq(lhs, rhs) => {
                let rhs = rhs.eval(engine)?;
                if lhs.eval(engine)?.get_number()? <= rhs.get_number()? {
                    rhs
                } else {
                    return Err(Fault::Logic(self.clone()));
                }
            }
            Operator::GreaterThan(lhs, rhs) => {
                let rhs = rhs.eval(engine)?;
                if lhs.eval(engine)?.get_number()? > rhs.get_number()? {
                    rhs
                } else {
                    return Err(Fault::Logic(self.clone()));
                }
            }
            Operator::GreaterThanEq(lhs, rhs) => {
                let rhs = rhs.eval(engine)?;
                if lhs.eval(engine)?.get_number()? >= rhs.get_number()? {
                    rhs
                } else {
                    return Err(Fault::Logic(self.clone()));
                }
            }
            Operator::And(lhs, rhs) => {
                let rhs = rhs.eval(engine);
                if lhs.eval(engine).is_ok() && rhs.is_ok() {
                    rhs?
                } else {
                    return Err(Fault::Logic(self.clone()));
                }
            }
            Operator::Or(lhs, rhs) => {
                let lhs = lhs.eval(engine);
                let rhs = rhs.eval(engine);
                if lhs.is_ok() || rhs.is_ok() {
                    rhs.unwrap_or(lhs?)
                } else {
                    return Err(Fault::Logic(self.clone()));
                }
            }
            Operator::Not(val) => {
                let val = val.eval(engine);
                if val.is_ok() {
                    return Err(Fault::Logic(self.clone()));
                } else {
                    Value::Null
                }
            }
            Operator::Access(lhs, rhs) => {
                let lhs = lhs.eval(engine)?;
                let rhs = rhs.eval(engine)?;
                let err = Err(Fault::Apply(lhs.clone()));
                match lhs.clone() {
                    Value::List(list) => match rhs.clone() {
                        Value::Num(index) => {
                            ok!(list.get(index as usize), Fault::Index(rhs, lhs))?.clone()
                        }
                        Value::Range(start, end) => {
                            let mut result = Vec::new();
                            for i in start..end {
                                result.push(ok!(
                                    list.get(i).cloned(),
                                    Fault::Index(rhs.clone(), lhs.clone())
                                )?);
                            }
                            Value::List(result)
                        }
                        Value::List(query) => {
                            let index = ok!(
                                list.windows(query.len()).position(
                                    |i| Value::List(i.to_vec()) == Value::List(query.clone())
                                ),
                                Fault::Key(rhs, lhs)
                            )?;
                            Value::Range(index, index + query.len())
                        }
                        _ => return err,
                    },
                    Value::Str(str) => match rhs.clone() {
                        Value::Num(index) => {
                            let str = char_vec!(str);
                            Value::Str(ok!(
                                str.get(index as usize).cloned(),
                                Fault::Index(rhs.clone(), lhs.clone())
                            )?)
                        }
                        Value::Range(start, end) => {
                            let mut result = String::new();
                            let str: Vec<char> = str.chars().collect();
                            for i in start..end {
                                result.push(ok!(
                                    str.get(i).cloned(),
                                    Fault::Index(rhs.clone(), lhs.clone())
                                )?);
                            }
                            Value::Str(result)
                        }
                        Value::Str(query) => {
                            let index = ok!(str.find(&query), Fault::Key(rhs, lhs))?;
                            Value::Range(index, index + query.chars().count())
                        }
                        _ => return err,
                    },
                    Value::Dict(st) => match rhs.clone() {
                        Value::Str(key) => ok!(st.get(&key), Fault::Key(rhs, lhs))?.clone(),
                        _ => return err,
                    },
                    _ => return err,
                }
            }
            Operator::As(lhs, rhs) => {
                let lhs = lhs.eval(engine)?;
                let rhs = rhs.eval(engine)?;
                lhs.cast(&rhs.get_type()?)?
            }
            Operator::Apply(lhs, is_lazy, rhs) => {
                let func = lhs.eval(engine)?;
                if let Value::Func(obj) = func.clone() {
                    match obj {
                        Func::BuiltIn(func) => func(rhs.eval(engine)?, engine)?,
                        Func::UserDefined(parameter, code) => {
                            let code = code
                                .replace(
                                    &Expr::Refer(parameter),
                                    &if *is_lazy {
                                        rhs.clone()
                                    } else {
                                        Expr::Value(rhs.eval(engine)?)
                                    },
                                )
                                .replace(&Expr::Refer("self".to_string()), &Expr::Value(func));
                            code.eval(&mut engine.clone())?
                        }
                    }
                } else {
                    return Err(Fault::Apply(func));
                }
            }
            Operator::PipeLine(lhs, rhs) => {
                Operator::Apply(rhs.to_owned(), false, lhs.to_owned()).eval(engine)?
            }
            Operator::Assign(lhs, rhs) => {
                Statement::Let(lhs.to_owned(), false, rhs.to_owned()).eval(engine)?
            }
            Operator::AssignAdd(name, rhs) => Operator::Assign(
                name.to_owned(),
                Expr::Infix(Box::new(Operator::Add(name.to_owned(), rhs.clone()))),
            )
            .eval(engine)?,
            Operator::AssignSub(name, rhs) => Operator::Assign(
                name.to_owned(),
                Expr::Infix(Box::new(Operator::Sub(name.to_owned(), rhs.clone()))),
            )
            .eval(engine)?,
            Operator::AssignMul(name, rhs) => Operator::Assign(
                name.to_owned(),
                Expr::Infix(Box::new(Operator::Mul(name.to_owned(), rhs.clone()))),
            )
            .eval(engine)?,
            Operator::AssignDiv(name, rhs) => Operator::Assign(
                name.to_owned(),
                Expr::Infix(Box::new(Operator::Div(name.to_owned(), rhs.clone()))),
            )
            .eval(engine)?,
            Operator::AssignMod(name, rhs) => Operator::Assign(
                name.to_owned(),
                Expr::Infix(Box::new(Operator::Mod(name.to_owned(), rhs.clone()))),
            )
            .eval(engine)?,
            Operator::AssignPow(name, rhs) => Operator::Assign(
                name.to_owned(),
                Expr::Infix(Box::new(Operator::Pow(name.to_owned(), rhs.clone()))),
            )
            .eval(engine)?,
            Operator::To(from, to) => {
                let from = from.eval(engine)?.get_number()? as usize;
                let to = to.eval(engine)?.get_number()? as usize;
                if from < to {
                    Value::Range(from, to)
                } else {
                    return Err(Fault::Syntax);
                }
            }
        })
    }

    fn parse(source: &str) -> Result<Self, Fault> {
        let token_list: Vec<String> = tokenize(source, SPACE.as_ref())?;
        let token = Expr::parse(ok!(token_list.last())?)?;
        let operator = ok!(token_list.get(ok!(token_list.len().checked_sub(2))?))?;
        let has_lhs =
            |len: usize| Expr::parse(&join!(ok!(token_list.get(..token_list.len() - len))?));
        Ok(match operator.as_str() {
            "+" => Operator::Add(has_lhs(2)?, token),
            "-" => Operator::Sub(has_lhs(2)?, token),
            "*" => Operator::Mul(has_lhs(2)?, token),
            "/" => Operator::Div(has_lhs(2)?, token),
            "%" => Operator::Mod(has_lhs(2)?, token),
            "^" => Operator::Pow(has_lhs(2)?, token),
            "==" => Operator::Equal(has_lhs(2)?, token),
            "!=" => Operator::NotEq(has_lhs(2)?, token),
            "<" => Operator::LessThan(has_lhs(2)?, token),
            "<=" => Operator::LessThanEq(has_lhs(2)?, token),
            ">" => Operator::GreaterThan(has_lhs(2)?, token),
            ">=" => Operator::GreaterThanEq(has_lhs(2)?, token),
            "&" => Operator::And(has_lhs(2)?, token),
            "|" => Operator::Or(has_lhs(2)?, token),
            "?" => Operator::Apply(has_lhs(2)?, true, token),
            "::" => Operator::Access(has_lhs(2)?, token),
            "as" => Operator::As(has_lhs(2)?, token),
            "|>" => Operator::PipeLine(has_lhs(2)?, token),
            ":=" => Operator::Assign(has_lhs(2)?, token),
            "+=" => Operator::AssignAdd(has_lhs(2)?, token),
            "-=" => Operator::AssignSub(has_lhs(2)?, token),
            "*=" => Operator::AssignMul(has_lhs(2)?, token),
            "/=" => Operator::AssignDiv(has_lhs(2)?, token),
            "%=" => Operator::AssignMod(has_lhs(2)?, token),
            "^=" => Operator::AssignPow(has_lhs(2)?, token),
            "~" => Operator::To(has_lhs(2)?, token),
            operator => {
                if operator.starts_with("`") && operator.ends_with("`") {
                    let operator = operator[1..operator.len() - 1].to_string();
                    Operator::Apply(
                        Expr::Infix(Box::new(Operator::Apply(
                            Expr::parse(&operator)?,
                            false,
                            has_lhs(2)?,
                        ))),
                        false,
                        token,
                    )
                } else {
                    Operator::Apply(has_lhs(1)?, false, token)
                }
            }
        })
    }

    fn replace(&self, from: &Expr, to: &Expr) -> Self {
        match self {
            Operator::Add(lhs, rhs) => Operator::Add(lhs.replace(from, to), rhs.replace(from, to)),
            Operator::Sub(lhs, rhs) => Operator::Sub(lhs.replace(from, to), rhs.replace(from, to)),
            Operator::Mul(lhs, rhs) => Operator::Mul(lhs.replace(from, to), rhs.replace(from, to)),
            Operator::Div(lhs, rhs) => Operator::Div(lhs.replace(from, to), rhs.replace(from, to)),
            Operator::Mod(lhs, rhs) => Operator::Mod(lhs.replace(from, to), rhs.replace(from, to)),
            Operator::Pow(lhs, rhs) => Operator::Pow(lhs.replace(from, to), rhs.replace(from, to)),
            Operator::Equal(lhs, rhs) => {
                Operator::Equal(lhs.replace(from, to), rhs.replace(from, to))
            }
            Operator::NotEq(lhs, rhs) => {
                Operator::NotEq(lhs.replace(from, to), rhs.replace(from, to))
            }
            Operator::LessThan(lhs, rhs) => {
                Operator::LessThan(lhs.replace(from, to), rhs.replace(from, to))
            }
            Operator::LessThanEq(lhs, rhs) => {
                Operator::LessThanEq(lhs.replace(from, to), rhs.replace(from, to))
            }
            Operator::GreaterThan(lhs, rhs) => {
                Operator::GreaterThan(lhs.replace(from, to), rhs.replace(from, to))
            }
            Operator::GreaterThanEq(lhs, rhs) => {
                Operator::GreaterThanEq(lhs.replace(from, to), rhs.replace(from, to))
            }
            Operator::And(lhs, rhs) => Operator::And(lhs.replace(from, to), rhs.replace(from, to)),
            Operator::Or(lhs, rhs) => Operator::Or(lhs.replace(from, to), rhs.replace(from, to)),
            Operator::Not(val) => Operator::Not(val.replace(from, to)),
            Operator::Access(lhs, rhs) => {
                Operator::Access(lhs.replace(from, to), rhs.replace(from, to))
            }
            Operator::As(lhs, rhs) => Operator::As(lhs.replace(from, to), rhs.replace(from, to)),
            Operator::Apply(lhs, is_lazy, rhs) => {
                Operator::Apply(lhs.replace(from, to), *is_lazy, rhs.replace(from, to))
            }
            Operator::Assign(lhs, rhs) => {
                Operator::Assign(lhs.replace(from, to), rhs.replace(from, to))
            }
            Operator::PipeLine(lhs, rhs) => {
                Operator::PipeLine(lhs.replace(from, to), rhs.replace(from, to))
            }
            Operator::AssignAdd(lhs, rhs) => {
                Operator::AssignAdd(lhs.replace(from, to), rhs.replace(from, to))
            }
            Operator::AssignSub(lhs, rhs) => {
                Operator::AssignSub(lhs.replace(from, to), rhs.replace(from, to))
            }
            Operator::AssignMul(lhs, rhs) => {
                Operator::AssignMul(lhs.replace(from, to), rhs.replace(from, to))
            }
            Operator::AssignDiv(lhs, rhs) => {
                Operator::AssignDiv(lhs.replace(from, to), rhs.replace(from, to))
            }
            Operator::AssignMod(lhs, rhs) => {
                Operator::AssignMod(lhs.replace(from, to), rhs.replace(from, to))
            }
            Operator::AssignPow(lhs, rhs) => {
                Operator::AssignPow(lhs.replace(from, to), rhs.replace(from, to))
            }
            Operator::To(lhs, rhs) => Operator::To(lhs.replace(from, to), rhs.replace(from, to)),
        }
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Operator::Add(lhs, rhs) => format!("{lhs} + {rhs}"),
                Operator::Sub(lhs, rhs) => format!("{lhs} - {rhs}"),
                Operator::Mul(lhs, rhs) => format!("{lhs} * {rhs}"),
                Operator::Div(lhs, rhs) => format!("{lhs} / {rhs}"),
                Operator::Mod(lhs, rhs) => format!("{lhs} % {rhs}"),
                Operator::Pow(lhs, rhs) => format!("{lhs} ^ {rhs}"),
                Operator::Equal(lhs, rhs) => format!("{lhs} == {rhs}"),
                Operator::NotEq(lhs, rhs) => format!("{lhs} != {rhs}"),
                Operator::LessThan(lhs, rhs) => format!("{lhs} < {rhs}"),
                Operator::LessThanEq(lhs, rhs) => format!("{lhs} <= {rhs}"),
                Operator::GreaterThan(lhs, rhs) => format!("{lhs} > {rhs}"),
                Operator::GreaterThanEq(lhs, rhs) => format!("{lhs} >= {rhs}"),
                Operator::And(lhs, rhs) => format!("{lhs} & {rhs}"),
                Operator::Or(lhs, rhs) => format!("{lhs} | {rhs}"),
                Operator::Not(val) => format!("!{val}"),
                Operator::Access(lhs, rhs) => format!("{lhs} :: {rhs}"),
                Operator::As(lhs, rhs) => format!("{lhs} as {rhs}",),
                Operator::Assign(lhs, rhs) => format!("{lhs} := {rhs}",),
                Operator::PipeLine(lhs, rhs) => format!("{lhs} |> {rhs}"),
                Operator::Apply(lhs, true, rhs) => format!("{lhs} ? {rhs}"),
                Operator::Apply(lhs, false, rhs) => format!("{lhs} {rhs}"),
                Operator::AssignAdd(lhs, rhs) => format!("{lhs} += {rhs}"),
                Operator::AssignSub(lhs, rhs) => format!("{lhs} -= {rhs}"),
                Operator::AssignMul(lhs, rhs) => format!("{lhs} *= {rhs}"),
                Operator::AssignDiv(lhs, rhs) => format!("{lhs} /= {rhs}"),
                Operator::AssignMod(lhs, rhs) => format!("{lhs} %= {rhs}"),
                Operator::AssignPow(lhs, rhs) => format!("{lhs} ^= {rhs}"),
                Operator::To(lhs, rhs) => format!("{lhs} ~ {rhs}",),
            }
        )
    }
}

#[derive(Error)]
enum Fault {
    #[error("can not apply Function because `{0}` is not lambda abstract")]
    Apply(Value),

    #[error("key `{0}` is not found in the dict `{1}`")]
    Key(Value, Value),

    #[error("index `{0}` is out of the sequence `{1}`")]
    Index(Value, Value),

    #[error("access is denied because it's protected memory area")]
    AccessDenied,

    #[error("can not access undefined variable `{0}`")]
    Refer(String),

    #[error("can not type cast `{0}` to {1}")]
    Cast(Value, Type),

    #[error("at the IO processing has problem")]
    IO,

    #[error("the value `{0}` is different to expected type `{1}`")]
    Value(Value, Type),

    #[error("missmatching of arguments length when Function application")]
    ArgLen,

    #[error("the program is not able to parse. check out is the syntax correct")]
    Syntax,

    #[error("can not evaluate expression `{0}`")]
    Infix(Operator),

    #[error("the logical operation `{0}` has bankruptcy")]
    Logic(Operator),

    #[error("{}", _0.clone().unwrap_or("throwed by user-defined program".to_string()))]
    General(Option<String>),
}

impl Debug for Fault {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

#[derive(Debug, Clone)]
enum Value {
    Num(f64),
    Str(String),
    List(Vec<Value>),
    Range(usize, usize),
    Func(Func),
    Type(Type),
    Dict(IndexMap<String, Value>),
    Null,
}

impl Value {
    fn cast(&self, sig: &Type) -> Result<Value, Fault> {
        let err = Err(Fault::Cast(self.clone(), sig.clone()));
        Ok(match sig {
            Type::Num => Value::Num(match self {
                Value::Num(n) => n.to_owned(),
                Value::Str(s) => {
                    if let Ok(n) = s.trim().parse::<f64>() {
                        n
                    } else {
                        return err;
                    }
                }
                Value::Null => 0.0,
                _ => return err,
            }),
            Type::Str => Value::Str(match self {
                Value::Str(s) => s.to_string(),
                Value::Null => String::new(),
                _ => format!("{self}"),
            }),
            Type::List => Value::List(match self {
                Value::List(list) => list.to_owned(),
                Value::Str(str) => str.chars().map(|i| Value::Str(i.to_string())).collect(),
                Value::Dict(strct) => strct
                    .iter()
                    .map(|(k, y)| Value::List(vec![Value::Str(k.to_owned()), y.to_owned()]))
                    .collect(),
                Value::Range(start, end) => {
                    let mut range: Vec<Value> = vec![];
                    let mut current = *start;
                    while current < *end {
                        range.push(Value::Num(current as f64));
                        current += 1;
                    }
                    range
                }
                Value::Null => Vec::new(),
                _ => return err,
            }),
            Type::Kind => Value::Type(Type::parse(&self.get_str()?)?),
            _ => return err,
        })
    }

    fn get_number(&self) -> Result<f64, Fault> {
        match self {
            Value::Num(n) => Ok(n.to_owned()),
            _ => Err(Fault::Value(self.clone(), Type::Num)),
        }
    }

    fn get_str(&self) -> Result<String, Fault> {
        match self {
            Value::Str(s) => Ok(s.to_string()),
            _ => Err(Fault::Value(self.clone(), Type::Str)),
        }
    }

    fn get_list(&self) -> Result<Vec<Value>, Fault> {
        match self {
            Value::List(list) => Ok(list.to_owned()),
            _ => Err(Fault::Value(self.clone(), Type::List)),
        }
    }

    fn get_dict(&self) -> Result<IndexMap<String, Value>, Fault> {
        match self {
            Value::Dict(list) => Ok(list.to_owned()),
            _ => Err(Fault::Value(self.clone(), Type::List)),
        }
    }

    fn get_type(&self) -> Result<Type, Fault> {
        match self {
            Value::Type(sig) => Ok(sig.to_owned()),
            _ => Err(Fault::Value(self.clone(), Type::Kind)),
        }
    }

    fn type_of(&self) -> Type {
        match self {
            Value::Num(_) => Type::Num,
            Value::Str(_) => Type::Str,
            Value::List(_) => Type::List,
            Value::Range(_, _) => Type::Range,
            Value::Func(_) => Type::Func,
            Value::Type(_) => Type::Kind,
            Value::Dict(_) => Type::Dict,
            Value::Null => Type::Kind,
        }
    }

    fn is_match(&self, pattern: &Value) -> bool {
        if let (Value::List(list), Value::List(pats)) = (self, pattern) {
            if list.len() != pats.len() {
                return false;
            }
            for (elm, pat) in list.iter().zip(pats) {
                if !elm.is_match(pat) {
                    return false;
                }
            }
            true
        } else if let (Value::Dict(strct), Value::Dict(pats)) = (self, pattern) {
            if strct.len() != pats.len() {
                return false;
            }
            for (elm, pat) in strct.iter().zip(pats) {
                if elm.0 != pat.0 || !elm.1.is_match(pat.1) {
                    return false;
                }
            }
            true
        } else if format!("{pattern}") == "_" {
            true
        } else {
            self == pattern
        }
    }

    fn modify_inside(
        &self,
        index: &Value,
        val: &Option<Value>,
        engine: &mut Engine,
    ) -> Result<Value, Fault> {
        let err = Err(Fault::Infix(Operator::Access(
            Expr::Value(self.clone()),
            Expr::Value(index.clone()),
        )));
        Ok(match self.clone() {
            Value::List(mut list) => match index.clone() {
                Value::Num(index) => {
                    index_check!(list, index, self);
                    if let Some(val) = val {
                        list[index as usize] = val.clone();
                    } else {
                        list.remove(index as usize);
                    }
                    Value::List(list)
                }
                Value::Range(start, end) => {
                    for _ in start..end {
                        index_check!(list, start as f64, self);
                        list.remove(start);
                    }
                    if let Some(val) = val {
                        list.insert(start, val.clone());
                    }
                    Value::List(list)
                }
                Value::List(_) => self.modify_inside(
                    &Operator::Access(Expr::Value(self.clone()), Expr::Value(index.clone()))
                        .eval(engine)?,
                    val,
                    engine,
                )?,
                _ => return err,
            },
            Value::Str(str) => match index.clone() {
                Value::Num(index) => {
                    let mut str = char_vec!(str);
                    index_check!(str, index, self);
                    if let Some(val) = val {
                        str[index as usize] = val.get_str()?;
                    } else {
                        str.remove(index as usize);
                    }
                    Value::Str(str.concat())
                }
                Value::Range(start, end) => {
                    let mut str = char_vec!(str);
                    for _ in start..end {
                        index_check!(str, start as f64, self);
                        str.remove(start);
                    }
                    if let Some(val) = val {
                        str.insert(start, val.get_str()?);
                    }
                    Value::Str(str.concat())
                }
                Value::Str(query) => {
                    if let Some(val) = val {
                        Value::Str(str.replace(&query, &val.get_str()?))
                    } else {
                        Value::Str(remove!(str, &query))
                    }
                }

                _ => return err,
            },
            Value::Dict(mut st) => match index {
                Value::Str(key) => {
                    if let Some(val) = val {
                        st.insert(key.clone(), val.clone());
                    } else {
                        st.shift_remove(key);
                    }
                    Value::Dict(st)
                }
                _ => return err,
            },
            _ => return err,
        })
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Value::Str(str) => format!(
                    "\"{}\"",
                    str.replace("\\", "\\\\")
                        .replace("'", "\\'")
                        .replace("\"", "\\\"")
                        .replace("`", "\\`")
                        .replace("\n", "\\n")
                        .replace("\t", "\\t")
                        .replace("\r", "\\r")
                ),
                Value::Num(n) => n.to_string(),
                Value::Null => "null".to_string(),
                Value::Func(Func::BuiltIn(obj)) => format!("λx.{obj:?}"),
                Value::Func(Func::UserDefined(arg, code)) => format!("λ{arg}.{code}"),
                Value::List(l) => format!(
                    "[{}]",
                    l.iter()
                        .map(|i| format!("{i}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
                Value::Range(start, end) => format!("({start} ~ {end})",),
                Value::Type(kind) => format!("{kind}"),
                Value::Dict(val) => format!(
                    "{{ {} }}",
                    val.iter()
                        .map(|(k, v)| format!("{k}: {v}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
            }
        )
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        format!("{self}") == format!("{other}")
    }
}

#[derive(Debug, Clone)]
enum Type {
    Num,
    Str,
    List,
    Range,
    Func,
    Kind,
    Dict,
}

impl Type {
    fn parse(token: &str) -> Result<Type, Fault> {
        let token = token.trim();
        Ok(if token == "num" {
            Type::Num
        } else if token == "str" {
            Type::Str
        } else if token == "list" {
            Type::List
        } else if token == "range" {
            Type::Range
        } else if token == "func" {
            Type::Func
        } else if token == "kind" {
            Type::Kind
        } else if token == "dict" {
            Type::Dict
        } else {
            return Err(Fault::Syntax);
        })
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Type::Num => "num".to_string(),
                Type::Str => "str".to_string(),
                Type::List => "list".to_string(),
                Type::Range => "range".to_string(),
                Type::Func => "func".to_string(),
                Type::Kind => "kind".to_string(),
                Type::Dict => "dict".to_string(),
            }
        )
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Type) -> bool {
        format!("{self}") == format!("{other}")
    }
}

fn tokenize(input: &str, delimiter: &[&str]) -> Result<Vec<String>, Fault> {
    let mut tokens: Vec<String> = Vec::new();
    let mut current_token = String::new();
    let mut in_parentheses: usize = 0;
    let mut in_quote = false;
    let mut is_escape = false;

    let chars: Vec<String> = input.chars().map(String::from).collect();
    let mut index = 0;

    while index < chars.len() {
        let include_letter = |x: &str| {
            chars
                .get(index..index + x.chars().count())
                .and_then(|y| Some(x == y.concat()))
                .unwrap_or(false)
        };
        let c = ok!(chars.get(index))?.to_owned();
        if is_escape {
            current_token.push_str(match c.as_str() {
                "n" => "\n",
                "t" => "\t",
                "r" => "\r",
                _ => &c,
            });
            is_escape = false;
            index += 1;
        } else if include_letter(BEGIN) {
            current_token.push_str(BEGIN);
            index += BEGIN.chars().count();
            in_parentheses += 1;
        } else if include_letter(END) {
            current_token.push_str(END);
            index += END.chars().count();
            if let Some(i) = in_parentheses.checked_sub(1) {
                in_parentheses = i;
            } else {
                return Err(Fault::Syntax);
            }
        } else if ["(", "[", "{"].contains(&c.as_str()) {
            current_token.push_str(&c.as_str());
            in_parentheses += 1;
            index += 1;
        } else if [")", "]", "}"].contains(&c.as_str()) {
            current_token.push_str(&c.as_str());
            if let Some(i) = in_parentheses.checked_sub(1) {
                in_parentheses = i;
            } else {
                return Err(Fault::Syntax);
            }
            index += 1;
        } else if ["\"", "'", "`"].contains(&c.as_str()) {
            in_quote = !in_quote;
            current_token.push_str(&c.as_str());
            index += 1;
        } else if c == "\\" {
            current_token.push_str(&c);
            is_escape = true;
            index += 1;
        } else {
            let mut is_delimit = false;
            'point: for delimit in delimiter {
                if include_letter(delimit)
                    && in_parentheses == 0
                    && !current_token.is_empty()
                    && !in_quote
                {
                    tokens.push(current_token.clone());
                    index += delimit.chars().count();
                    current_token.clear();
                    is_delimit = true;
                    break 'point;
                }
            }
            if !is_delimit {
                current_token.push_str(&c.as_str());
                index += 1;
            }
        }
    }

    // Syntax error check
    if is_escape || in_quote || in_parentheses != 0 {
        return Err(Fault::Syntax);
    }
    if !current_token.is_empty() {
        tokens.push(current_token.clone());
        current_token.clear();
    }
    Ok(tokens)
}

fn is_identifier(name: &str) -> bool {
    if name.is_empty() {
        return false;
    }
    if name == "_" {
        return true;
    }
    let mut chars = name.chars();
    let first_char = chars.next().unwrap();
    if !UnicodeXID::is_xid_start(first_char) {
        return false;
    }
    if !chars.all(UnicodeXID::is_xid_continue) {
        return false;
    }
    if RESERVED.contains(&name) {
        return false;
    }
    true
}

fn str_format(input: &str) -> Result<Vec<String>, Fault> {
    let mut tokens: Vec<String> = Vec::new();
    let mut current_token = String::new();
    let mut in_parentheses: usize = 0;
    let mut is_escape = false;

    for c in input.chars() {
        if is_escape {
            current_token.push(match c {
                'n' => '\n',
                't' => '\t',
                'r' => '\r',
                _ => c,
            });
            is_escape = false;
        } else {
            match c {
                '{' => {
                    if in_parentheses == 0 {
                        if !current_token.is_empty() {
                            tokens.push(current_token.clone());
                        }
                        current_token = c.to_string();
                    } else {
                        current_token.push(c)
                    }
                    in_parentheses += 1;
                }
                '}' => {
                    current_token.push(c);
                    if in_parentheses != 0 {
                        in_parentheses -= 1;
                    } else {
                        return Err(Fault::Syntax);
                    }
                    if in_parentheses == 0 {
                        if !current_token.is_empty() {
                            tokens.push(current_token.clone());
                        }
                        current_token.clear();
                    }
                }
                '\\' => {
                    current_token.push(c);
                    is_escape = true;
                }
                _ => current_token.push(c),
            }
        }
    }

    // Syntax error check
    if is_escape || in_parentheses != 0 {
        return Err(Fault::Syntax);
    }
    if !current_token.is_empty() {
        tokens.push(current_token.clone());
        current_token.clear();
    }
    Ok(tokens)
}

fn str_escape(str: &str) -> String {
    let mut result = String::new();
    let mut is_escape = false;
    for c in str.chars() {
        if is_escape {
            result.push(c);
            is_escape = false;
        } else {
            match c {
                '\\' => {
                    is_escape = true;
                }
                _ => result.push(c),
            }
        }
    }
    result
}
