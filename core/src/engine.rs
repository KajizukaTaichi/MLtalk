use crate::*;

#[derive(Debug, Clone)]
pub struct Engine {
    env: IndexMap<String, Value>,
    pure: IndexSet<String>,
    pub mode: Mode,
}

#[derive(Debug, Copy, Clone)]
pub enum Mode {
    Pure,
    Effect,
}

impl Engine {
    pub fn new() -> Engine {
        Engine {
            mode: Mode::Effect,
            pure: BUILTIN.to_vec().iter().map(|i| i.to_string()).collect(),
            env: IndexMap::from([
                (
                    "std".to_string(),
                    Value::Str("https://kajizukataichi.github.io/MLtalk/lib/std.ml".to_string()),
                ),
                (
                    "type".to_string(),
                    Value::Func(Func::BuiltIn(|expr, _| Ok(Value::Type(expr.type_of())))),
                ),
                (
                    "alphaConvert".to_string(),
                    Value::Func(Func::BuiltIn(|args, _| {
                        let args = args.get_list()?;
                        let func = ok!(args.first(), Fault::ArgLen)?;
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

    pub fn alloc(&mut self, name: &String, value: &Value) -> Result<(), Fault> {
        if self.is_pure(name) {
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

    pub fn access(&self, name: &str) -> Result<Value, Fault> {
        ok!(
            if let Mode::Pure = self.mode {
                if self.is_pure(name) {
                    self.env.get(name)
                } else if self.env.contains_key(name) {
                    return Err(Fault::Pure(name.to_string()));
                } else {
                    None
                }
            } else {
                self.env.get(name)
            },
            Fault::Refer(name.to_owned())
        )
        .cloned()
    }

    pub fn set_pure(&mut self, name: &str) {
        self.pure.insert(name.to_string());
    }

    pub fn is_pure(&self, name: &str) -> bool {
        self.pure.contains(&name.to_string())
    }
}
