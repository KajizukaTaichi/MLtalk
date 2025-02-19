use crate::*;

#[derive(Debug, Clone)]
pub struct Engine {
    pub scope: IndexMap<String, Value>,
    pub effective: IndexSet<String>,
    pub is_toplevel: bool,
    pub is_lazy: bool,
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
            mode: Mode::Pure,
            effective: IndexSet::new(),
            is_toplevel: true,
            is_lazy: false,
            scope: IndexMap::from([
                (
                    "stdlib".to_string(),
                    Value::Str("https://kajizukataichi.github.io/MLtalk/lib/std.ml".to_string()),
                ),
                (
                    "eval".to_string(),
                    Value::Func(Func::BuiltIn(
                        |expr, engine| Ok(Block::parse(&expr.get_str()?)?.eval(engine)?),
                        Type::Func(Some(Box::new((Type::Str, Type::Any))), Mode::Effect),
                    )),
                ),
                (
                    "type".to_string(),
                    Value::Func(Func::BuiltIn(
                        |expr, _| Ok(Value::Type(expr.type_of())),
                        Type::Func(Some(Box::new((Type::Any, Type::Kind))), Mode::Pure),
                    )),
                ),
                (
                    "alphaConvert".to_string(),
                    Value::Func(Func::BuiltIn(
                        |args, _| {
                            let args = args.get_list()?;
                            let func = ok!(args.first(), Fault::ArgLen)?.get_func()?;
                            let new_name = ok!(args.get(1), Fault::ArgLen)?.get_str()?;
                            let Func::UserDefined(old_name, body, anno) = func else {
                                return Err(Fault::Type(
                                    Value::Func(func.to_owned()),
                                    Type::Func(None, Mode::Pure),
                                ));
                            };
                            Ok(Value::Func(Func::UserDefined(
                                new_name.clone(),
                                Box::new(body.replace(
                                    &Expr::Refer(old_name.to_owned()),
                                    &Expr::Refer(new_name),
                                )),
                                anno.clone(),
                            )))
                        },
                        Type::Func(
                            Some(Box::new((
                                Type::List(Some(Box::new(Type::Union(vec![
                                    Type::Func(None, Mode::Pure),
                                    Type::Str,
                                ])))),
                                Type::Func(None, Mode::Pure),
                            ))),
                            Mode::Pure,
                        ),
                    )),
                ),
            ]),
        }
    }

    pub fn malloc(&mut self, name: &String, value: &Value) -> Result<(), Fault> {
        if is_identifier(name) {
            if name != "_" {
                self.scope.insert(name.clone(), value.clone());
            }
            Ok(())
        } else {
            Err(Fault::Syntax)
        }
    }

    pub fn access(&mut self, name: &str) -> Result<Value, Fault> {
        let val = ok!(self.scope.get(name), Fault::Refer(name.to_owned()))?.clone();
        if let Mode::Pure = self.mode {
            if Expr::Value(val.clone()).is_pure() {
                Ok(val)
            } else {
                Err(Fault::Pure(name.to_string()))
            }
        } else {
            Ok(val)
        }
    }

    pub fn effect_collection(&mut self) {
        for (name, value) in &self.scope {
            if Expr::Value(value.clone()).is_pure() {
                self.effective.insert(name.to_string());
            }
        }
    }
}
