use crate::*;

#[derive(Debug, Clone)]
pub struct Engine {
    scope: IndexMap<String, Value>,
    effect: IndexSet<String>,
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
            effect: IndexSet::new(),
            scope: IndexMap::from([
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
            ]),
        }
    }

    pub fn alloc(&mut self, name: &String, value: &Value) -> Result<(), Fault> {
        if is_identifier(name) {
            if name != "_" {
                self.scope.insert(name.clone(), value.clone());
            }
            Ok(())
        } else {
            Err(Fault::Syntax)
        }
    }

    pub fn access(&self, name: &str) -> Result<Value, Fault> {
        ok!(
            if let Mode::Pure = self.mode {
                if self.is_effective(name) {
                    return Err(Fault::Pure(name.to_string()));
                } else {
                    self.scope.get(name)
                }
            } else {
                self.scope.get(name)
            },
            Fault::Refer(name.to_owned())
        )
        .cloned()
    }

    pub fn set_effect(&mut self, name: &str) {
        self.effect.insert(name.to_string());
    }

    pub fn unset_effect(&mut self, name: &str) {
        self.effect.shift_remove(name);
    }

    pub fn is_effective(&self, name: &str) -> bool {
        self.effect.contains(&name.to_string())
    }
}
