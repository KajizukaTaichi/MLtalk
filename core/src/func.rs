use crate::*;

#[derive(Clone, Debug)]
pub enum Func {
    BuiltIn(fn(Value, &mut Engine) -> Result<Value, Fault>),
    UserDefined(String, Box<Expr>, Type),
}

impl Func {
    pub fn parse(source: &str) -> Result<Self, Fault> {
        // Lambda abstract that original formula in the theory
        if source.starts_with("λ") && source.contains(".") {
            let source = remove!(source, "λ");
            Self::init(&source)
        // Lambda abstract using back-slash instead of lambda mark
        } else if source.starts_with("\\") && source.contains(".") {
            let source = remove!(source, "\\");
            Self::init(&source)
        } else {
            Err(Fault::Syntax)
        }
    }

    fn init(source: &str) -> Result<Self, Fault> {
        let (arg, body) = ok!(source.split_once("."))?;
        let arg = arg.trim();
        if arg.is_empty() {
            return Err(Fault::Syntax);
        }
        let splited_body = tokenize(body, ["->"].as_slice(), false);
        let (arg, body, annotation) =
            if let (Some((arg, ano_arg)), Ok(body)) = (arg.split_once(":"), splited_body) {
                let ano_ret = ok!(body.last())?;
                let body = join!(ok!(body.get(..body.len() - 1))?, "->");
                if let Some((ano_ret, "effect")) =
                    ano_ret.rsplit_once("+").map(|x| (x.0.trim(), x.1.trim()))
                {
                    (
                        arg,
                        body,
                        Type::Func(
                            Some(Box::new((Type::parse(ano_arg)?, Type::parse(ano_ret)?))),
                            Mode::Effect,
                        ),
                    )
                } else {
                    (
                        arg,
                        body,
                        Type::Func(
                            Some(Box::new((Type::parse(ano_arg)?, Type::parse(ano_ret)?))),
                            Mode::Pure,
                        ),
                    )
                }
            } else {
                (arg, body.to_string(), Type::Func(None, Mode::Pure))
            };
        if !is_identifier(arg) {
            return Err(Fault::Syntax);
        }
        Ok(Func::UserDefined(
            arg.to_string(),
            Box::new(Expr::parse(&body)?),
            annotation,
        ))
    }

    pub fn bind(&self, anno: Type) -> Result<Self, Fault> {
        let Func::UserDefined(arg, body, _) = self else {
            return Err(Fault::Syntax);
        };
        if let Type::Func(Some(inner), mode) = anno {
            Ok(Func::UserDefined(
                arg.to_owned(),
                if let Expr::Value(Value::Func(func)) = *body.clone() {
                    Box::new(Expr::Value(Value::Func(func.bind(inner.1.clone())?)))
                } else {
                    body.clone()
                },
                Type::Func(Some(Box::new((inner.0, inner.1))), mode),
            ))
        } else {
            Ok(self.clone())
        }
    }

    pub fn apply(&self, rhs: Expr, engine: &mut Engine) -> Result<Value, Fault> {
        Ok(match self {
            Func::BuiltIn(func) => func(rhs.eval(engine)?, engine)?,
            Func::UserDefined(argument, code, Type::Func(type_annotate, func_mode)) => {
                let code = code.replace(
                    &Expr::Refer(argument.to_string()),
                    &if engine.is_lazy {
                        rhs.clone()
                    } else {
                        let val = rhs.eval(engine)?;
                        if let Some(arg) = type_annotate.clone() {
                            if arg.0 != val.type_of() {
                                return Err(Fault::Type(val, arg.0));
                            }
                            Expr::Value(val)
                        } else {
                            Expr::Value(val)
                        }
                    },
                );

                // Check effect
                if let Mode::Pure = engine.mode {
                    if let Mode::Effect = func_mode {
                        return Err(Fault::Pure(Value::Func(self.clone()).to_string()));
                    }
                }

                // Create function's scope
                let func_engine = &mut engine.clone();
                func_engine.is_toplevel = false;

                let result = code.eval(func_engine)?;
                if let Some(arg) = type_annotate {
                    if arg.1 != result.type_of() {
                        return Err(Fault::Type(result, arg.1.clone()));
                    }
                    result
                } else {
                    result
                }
            }
            Func::UserDefined(_, _, other_type) => {
                return Err(Fault::Type(Value::Func(self.clone()), other_type.clone()))
            }
        })
    }
}
