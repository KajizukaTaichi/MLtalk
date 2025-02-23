use crate::*;

#[derive(Clone, Debug)]
pub enum Func {
    BuiltIn(fn(Value, &mut Engine) -> Result<Value, Fault>, Type),
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
                let body = Expr::parse(&body)?;

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
                let body = Expr::parse(&body)?;
                (arg, body.clone(), Type::Func(None, Mode::Pure))
            };
        if !is_identifier(arg) {
            return Err(Fault::Syntax);
        }
        Ok(Func::UserDefined(
            arg.to_string(),
            Box::new(body),
            annotation,
        ))
    }

    pub fn autobind_effect(&self, engine: &mut Engine) -> Self {
        if let Func::UserDefined(arg, body, Type::Func(anno, _)) = self {
            let expr_mode = body
                .is_pure(engine)
                .then(|| Mode::Pure)
                .unwrap_or(Mode::Effect);
            Func::UserDefined(
                arg.clone(),
                body.clone(),
                Type::Func(anno.clone(), expr_mode),
            )
        } else {
            self.clone()
        }
    }

    pub fn bind_type(&self, anno: Type, engine: &mut Engine) -> Result<Self, Fault> {
        match self {
            Func::UserDefined(arg, body, _) => {
                if let Type::Func(Some(inner), ano_mode) = anno.clone() {
                    Ok(Func::UserDefined(
                        arg.to_owned(),
                        if let Expr::Value(Value::Func(func)) = *body.clone() {
                            Box::new(Expr::Value(Value::Func(
                                func.bind_type(inner.1.clone(), engine)?,
                            )))
                        } else {
                            body.clone()
                        },
                        Type::Func(Some(Box::new((inner.0, inner.1))), ano_mode),
                    ))
                } else if let Type::Func(None, ano_mode) = anno.clone() {
                    Ok(Func::UserDefined(
                        arg.to_owned(),
                        body.clone(),
                        Type::Func(None, ano_mode),
                    ))
                } else {
                    Ok(self.clone())
                }
            }
            _ => Err(Fault::General(Some(format!(
                "builtin functions are can't be type bound"
            )))),
        }
    }

    pub fn apply(&self, rhs: Expr, engine: &mut Engine) -> Result<Value, Fault> {
        Ok(match self {
            Func::BuiltIn(func_obj, Type::Func(_, func_mode)) => {
                // Check effect
                if let (Mode::Effect, Mode::Pure) = (func_mode, engine.mode) {
                    return Err(Fault::Pure(Value::Func(self.clone()).to_string()));
                };
                func_obj(rhs.eval(engine)?, engine)?
            }
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
                if let (Mode::Effect, Mode::Pure) = (func_mode, engine.mode) {
                    return Err(Fault::Pure(Value::Func(self.clone()).to_string()));
                };

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
            Func::UserDefined(_, _, other_type) | Func::BuiltIn(_, other_type) => {
                return Err(Fault::Type(Value::Func(self.clone()), other_type.clone()))
            }
        })
    }

    pub fn infer(&self, engine: &Engine, arg_type: &Value) -> Result<Type, Fault> {
        match self {
            Func::UserDefined(arg_name, body, _) => Ok(Type::Func(
                Some(Box::new((
                    arg_type.type_of(),
                    ok!(body
                        .replace(
                            &Expr::Refer(arg_name.clone()),
                            &Expr::Value(arg_type.clone())
                        )
                        .infer(&engine))?,
                ))),
                engine.mode,
            )),
            _ => Err(Fault::General(Some(format!(
                "builtin functions are can't type inference"
            )))),
        }
    }
}
