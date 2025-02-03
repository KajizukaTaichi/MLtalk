use crate::*;

#[derive(Debug, Clone)]
pub enum Statement {
    Print(Vec<Expr>),
    Let(Expr, bool, Expr),
    If(Box<Statement>, Expr, Option<Expr>),
    For(Expr, Expr, Expr),
    While(Box<Statement>, Expr),
    Fault(Option<Expr>),
    Expr(Expr),
}

impl Statement {
    pub fn eval(&self, engine: &mut Engine) -> Result<Value, Fault> {
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
                        return Err(Fault::Let(Expr::List(list.to_owned())));
                    }
                } else if let Expr::Dict(dict) = name {
                    let val = expr.eval(engine)?;
                    let val = val.get_dict()?;
                    for (key, name) in dict {
                        let val = ok!(
                            val.get(key),
                            Fault::Key(Value::Str(key.to_owned()), Value::Dict(val.clone()))
                        )?;
                        let val = Expr::Value(val.clone());
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
                            return Err(Fault::Let(Expr::Value(name)));
                        }
                        val
                    }
                } else {
                    let name = name.eval(engine)?;
                    let val = expr.eval(engine)?;
                    if name != val {
                        return Err(Fault::Let(Expr::Value(name)));
                    }
                    val
                }
            }
            Statement::If(expr, then, r#else) => {
                if expr.eval(engine).is_ok() {
                    then.eval(engine)?
                } else if let Some(r#else) = r#else {
                    r#else.clone().eval(engine)?
                } else {
                    Value::Null
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

    pub fn parse(code: &str) -> Result<Statement, Fault> {
        let code = code.trim();
        if let Some(code) = code.strip_prefix("print") {
            let mut exprs = vec![];
            for i in tokenize(code, &[","])? {
                exprs.push(Expr::parse(&i).unwrap_or(Expr::Block(Block::parse(&i)?)));
            }
            Ok(Statement::Print(exprs))
        } else if let (_, Some(codes)) | (Some(codes), _) =
            (code.strip_prefix("let"), code.strip_prefix("const"))
        {
            let splited = tokenize(codes, &["="])?;
            let (name, codes) = (ok!(splited.first())?, join!(ok!(splited.get(1..))?, "="));
            Ok(Statement::Let(
                Expr::parse(name)?,
                code.starts_with("const"),
                Expr::parse(&codes).unwrap_or(Expr::Block(Block::parse(&codes)?)),
            ))
        } else if let Some(code) = code.strip_prefix("if") {
            let code = tokenize(code, SPACE.as_ref())?;
            let pos_then = ok!(code.iter().position(|i| i == "then"))?;
            if let Some(pos_else) = code.iter().position(|i| i == "else") {
                let cond_section = join!(ok!(code.get(0..pos_then))?);
                let then_section = join!(ok!(code.get(pos_then + 1..pos_else))?);
                let else_section = join!(ok!(code.get(pos_else + 1..))?);
                Ok(Statement::If(
                    Box::new(Statement::parse(&cond_section)?),
                    Expr::parse(&then_section).unwrap_or(Expr::Block(Block::parse(&then_section)?)),
                    Some(
                        Expr::parse(&else_section)
                            .unwrap_or(Expr::Block(Block::parse(&else_section)?)),
                    ),
                ))
            } else {
                let cond_section = join!(ok!(code.get(0..pos_then))?);
                let then_section = join!(ok!(code.get(pos_then + 1..))?);
                Ok(Statement::If(
                    Box::new(Statement::parse(&cond_section)?),
                    Expr::parse(&then_section).unwrap_or(Expr::Block(Block::parse(&then_section)?)),
                    None,
                ))
            }
        } else if let Some(code) = code.strip_prefix("for") {
            let code = tokenize(code, SPACE.as_ref())?;
            let pos_in = ok!(code.iter().position(|i| i == "in"))?;
            let pos_do = ok!(code.iter().position(|i| i == "do"))?;
            let counter_section = join!(ok!(code.get(0..pos_in))?);
            let iter_section = join!(ok!(code.get(pos_in + 1..pos_do))?);
            let body_section = join!(ok!(code.get(pos_do + 1..))?);
            Ok(Statement::For(
                Expr::parse(&counter_section)?,
                Expr::parse(&iter_section).unwrap_or(Expr::Block(Block::parse(&iter_section)?)),
                Expr::parse(&body_section).unwrap_or(Expr::Block(Block::parse(&body_section)?)),
            ))
        } else if let Some(code) = code.strip_prefix("while") {
            let code = tokenize(code, SPACE.as_ref())?;
            let pos_loop = ok!(code.iter().position(|i| i == "loop"))?;
            let cond_section = join!(ok!(code.get(0..pos_loop))?);
            let body_section = join!(ok!(code.get(pos_loop + 1..))?);
            Ok(Statement::While(
                Box::new(Statement::parse(&cond_section)?),
                Expr::parse(&body_section).unwrap_or(Expr::Block(Block::parse(&body_section)?)),
            ))
        } else if let Some(code) = code.strip_prefix("fault") {
            Ok(Statement::Fault(some!(Expr::parse(code))))
        } else {
            Ok(Statement::Expr(Expr::parse(code)?))
        }
    }

    pub fn replace(&self, from: &Expr, to: &Expr) -> Self {
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
