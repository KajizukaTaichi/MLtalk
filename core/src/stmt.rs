use crate::*;

#[derive(Debug, Clone)]
pub enum Stmt {
    Print(Vec<Expr>),
    Let(Expr, bool, Expr),
    If(Box<Stmt>, Expr, Option<Expr>),
    For(Expr, Expr, Expr),
    While(Box<Stmt>, Expr),
    Fault(Option<Expr>),
    Expr(Expr),
}

impl Stmt {
    pub fn eval(&self, engine: &mut Engine) -> Result<Value, Fault> {
        Ok(match self {
            Stmt::Print(expr) => {
                for i in expr {
                    print!("{}", i.eval(engine)?.cast(&Type::Str)?.get_str()?);
                }
                io::stdout().flush().unwrap();
                Value::Null
            }
            Stmt::Let(name, is_protect, expr) => {
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
                            Stmt::Let(name.to_owned(), *is_protect, Expr::Value(val.clone()))
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
                        Stmt::Let(name.to_owned(), *is_protect, val).eval(engine)?;
                    }
                    Value::Dict(val)
                } else if let Expr::Infix(infix) = name {
                    let infix = *infix.clone();
                    if let Op::Access(accessor, key) = infix {
                        let val = expr.eval(engine)?;
                        let obj = accessor.eval(engine)?;
                        let key = key.eval(engine)?;
                        let updated_obj = obj.modify_inside(&key, &Some(val.clone()), engine)?;
                        Stmt::Let(accessor, *is_protect, Expr::Value(updated_obj.clone()))
                            .eval(engine)?
                    } else if let Op::As(name, sig) = infix {
                        let val = expr.eval(engine)?;
                        let sig = sig.eval(engine)?.get_type()?;
                        if val.type_of() != sig {
                            return Err(Fault::Value(val, sig));
                        }
                        Stmt::Let(name, *is_protect, Expr::Value(val.clone())).eval(engine)?
                    } else if let Op::Apply(name, false, arg) = infix {
                        return Stmt::Let(
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
            Stmt::If(expr, then, r#else) => {
                if expr.eval(engine).is_ok() {
                    then.eval(engine)?
                } else if let Some(r#else) = r#else {
                    r#else.clone().eval(engine)?
                } else {
                    Value::Null
                }
            }
            Stmt::For(counter, expr, code) => {
                let mut result = Value::Null;
                for i in expr.eval(engine)?.cast(&Type::List)?.get_list()? {
                    Stmt::Let(counter.clone(), false, Expr::Value(i)).eval(engine)?;
                    result = code.eval(engine)?;
                }
                result
            }
            Stmt::While(expr, code) => {
                let mut result = Value::Null;
                while expr.eval(engine).is_ok() {
                    result = code.eval(engine)?;
                }
                result
            }
            Stmt::Fault(Some(msg)) => {
                return Err(Fault::General(Some(msg.eval(engine)?.get_str()?)))
            }
            Stmt::Fault(None) => return Err(Fault::General(None)),
            Stmt::Expr(expr) => expr.eval(engine)?,
        })
    }

    pub fn parse(code: &str) -> Result<Stmt, Fault> {
        let code = code.trim();
        if let Some(code) = code.strip_prefix("print") {
            let mut exprs = vec![];
            for i in tokenize(code, &[","])? {
                exprs.push(Expr::parse(&i).unwrap_or(Expr::Block(Block::parse(&i)?)));
            }
            Ok(Stmt::Print(exprs))
        } else if let (_, Some(codes)) | (Some(codes), _) =
            (code.strip_prefix("let"), code.strip_prefix("const"))
        {
            let splited = tokenize(codes, &["="])?;
            let (name, codes) = (ok!(splited.first())?, join!(ok!(splited.get(1..))?, "="));
            Ok(Stmt::Let(
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
                Ok(Stmt::If(
                    Box::new(Stmt::parse(&cond_section)?),
                    Expr::parse(&then_section).unwrap_or(Expr::Block(Block::parse(&then_section)?)),
                    Some(
                        Expr::parse(&else_section)
                            .unwrap_or(Expr::Block(Block::parse(&else_section)?)),
                    ),
                ))
            } else {
                let cond_section = join!(ok!(code.get(0..pos_then))?);
                let then_section = join!(ok!(code.get(pos_then + 1..))?);
                Ok(Stmt::If(
                    Box::new(Stmt::parse(&cond_section)?),
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
            Ok(Stmt::For(
                Expr::parse(&counter_section)?,
                Expr::parse(&iter_section).unwrap_or(Expr::Block(Block::parse(&iter_section)?)),
                Expr::parse(&body_section).unwrap_or(Expr::Block(Block::parse(&body_section)?)),
            ))
        } else if let Some(code) = code.strip_prefix("while") {
            let code = tokenize(code, SPACE.as_ref())?;
            let pos_loop = ok!(code.iter().position(|i| i == "loop"))?;
            let cond_section = join!(ok!(code.get(0..pos_loop))?);
            let body_section = join!(ok!(code.get(pos_loop + 1..))?);
            Ok(Stmt::While(
                Box::new(Stmt::parse(&cond_section)?),
                Expr::parse(&body_section).unwrap_or(Expr::Block(Block::parse(&body_section)?)),
            ))
        } else if let Some(code) = code.strip_prefix("fault") {
            Ok(Stmt::Fault(some!(Expr::parse(code))))
        } else {
            Ok(Stmt::Expr(Expr::parse(code)?))
        }
    }

    pub fn replace(&self, from: &Expr, to: &Expr) -> Self {
        match self {
            Stmt::Print(vals) => Stmt::Print(vals.iter().map(|j| j.replace(from, to)).collect()),
            Stmt::Let(name, is_protect, val) => {
                Stmt::Let(name.clone(), *is_protect, val.replace(from, to))
            }
            Stmt::If(cond, then, r#else) => Stmt::If(
                Box::new(cond.replace(from, to)),
                then.replace(from, to),
                r#else.clone().map(|j| j.replace(from, to)),
            ),
            Stmt::For(counter, iter, code) => Stmt::For(
                counter.clone(),
                iter.replace(from, to),
                code.replace(from, to),
            ),
            Stmt::While(cond, code) => {
                Stmt::While(Box::new(cond.replace(from, to)), code.replace(from, to))
            }
            Stmt::Fault(Some(msg)) => Stmt::Fault(Some(msg.replace(from, to))),
            Stmt::Fault(None) => Stmt::Fault(None),
            Stmt::Expr(val) => Stmt::Expr(val.replace(from, to)),
        }
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Stmt::Print(exprs) => format!(
                    "print {}",
                    exprs
                        .iter()
                        .map(|i| format!("{i}"))
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
                Stmt::Let(name, false, val) => format!("let {name} = {val}"),
                Stmt::Let(name, true, val) => format!("const {name} = {val}"),
                Stmt::If(cond, then, r#else) =>
                    if let Some(r#else) = r#else {
                        format!("if {cond} then {then} else {}", r#else)
                    } else {
                        format!("if {cond} then {then}")
                    },
                Stmt::For(counter, iterator, code) => {
                    format!("for {counter} in {iterator} do {code}")
                }
                Stmt::While(cond, code) => {
                    format!("while {cond} loop {code}")
                }
                Stmt::Fault(Some(msg)) => format!("fault {msg}"),
                Stmt::Fault(None) => "fault".to_string(),
                Stmt::Expr(expr) => format!("{expr}"),
            }
        )
    }
}
