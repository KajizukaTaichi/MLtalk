use crate::*;

#[derive(Debug, Clone)]
pub enum Stmt {
    Let(Expr, Expr),
    If(Box<Stmt>, Expr, Option<Box<Stmt>>),
    For(Expr, Expr, Expr),
    While(Box<Stmt>, Expr),
    Fault(Option<Expr>),
    Effect(Box<Stmt>),
    Bind(Expr, Type),
    Lazy(Box<Stmt>),
    Expr(Expr),
}

impl Node for Stmt {
    fn eval(&self, engine: &mut Engine) -> Result<Value, Fault> {
        if let (Mode::Pure, false, false) = (engine.mode, self.is_pure(engine), engine.is_toplevel)
        {
            return Err(Fault::Pure(self.to_string()));
        }

        Ok(match self {
            Stmt::Let(name, expr) => {
                if let Expr::Refer(name) = name {
                    if let (Mode::Pure, false) = (engine.mode, expr.is_pure(engine)) {
                        return Err(Fault::Pure(expr.to_string()));
                    }
                    let val = if engine.is_lazy {
                        Value::Func(Func::UserDefined(
                            "_".to_string(),
                            Box::new(expr.clone()),
                            Type::Func(None, engine.mode),
                        ))
                    } else {
                        expr.eval(engine)?
                    };
                    engine.alloc(name, &val)?;

                    if let Mode::Effect = engine.mode {
                        engine.set_effect(name);
                    } else {
                        engine.unset_effect(name);
                    }
                    val
                } else if let Expr::List(list) = name {
                    let val = expr.eval(engine)?;
                    let val = val.get_list()?;
                    if list.len() == list.len() {
                        for (name, val) in list.iter().zip(&val) {
                            Stmt::Let(name.to_owned(), Expr::Value(val.clone())).eval(engine)?;
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
                        Stmt::Let(name.to_owned(), val).eval(engine)?;
                    }
                    Value::Dict(val)
                } else if let Expr::Infix(infix) = name {
                    let infix = *infix.clone();
                    if let Op::Access(accessor, key) = infix {
                        let val = expr.eval(engine)?;
                        let obj = accessor.eval(engine)?;
                        let key = key.eval(engine)?;
                        let updated_obj = obj.modify_inside(&key, &Some(val.clone()), engine)?;
                        Stmt::Let(accessor, Expr::Value(updated_obj.clone())).eval(engine)?
                    } else if let Op::As(name, sig) = infix {
                        let val = expr.eval(engine)?;
                        let sig = sig.eval(engine)?.get_type()?;
                        if sig != val.type_of() {
                            return Err(Fault::Type(val, sig));
                        }
                        Stmt::Let(name, Expr::Value(val.clone())).eval(engine)?
                    } else if let Op::Call(name, arg) = infix {
                        return Stmt::Let(
                            name,
                            Expr::Value(Value::Func(Func::UserDefined(
                                arg.to_string(),
                                Box::new(expr.to_owned()),
                                Type::Func(None, engine.mode),
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
                for i in expr.eval(engine)?.cast(&Type::List(None))?.get_list()? {
                    Stmt::Let(counter.clone(), Expr::Value(i)).eval(engine)?;
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
            Stmt::Effect(expr) => {
                let old_mode = engine.mode;
                engine.mode = Mode::Effect;
                let result = expr.eval(engine);
                engine.mode = old_mode;
                result?
            }
            Stmt::Lazy(expr) => {
                let old_is_lazy = engine.is_lazy;
                engine.is_lazy = true;
                let result = expr.eval(engine);
                engine.is_lazy = old_is_lazy;
                result?
            }
            Stmt::Bind(expr, anno) => {
                let val = expr.eval(engine)?;
                let Value::Func(func) = val else {
                    return Err(Fault::Type(val, Type::Func(None, engine.mode)));
                };
                Stmt::Let(
                    expr.clone(),
                    Expr::Value(Value::Func(func.bind(anno.clone())?)),
                )
                .eval(engine)?
            }
            Stmt::Expr(expr) => expr.eval(engine)?,
        })
    }

    fn parse(code: &str) -> Result<Stmt, Fault> {
        let code = code.trim();
        if let Some(codes) = code.strip_prefix("let") {
            let splited = tokenize(codes, &["="], false)?;
            let (name, codes) = (ok!(splited.first())?, join!(ok!(splited.get(1..))?, "="));
            Ok(Stmt::Let(
                Expr::parse(name)?,
                Expr::parse(&codes).unwrap_or(Expr::Block(Block::parse(&codes)?)),
            ))
        } else if let Some(code) = code.strip_prefix("if") {
            let code = tokenize(code, SPACE.as_ref(), false)?;
            let pos_then = ok!(code.iter().position(|i| i == "then"))?;
            if let Some(pos_else) = code.iter().position(|i| i == "else") {
                let cond_section = join!(ok!(code.get(0..pos_then))?);
                let then_section = join!(ok!(code.get(pos_then + 1..pos_else))?);
                let else_section = join!(ok!(code.get(pos_else + 1..))?);
                Ok(Stmt::If(
                    Box::new(Stmt::parse(&cond_section)?),
                    Expr::parse(&then_section).unwrap_or(Expr::Block(Block::parse(&then_section)?)),
                    Some(Box::new(Stmt::parse(&else_section)?)),
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
        } else if let Some(code) = code.strip_prefix("match") {
            let code = tokenize(code, SPACE.as_ref(), false)?;
            let pos_then = ok!(code.iter().position(|i| i == "with"))?;
            let cond_section = Expr::parse(&join!(ok!(code.get(0..pos_then))?))?;
            let pattern_section = join!(ok!(code.get(pos_then + 1..))?);

            let mut pattern_tree = None;
            for pattern_line in tokenize(&pattern_section, &[","], false)? {
                let tokens = tokenize(&pattern_line, &["=>"], false)?;
                let pattern_expr =
                    Stmt::Let(Expr::parse(ok!(tokens.first())?)?, cond_section.clone());
                pattern_tree = Some(ok!(nest_pattern_tree(
                    pattern_tree,
                    Stmt::If(
                        Box::new(pattern_expr),
                        Expr::parse(&join!(ok!(tokens.get(1..))?, "=>"))?,
                        None,
                    )
                ))?);
            }
            Ok(ok!(pattern_tree)?)
        } else if let Some(code) = code.strip_prefix("for") {
            let code = tokenize(code, SPACE.as_ref(), false)?;
            let pos_eq = ok!(code.iter().position(|i| i == "="))?;
            let pos_do = ok!(code.iter().position(|i| i == "do"))?;
            let counter_section = join!(ok!(code.get(0..pos_eq))?);
            let iter_section = join!(ok!(code.get(pos_eq + 1..pos_do))?);
            let body_section = join!(ok!(code.get(pos_do + 1..))?);
            Ok(Stmt::For(
                Expr::parse(&counter_section)?,
                Expr::parse(&iter_section).unwrap_or(Expr::Block(Block::parse(&iter_section)?)),
                Expr::parse(&body_section).unwrap_or(Expr::Block(Block::parse(&body_section)?)),
            ))
        } else if let Some(code) = code.strip_prefix("while") {
            let code = tokenize(code, SPACE.as_ref(), false)?;
            let pos_do = ok!(code.iter().position(|i| i == "do"))?;
            let cond_section = join!(ok!(code.get(0..pos_do))?);
            let body_section = join!(ok!(code.get(pos_do + 1..))?);
            Ok(Stmt::While(
                Box::new(Stmt::parse(&cond_section)?),
                Expr::parse(&body_section).unwrap_or(Expr::Block(Block::parse(&body_section)?)),
            ))
        } else if let Some(code) = code.strip_prefix("fault") {
            let code = code.trim();
            if code.is_empty() {
                Ok(Stmt::Fault(None))
            } else {
                Ok(Stmt::Fault(Some(Expr::parse(code)?)))
            }
        } else if let Some(code) = code.strip_prefix("effect") {
            Ok(Stmt::Effect(Box::new(Stmt::parse(code)?)))
        } else if let Some(code) = code.strip_prefix("lazy") {
            Ok(Stmt::Lazy(Box::new(Stmt::parse(code)?)))
        } else if let Some(codes) = code.strip_prefix("bind") {
            let splited = tokenize(codes, &["="], false)?;
            let (name, anno) = (ok!(splited.first())?, join!(ok!(splited.get(1..))?, "="));
            Ok(Stmt::Bind(Expr::parse(name)?, Type::parse(&anno)?))
        } else {
            Ok(Stmt::Expr(Expr::parse(code)?))
        }
    }

    fn replace(&self, from: &Expr, to: &Expr) -> Self {
        match self {
            Stmt::Let(name, val) => Stmt::Let(name.clone(), val.replace(from, to)),
            Stmt::If(cond, then, r#else) => Stmt::If(
                Box::new(cond.replace(from, to)),
                then.replace(from, to),
                r#else.clone().map(|j| Box::new(j.replace(from, to))),
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
            Stmt::Effect(val) => Stmt::Effect(Box::new(val.replace(from, to))),
            Stmt::Lazy(val) => Stmt::Effect(Box::new(val.replace(from, to))),
            Stmt::Bind(val, anno) => Stmt::Bind(val.replace(from, to), anno.clone()),
            Stmt::Expr(val) => Stmt::Expr(val.replace(from, to)),
        }
    }

    fn is_pure(&self, engine: &Engine) -> bool {
        match self {
            Stmt::Let(name, expr) => name.is_pure(engine) && expr.is_pure(engine),
            Stmt::If(expr, then, r#else) => {
                expr.is_pure(engine)
                    && then.is_pure(engine)
                    && r#else.clone().map(|i| i.is_pure(engine)).unwrap_or(true)
            }
            Stmt::For(counter, expr, code) => {
                counter.is_pure(engine) && expr.is_pure(engine) && code.is_pure(engine)
            }
            Stmt::While(expr, code) => expr.is_pure(engine) && code.is_pure(engine),
            Stmt::Fault(msg) => msg.clone().map(|i| i.is_pure(engine)).unwrap_or(true),
            Stmt::Effect(_) => false,
            Stmt::Bind(name, _) => name.is_pure(engine),
            Stmt::Lazy(expr) => expr.is_pure(engine),
            Stmt::Expr(expr) => expr.is_pure(engine),
        }
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Stmt::Let(name, val) => format!("let {name} = {val}"),
                Stmt::If(cond, then, r#else) =>
                    if let Some(r#else) = r#else {
                        format!("if {cond} then {then} else {}", r#else)
                    } else {
                        format!("if {cond} then {then}")
                    },
                Stmt::For(counter, iterator, code) => {
                    format!("for {counter} = {iterator} do {code}")
                }
                Stmt::While(cond, code) => {
                    format!("while {cond} do {code}")
                }
                Stmt::Fault(Some(msg)) => format!("fault {msg}"),
                Stmt::Fault(None) => "fault".to_string(),
                Stmt::Effect(expr) => format!("effect {expr}"),
                Stmt::Lazy(expr) => format!("lazy {expr}"),
                Stmt::Bind(expr, anno) => format!("bind {expr} = {anno}"),
                Stmt::Expr(expr) => format!("{expr}"),
            }
        )
    }
}

fn nest_pattern_tree(pt: Option<Stmt>, to_add: Stmt) -> Option<Stmt> {
    if let Some(Stmt::If(expr, then, None)) = pt {
        Some(Stmt::If(expr, then, Some(Box::new(to_add))))
    } else if let Some(Stmt::If(expr, then, Some(pt_else))) = pt {
        Some(Stmt::If(
            expr,
            then,
            Some(Box::new(nest_pattern_tree(Some(*pt_else), to_add)?)),
        ))
    } else {
        Some(to_add)
    }
}
