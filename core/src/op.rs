use crate::*;

#[derive(Debug, Clone)]
pub enum Op {
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

impl Node for Op {
    fn eval(&self, engine: &mut Engine) -> Result<Value, Fault> {
        Ok(match self {
            Op::Add(lhs, rhs) => {
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
            Op::Sub(lhs, rhs) => {
                let lhs = lhs.eval(engine)?;
                let rhs = rhs.eval(engine)?;
                if let (Value::Num(lhs), Value::Num(rhs)) = (&lhs, &rhs) {
                    Value::Num(lhs - rhs)
                } else {
                    lhs.modify_inside(&rhs, &None, engine)?
                }
            }
            Op::Mul(lhs, rhs) => {
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
            Op::Div(lhs, rhs) => {
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
            Op::Mod(lhs, rhs) => {
                Value::Num(lhs.eval(engine)?.get_number()? % rhs.eval(engine)?.get_number()?)
            }
            Op::Pow(lhs, rhs) => Value::Num(
                lhs.eval(engine)?
                    .get_number()?
                    .powf(rhs.eval(engine)?.get_number()?),
            ),
            Op::Equal(lhs, rhs) => {
                let lhs = lhs.eval(engine)?;
                let rhs = rhs.eval(engine)?;
                if rhs.is_match(&lhs) {
                    rhs
                } else {
                    return Err(Fault::Logic(self.clone()));
                }
            }
            Op::NotEq(lhs, rhs) => {
                let lhs = lhs.eval(engine)?;
                let rhs = rhs.eval(engine)?;
                if !rhs.is_match(&lhs) {
                    rhs
                } else {
                    return Err(Fault::Logic(self.clone()));
                }
            }
            Op::LessThan(lhs, rhs) => {
                let rhs = rhs.eval(engine)?;
                if lhs.eval(engine)?.get_number()? < rhs.get_number()? {
                    rhs
                } else {
                    return Err(Fault::Logic(self.clone()));
                }
            }
            Op::LessThanEq(lhs, rhs) => {
                let rhs = rhs.eval(engine)?;
                if lhs.eval(engine)?.get_number()? <= rhs.get_number()? {
                    rhs
                } else {
                    return Err(Fault::Logic(self.clone()));
                }
            }
            Op::GreaterThan(lhs, rhs) => {
                let rhs = rhs.eval(engine)?;
                if lhs.eval(engine)?.get_number()? > rhs.get_number()? {
                    rhs
                } else {
                    return Err(Fault::Logic(self.clone()));
                }
            }
            Op::GreaterThanEq(lhs, rhs) => {
                let rhs = rhs.eval(engine)?;
                if lhs.eval(engine)?.get_number()? >= rhs.get_number()? {
                    rhs
                } else {
                    return Err(Fault::Logic(self.clone()));
                }
            }
            Op::And(lhs, rhs) => {
                let rhs = rhs.eval(engine);
                if lhs.eval(engine).is_ok() && rhs.is_ok() {
                    rhs?
                } else {
                    return Err(Fault::Logic(self.clone()));
                }
            }
            Op::Or(lhs, rhs) => {
                let lhs = lhs.eval(engine);
                let rhs = rhs.eval(engine);
                if let (Ok(result), _) | (_, Ok(result)) = (lhs, rhs) {
                    result
                } else {
                    return Err(Fault::Logic(self.clone()));
                }
            }
            Op::Not(val) => {
                let val = val.eval(engine);
                if val.is_ok() {
                    return Err(Fault::Logic(self.clone()));
                } else {
                    Value::Null
                }
            }
            Op::Access(lhs, rhs) => {
                let lhs = lhs.eval(engine)?;
                let rhs = rhs.eval(engine)?;
                let err = Err(Fault::Infix(self.clone()));
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
            Op::As(lhs, rhs) => {
                let lhs = lhs.eval(engine)?;
                let rhs = rhs.eval(engine)?;
                lhs.cast(&rhs.get_type()?)?
            }
            Op::Apply(lhs, is_lazy, rhs) => {
                let func = lhs.eval(engine)?;
                if let Value::Func(obj) = func.clone() {
                    match obj {
                        Func::BuiltIn(func) => func(rhs.eval(engine)?, engine)?,
                        Func::UserDefined(argument, code) => {
                            let code = code
                                .replace(
                                    &Expr::Refer(argument),
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
                } else if let (Value::Dict(obj), Expr::Refer(method)) = (&func, rhs) {
                    let obj = Expr::Value(Value::Dict(obj.clone()));
                    Op::Apply(
                        Expr::Infix(Box::new(Op::Access(
                            obj.clone(),
                            Expr::Value(Value::Str(method.to_owned())),
                        ))),
                        false,
                        obj,
                    )
                    .eval(engine)?
                } else {
                    return Err(Fault::Apply(func));
                }
            }
            Op::PipeLine(lhs, rhs) => {
                Op::Apply(rhs.to_owned(), false, lhs.to_owned()).eval(engine)?
            }
            Op::Assign(lhs, rhs) => {
                Stmt::Let(lhs.to_owned(), rhs.to_owned(), false).eval(engine)?
            }
            Op::AssignAdd(name, rhs) => Op::Assign(
                name.to_owned(),
                Expr::Infix(Box::new(Op::Add(name.to_owned(), rhs.clone()))),
            )
            .eval(engine)?,
            Op::AssignSub(name, rhs) => Op::Assign(
                name.to_owned(),
                Expr::Infix(Box::new(Op::Sub(name.to_owned(), rhs.clone()))),
            )
            .eval(engine)?,
            Op::AssignMul(name, rhs) => Op::Assign(
                name.to_owned(),
                Expr::Infix(Box::new(Op::Mul(name.to_owned(), rhs.clone()))),
            )
            .eval(engine)?,
            Op::AssignDiv(name, rhs) => Op::Assign(
                name.to_owned(),
                Expr::Infix(Box::new(Op::Div(name.to_owned(), rhs.clone()))),
            )
            .eval(engine)?,
            Op::AssignMod(name, rhs) => Op::Assign(
                name.to_owned(),
                Expr::Infix(Box::new(Op::Mod(name.to_owned(), rhs.clone()))),
            )
            .eval(engine)?,
            Op::AssignPow(name, rhs) => Op::Assign(
                name.to_owned(),
                Expr::Infix(Box::new(Op::Pow(name.to_owned(), rhs.clone()))),
            )
            .eval(engine)?,
            Op::To(from, to) => {
                let from = from.eval(engine)?.get_number()? as usize;
                let to = to.eval(engine)?.get_number()? as usize;
                if from < to {
                    Value::Range(from, to)
                } else {
                    return Err(Fault::Range);
                }
            }
        })
    }

    fn parse(source: &str) -> Result<Self, Fault> {
        let token_list: Vec<String> = tokenize(source, SPACE.as_ref(), true)?;
        let token = Expr::parse(ok!(token_list.last())?)?;
        let operator = ok!(token_list.get(ok!(token_list.len().checked_sub(2))?))?;
        let has_lhs =
            |len: usize| Expr::parse(&join!(ok!(token_list.get(..token_list.len() - len))?));
        Ok(match operator.as_str() {
            "+" => Op::Add(has_lhs(2)?, token),
            "*" => Op::Mul(has_lhs(2)?, token),
            "/" => Op::Div(has_lhs(2)?, token),
            "%" => Op::Mod(has_lhs(2)?, token),
            "^" => Op::Pow(has_lhs(2)?, token),
            "==" => Op::Equal(has_lhs(2)?, token),
            "!=" => Op::NotEq(has_lhs(2)?, token),
            "<" => Op::LessThan(has_lhs(2)?, token),
            "<=" => Op::LessThanEq(has_lhs(2)?, token),
            ">" => Op::GreaterThan(has_lhs(2)?, token),
            ">=" => Op::GreaterThanEq(has_lhs(2)?, token),
            "&&" => Op::And(has_lhs(2)?, token),
            "||" => Op::Or(has_lhs(2)?, token),
            "?" => Op::Apply(has_lhs(2)?, true, token),
            "::" => Op::Access(has_lhs(2)?, token),
            "as" => Op::As(has_lhs(2)?, token),
            "|>" => Op::PipeLine(has_lhs(2)?, token),
            ":=" => Op::Assign(has_lhs(2)?, token),
            "+=" => Op::AssignAdd(has_lhs(2)?, token),
            "-=" => Op::AssignSub(has_lhs(2)?, token),
            "*=" => Op::AssignMul(has_lhs(2)?, token),
            "/=" => Op::AssignDiv(has_lhs(2)?, token),
            "%=" => Op::AssignMod(has_lhs(2)?, token),
            "^=" => Op::AssignPow(has_lhs(2)?, token),
            "~" => Op::To(has_lhs(2)?, token),
            "-" => {
                if let Ok(lhs) = has_lhs(2) {
                    Op::Sub(lhs, token)
                } else if let Ok(Expr::Infix(infix)) = Expr::parse(&format!(
                    "{} (0 - {})",
                    &join!(ok!(token_list.get(..token_list.len() - 2))?),
                    token
                )) {
                    *infix
                } else {
                    Op::Sub(Expr::Value(Value::Num(0.0)), token)
                }
            }
            "!" => {
                let lhs = join!(ok!(token_list.get(..token_list.len() - 2))?)
                    .trim()
                    .to_string();
                if lhs.is_empty() {
                    Op::Not(token)
                } else {
                    let Ok(Expr::Infix(infix)) = Expr::parse(&format!("{lhs} (!{})", token)) else {
                        return Err(Fault::Syntax);
                    };
                    *infix
                }
            }
            operator => {
                if operator.starts_with("`") && operator.ends_with("`") {
                    let operator = operator[1..operator.len() - 1].to_string();
                    Op::Apply(
                        Expr::Infix(Box::new(Op::Apply(
                            Expr::parse(&operator)?,
                            false,
                            has_lhs(2)?,
                        ))),
                        false,
                        token,
                    )
                } else {
                    Op::Apply(has_lhs(1)?, false, token)
                }
            }
        })
    }

    fn replace(&self, from: &Expr, to: &Expr) -> Self {
        match self {
            Op::Add(lhs, rhs) => Op::Add(lhs.replace(from, to), rhs.replace(from, to)),
            Op::Sub(lhs, rhs) => Op::Sub(lhs.replace(from, to), rhs.replace(from, to)),
            Op::Mul(lhs, rhs) => Op::Mul(lhs.replace(from, to), rhs.replace(from, to)),
            Op::Div(lhs, rhs) => Op::Div(lhs.replace(from, to), rhs.replace(from, to)),
            Op::Mod(lhs, rhs) => Op::Mod(lhs.replace(from, to), rhs.replace(from, to)),
            Op::Pow(lhs, rhs) => Op::Pow(lhs.replace(from, to), rhs.replace(from, to)),
            Op::Equal(lhs, rhs) => Op::Equal(lhs.replace(from, to), rhs.replace(from, to)),
            Op::NotEq(lhs, rhs) => Op::NotEq(lhs.replace(from, to), rhs.replace(from, to)),
            Op::LessThan(lhs, rhs) => Op::LessThan(lhs.replace(from, to), rhs.replace(from, to)),
            Op::LessThanEq(lhs, rhs) => {
                Op::LessThanEq(lhs.replace(from, to), rhs.replace(from, to))
            }
            Op::GreaterThan(lhs, rhs) => {
                Op::GreaterThan(lhs.replace(from, to), rhs.replace(from, to))
            }
            Op::GreaterThanEq(lhs, rhs) => {
                Op::GreaterThanEq(lhs.replace(from, to), rhs.replace(from, to))
            }
            Op::And(lhs, rhs) => Op::And(lhs.replace(from, to), rhs.replace(from, to)),
            Op::Or(lhs, rhs) => Op::Or(lhs.replace(from, to), rhs.replace(from, to)),
            Op::Not(val) => Op::Not(val.replace(from, to)),
            Op::Access(lhs, rhs) => Op::Access(lhs.replace(from, to), rhs.replace(from, to)),
            Op::As(lhs, rhs) => Op::As(lhs.replace(from, to), rhs.replace(from, to)),
            Op::Apply(lhs, is_lazy, rhs) => {
                Op::Apply(lhs.replace(from, to), *is_lazy, rhs.replace(from, to))
            }
            Op::Assign(lhs, rhs) => Op::Assign(lhs.replace(from, to), rhs.replace(from, to)),
            Op::PipeLine(lhs, rhs) => Op::PipeLine(lhs.replace(from, to), rhs.replace(from, to)),
            Op::AssignAdd(lhs, rhs) => Op::AssignAdd(lhs.replace(from, to), rhs.replace(from, to)),
            Op::AssignSub(lhs, rhs) => Op::AssignSub(lhs.replace(from, to), rhs.replace(from, to)),
            Op::AssignMul(lhs, rhs) => Op::AssignMul(lhs.replace(from, to), rhs.replace(from, to)),
            Op::AssignDiv(lhs, rhs) => Op::AssignDiv(lhs.replace(from, to), rhs.replace(from, to)),
            Op::AssignMod(lhs, rhs) => Op::AssignMod(lhs.replace(from, to), rhs.replace(from, to)),
            Op::AssignPow(lhs, rhs) => Op::AssignPow(lhs.replace(from, to), rhs.replace(from, to)),
            Op::To(lhs, rhs) => Op::To(lhs.replace(from, to), rhs.replace(from, to)),
        }
    }

    fn is_pure(&self, engine: &Engine) -> bool {
        match self {
            Op::Add(lhs, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
            Op::Sub(lhs, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
            Op::Mul(lhs, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
            Op::Div(lhs, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
            Op::Mod(lhs, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
            Op::Pow(lhs, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
            Op::Equal(lhs, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
            Op::NotEq(lhs, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
            Op::LessThan(lhs, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
            Op::LessThanEq(lhs, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
            Op::GreaterThan(lhs, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
            Op::GreaterThanEq(lhs, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
            Op::And(lhs, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
            Op::Or(lhs, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
            Op::Not(val) => val.is_pure(engine),
            Op::Access(lhs, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
            Op::As(lhs, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
            Op::Apply(lhs, _, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
            Op::Assign(lhs, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
            Op::PipeLine(lhs, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
            Op::AssignAdd(lhs, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
            Op::AssignSub(lhs, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
            Op::AssignMul(lhs, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
            Op::AssignDiv(lhs, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
            Op::AssignMod(lhs, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
            Op::AssignPow(lhs, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
            Op::To(lhs, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
        }
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Op::Add(lhs, rhs) => format!("{lhs} + {rhs}"),
                Op::Sub(Expr::Value(Value::Num(0.0)), rhs) => format!("-{rhs}"),
                Op::Sub(lhs, rhs) => format!("{lhs} - {rhs}"),
                Op::Mul(lhs, rhs) => format!("{lhs} * {rhs}"),
                Op::Div(lhs, rhs) => format!("{lhs} / {rhs}"),
                Op::Mod(lhs, rhs) => format!("{lhs} % {rhs}"),
                Op::Pow(lhs, rhs) => format!("{lhs} ^ {rhs}"),
                Op::Equal(lhs, rhs) => format!("{lhs} == {rhs}"),
                Op::NotEq(lhs, rhs) => format!("{lhs} != {rhs}"),
                Op::LessThan(lhs, rhs) => format!("{lhs} < {rhs}"),
                Op::LessThanEq(lhs, rhs) => format!("{lhs} <= {rhs}"),
                Op::GreaterThan(lhs, rhs) => format!("{lhs} > {rhs}"),
                Op::GreaterThanEq(lhs, rhs) => format!("{lhs} >= {rhs}"),
                Op::And(lhs, rhs) => format!("{lhs} && {rhs}"),
                Op::Or(lhs, rhs) => format!("{lhs} || {rhs}"),
                Op::Not(val) => format!("!{val}"),
                Op::Access(lhs, rhs) => format!("{lhs} :: {rhs}"),
                Op::As(lhs, rhs) => format!("{lhs} as {rhs}",),
                Op::Assign(lhs, rhs) => format!("{lhs} := {rhs}",),
                Op::PipeLine(lhs, rhs) => format!("{lhs} |> {rhs}"),
                Op::Apply(lhs, true, rhs) => format!("{lhs} ? {rhs}"),
                Op::Apply(lhs, false, rhs) => format!("{lhs} {rhs}"),
                Op::AssignAdd(lhs, rhs) => format!("{lhs} += {rhs}"),
                Op::AssignSub(lhs, rhs) => format!("{lhs} -= {rhs}"),
                Op::AssignMul(lhs, rhs) => format!("{lhs} *= {rhs}"),
                Op::AssignDiv(lhs, rhs) => format!("{lhs} /= {rhs}"),
                Op::AssignMod(lhs, rhs) => format!("{lhs} %= {rhs}"),
                Op::AssignPow(lhs, rhs) => format!("{lhs} ^= {rhs}"),
                Op::To(lhs, rhs) => format!("{lhs} ~ {rhs}",),
            }
        )
    }
}
