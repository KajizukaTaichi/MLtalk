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
    Less(Expr, Expr),
    LessEq(Expr, Expr),
    Greater(Expr, Expr),
    GreaterEq(Expr, Expr),
    And(Expr, Expr),
    Or(Expr, Expr),
    Not(Expr),
    Access(Expr, Expr),
    As(Expr, Expr),
    Call(Expr, Expr),
    PipeLine(Expr, Expr),
    Assign(Expr, Expr),
    To(Expr, Expr),
}

impl Node for Op {
    fn eval(&self, engine: &mut Engine) -> Result<Value, Fault> {
        if let (Mode::Pure, false) = (engine.mode, self.is_pure(engine)) {
            return Err(Fault::Pure(self.to_string()));
        }

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
                if rhs == lhs {
                    rhs
                } else {
                    return Err(Fault::Logic(self.clone()));
                }
            }
            Op::NotEq(lhs, rhs) => {
                let lhs = lhs.eval(engine)?;
                let rhs = rhs.eval(engine)?;
                if rhs != lhs {
                    rhs
                } else {
                    return Err(Fault::Logic(self.clone()));
                }
            }
            Op::Less(lhs, rhs) => {
                let rhs = rhs.eval(engine)?;
                if lhs.eval(engine)?.get_number()? < rhs.get_number()? {
                    rhs
                } else {
                    return Err(Fault::Logic(self.clone()));
                }
            }
            Op::LessEq(lhs, rhs) => {
                let rhs = rhs.eval(engine)?;
                if lhs.eval(engine)?.get_number()? <= rhs.get_number()? {
                    rhs
                } else {
                    return Err(Fault::Logic(self.clone()));
                }
            }
            Op::Greater(lhs, rhs) => {
                let rhs = rhs.eval(engine)?;
                if lhs.eval(engine)?.get_number()? > rhs.get_number()? {
                    rhs
                } else {
                    return Err(Fault::Logic(self.clone()));
                }
            }
            Op::GreaterEq(lhs, rhs) => {
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
                if let (_, Ok(result)) | (Ok(result), _) = (lhs, rhs) {
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
                let err = Err(Fault::Infix(Op::Access(
                    Expr::Value(lhs.clone()),
                    Expr::Value(rhs.clone()),
                )));
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
                    Value::Range(start, end) => match rhs.clone() {
                        Value::Num(num) => {
                            if start <= num as usize && (num as usize) < end {
                                Value::Num(num)
                            } else {
                                return Err(Fault::Logic(Op::Less(
                                    Expr::Infix(Box::new(Op::LessEq(
                                        Expr::Value(Value::Num(start as f64)),
                                        Expr::Value(Value::Num(num)),
                                    ))),
                                    Expr::Value(Value::Num(end as f64)),
                                )));
                            }
                        }
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
            Op::Call(lhs, rhs) => {
                let func = lhs.eval(engine)?;
                if let Value::Func(obj) = func.clone() {
                    obj.apply(rhs.clone(), engine)?
                } else if let (Value::Dict(obj), Expr::Refer(method)) = (&func, rhs) {
                    let obj = Expr::Value(Value::Dict(obj.clone()));
                    Op::Call(
                        Expr::Infix(Box::new(Op::Access(
                            obj.clone(),
                            Expr::Value(Value::Str(method.to_owned())),
                        ))),
                        obj,
                    )
                    .eval(engine)?
                } else {
                    return Err(Fault::Apply(func));
                }
            }
            Op::PipeLine(lhs, rhs) => Op::Call(rhs.to_owned(), lhs.to_owned()).eval(engine)?,
            Op::Assign(lhs, rhs) => Stmt::Let(lhs.to_owned(), rhs.to_owned()).eval(engine)?,
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
            "<" => Op::Less(has_lhs(2)?, token),
            "<=" => Op::LessEq(has_lhs(2)?, token),
            ">" => Op::Greater(has_lhs(2)?, token),
            ">=" => Op::GreaterEq(has_lhs(2)?, token),
            "&&" => Op::And(has_lhs(2)?, token),
            "||" => Op::Or(has_lhs(2)?, token),
            "::" => Op::Access(has_lhs(2)?, token),
            "as" => Op::As(has_lhs(2)?, token),
            "|>" => Op::PipeLine(has_lhs(2)?, token),
            ":=" => Op::Assign(has_lhs(2)?, token),
            "+=" => Op::Assign(
                has_lhs(2)?,
                Expr::Infix(Box::new(Op::Add(has_lhs(2)?, token))),
            ),
            "-=" => Op::Assign(
                has_lhs(2)?,
                Expr::Infix(Box::new(Op::Sub(has_lhs(2)?, token))),
            ),
            "*=" => Op::Assign(
                has_lhs(2)?,
                Expr::Infix(Box::new(Op::Mul(has_lhs(2)?, token))),
            ),
            "/=" => Op::Assign(
                has_lhs(2)?,
                Expr::Infix(Box::new(Op::Div(has_lhs(2)?, token))),
            ),
            "%=" => Op::Assign(
                has_lhs(2)?,
                Expr::Infix(Box::new(Op::Mod(has_lhs(2)?, token))),
            ),
            "^=" => Op::Assign(
                has_lhs(2)?,
                Expr::Infix(Box::new(Op::Pow(has_lhs(2)?, token))),
            ),
            "~" => Op::To(has_lhs(2)?, token),
            "-" => {
                if let Ok(lhs) = has_lhs(2) {
                    Op::Sub(lhs, token)
                } else if token_list.len() == 2 {
                    Op::Sub(Expr::Value(Value::Num(0.0)), token)
                } else {
                    Op::parse(&format!(
                        "{} (0 - {token})",
                        &join!(ok!(token_list.get(..token_list.len() - 2))?),
                    ))?
                }
            }
            "!" => {
                if token_list.len() == 2 {
                    Op::Not(token)
                } else {
                    Op::parse(&format!(
                        "{} (!{token})",
                        join!(ok!(token_list.get(..token_list.len() - 2))?),
                    ))?
                }
            }
            operator => {
                if operator.starts_with("`") && operator.ends_with("`") {
                    let operator = operator[1..operator.len() - 1].to_string();
                    Op::Call(
                        Expr::Infix(Box::new(Op::Call(Expr::parse(&operator)?, has_lhs(2)?))),
                        token,
                    )
                } else {
                    Op::Call(has_lhs(1)?, token)
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
            Op::Less(lhs, rhs) => Op::Less(lhs.replace(from, to), rhs.replace(from, to)),
            Op::LessEq(lhs, rhs) => Op::LessEq(lhs.replace(from, to), rhs.replace(from, to)),
            Op::Greater(lhs, rhs) => Op::Greater(lhs.replace(from, to), rhs.replace(from, to)),
            Op::GreaterEq(lhs, rhs) => Op::GreaterEq(lhs.replace(from, to), rhs.replace(from, to)),
            Op::And(lhs, rhs) => Op::And(lhs.replace(from, to), rhs.replace(from, to)),
            Op::Or(lhs, rhs) => Op::Or(lhs.replace(from, to), rhs.replace(from, to)),
            Op::Not(val) => Op::Not(val.replace(from, to)),
            Op::Access(lhs, rhs) => Op::Access(lhs.replace(from, to), rhs.replace(from, to)),
            Op::As(lhs, rhs) => Op::As(lhs.replace(from, to), rhs.replace(from, to)),
            Op::Call(lhs, rhs) => Op::Call(lhs.replace(from, to), rhs.replace(from, to)),
            Op::Assign(lhs, rhs) => Op::Assign(lhs.replace(from, to), rhs.replace(from, to)),
            Op::PipeLine(lhs, rhs) => Op::PipeLine(lhs.replace(from, to), rhs.replace(from, to)),
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
            Op::Less(lhs, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
            Op::LessEq(lhs, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
            Op::Greater(lhs, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
            Op::GreaterEq(lhs, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
            Op::And(lhs, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
            Op::Or(lhs, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
            Op::Not(val) => val.is_pure(engine),
            Op::Access(lhs, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
            Op::As(lhs, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
            Op::Call(lhs, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
            Op::Assign(lhs, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
            Op::PipeLine(lhs, rhs) => lhs.is_pure(engine) && rhs.is_pure(engine),
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
                Op::Less(lhs, rhs) => format!("{lhs} < {rhs}"),
                Op::LessEq(lhs, rhs) => format!("{lhs} <= {rhs}"),
                Op::Greater(lhs, rhs) => format!("{lhs} > {rhs}"),
                Op::GreaterEq(lhs, rhs) => format!("{lhs} >= {rhs}"),
                Op::And(lhs, rhs) => format!("{lhs} && {rhs}"),
                Op::Or(lhs, rhs) => format!("{lhs} || {rhs}"),
                Op::Not(val) => format!("!{val}"),
                Op::Access(lhs, rhs) => format!("{lhs} :: {rhs}"),
                Op::As(lhs, rhs) => format!("{lhs} as {rhs}",),
                Op::Assign(lhs, rhs) => format!("{lhs} := {rhs}",),
                Op::PipeLine(lhs, rhs) => format!("{lhs} |> {rhs}"),
                Op::Call(lhs, rhs) => format!("{lhs} {rhs}"),
                Op::To(lhs, rhs) => format!("{lhs} ~ {rhs}",),
            }
        )
    }
}
