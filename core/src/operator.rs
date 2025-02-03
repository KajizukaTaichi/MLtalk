use crate::*;

#[derive(Debug, Clone)]
pub enum Operator {
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

impl Operator {
    pub fn eval(&self, engine: &mut Engine) -> Result<Value, Fault> {
        Ok(match self {
            Operator::Add(lhs, rhs) => {
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
            Operator::Sub(lhs, rhs) => {
                let lhs = lhs.eval(engine)?;
                let rhs = rhs.eval(engine)?;
                if let (Value::Num(lhs), Value::Num(rhs)) = (&lhs, &rhs) {
                    Value::Num(lhs - rhs)
                } else {
                    lhs.modify_inside(&rhs, &None, engine)?
                }
            }
            Operator::Mul(lhs, rhs) => {
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
            Operator::Div(lhs, rhs) => {
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
            Operator::Mod(lhs, rhs) => {
                Value::Num(lhs.eval(engine)?.get_number()? % rhs.eval(engine)?.get_number()?)
            }
            Operator::Pow(lhs, rhs) => Value::Num(
                lhs.eval(engine)?
                    .get_number()?
                    .powf(rhs.eval(engine)?.get_number()?),
            ),
            Operator::Equal(lhs, rhs) => {
                let lhs = lhs.eval(engine)?;
                let rhs = rhs.eval(engine)?;
                if rhs.is_match(&lhs) {
                    rhs
                } else {
                    return Err(Fault::Logic(self.clone()));
                }
            }
            Operator::NotEq(lhs, rhs) => {
                let lhs = lhs.eval(engine)?;
                let rhs = rhs.eval(engine)?;
                if !rhs.is_match(&lhs) {
                    rhs
                } else {
                    return Err(Fault::Logic(self.clone()));
                }
            }
            Operator::LessThan(lhs, rhs) => {
                let rhs = rhs.eval(engine)?;
                if lhs.eval(engine)?.get_number()? < rhs.get_number()? {
                    rhs
                } else {
                    return Err(Fault::Logic(self.clone()));
                }
            }
            Operator::LessThanEq(lhs, rhs) => {
                let rhs = rhs.eval(engine)?;
                if lhs.eval(engine)?.get_number()? <= rhs.get_number()? {
                    rhs
                } else {
                    return Err(Fault::Logic(self.clone()));
                }
            }
            Operator::GreaterThan(lhs, rhs) => {
                let rhs = rhs.eval(engine)?;
                if lhs.eval(engine)?.get_number()? > rhs.get_number()? {
                    rhs
                } else {
                    return Err(Fault::Logic(self.clone()));
                }
            }
            Operator::GreaterThanEq(lhs, rhs) => {
                let rhs = rhs.eval(engine)?;
                if lhs.eval(engine)?.get_number()? >= rhs.get_number()? {
                    rhs
                } else {
                    return Err(Fault::Logic(self.clone()));
                }
            }
            Operator::And(lhs, rhs) => {
                let rhs = rhs.eval(engine);
                if lhs.eval(engine).is_ok() && rhs.is_ok() {
                    rhs?
                } else {
                    return Err(Fault::Logic(self.clone()));
                }
            }
            Operator::Or(lhs, rhs) => {
                let lhs = lhs.eval(engine);
                let rhs = rhs.eval(engine);
                if lhs.is_ok() || rhs.is_ok() {
                    rhs.unwrap_or(lhs?)
                } else {
                    return Err(Fault::Logic(self.clone()));
                }
            }
            Operator::Not(val) => {
                let val = val.eval(engine);
                if val.is_ok() {
                    return Err(Fault::Logic(self.clone()));
                } else {
                    Value::Null
                }
            }
            Operator::Access(lhs, rhs) => {
                let lhs = lhs.eval(engine)?;
                let rhs = rhs.eval(engine)?;
                let err = Err(Fault::Apply(lhs.clone()));
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
            Operator::As(lhs, rhs) => {
                let lhs = lhs.eval(engine)?;
                let rhs = rhs.eval(engine)?;
                lhs.cast(&rhs.get_type()?)?
            }
            Operator::Apply(lhs, is_lazy, rhs) => {
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
                    Operator::Apply(
                        Expr::Infix(Box::new(Operator::Access(
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
            Operator::PipeLine(lhs, rhs) => {
                Operator::Apply(rhs.to_owned(), false, lhs.to_owned()).eval(engine)?
            }
            Operator::Assign(lhs, rhs) => {
                Statement::Let(lhs.to_owned(), false, rhs.to_owned()).eval(engine)?
            }
            Operator::AssignAdd(name, rhs) => Operator::Assign(
                name.to_owned(),
                Expr::Infix(Box::new(Operator::Add(name.to_owned(), rhs.clone()))),
            )
            .eval(engine)?,
            Operator::AssignSub(name, rhs) => Operator::Assign(
                name.to_owned(),
                Expr::Infix(Box::new(Operator::Sub(name.to_owned(), rhs.clone()))),
            )
            .eval(engine)?,
            Operator::AssignMul(name, rhs) => Operator::Assign(
                name.to_owned(),
                Expr::Infix(Box::new(Operator::Mul(name.to_owned(), rhs.clone()))),
            )
            .eval(engine)?,
            Operator::AssignDiv(name, rhs) => Operator::Assign(
                name.to_owned(),
                Expr::Infix(Box::new(Operator::Div(name.to_owned(), rhs.clone()))),
            )
            .eval(engine)?,
            Operator::AssignMod(name, rhs) => Operator::Assign(
                name.to_owned(),
                Expr::Infix(Box::new(Operator::Mod(name.to_owned(), rhs.clone()))),
            )
            .eval(engine)?,
            Operator::AssignPow(name, rhs) => Operator::Assign(
                name.to_owned(),
                Expr::Infix(Box::new(Operator::Pow(name.to_owned(), rhs.clone()))),
            )
            .eval(engine)?,
            Operator::To(from, to) => {
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

    pub fn parse(source: &str) -> Result<Self, Fault> {
        let token_list: Vec<String> = tokenize(source, SPACE.as_ref())?;
        let token = Expr::parse(ok!(token_list.last())?)?;
        let operator = ok!(token_list.get(ok!(token_list.len().checked_sub(2))?))?;
        let has_lhs =
            |len: usize| Expr::parse(&join!(ok!(token_list.get(..token_list.len() - len))?));
        Ok(match operator.as_str() {
            "+" => Operator::Add(has_lhs(2)?, token),
            "-" => Operator::Sub(has_lhs(2)?, token),
            "*" => Operator::Mul(has_lhs(2)?, token),
            "/" => Operator::Div(has_lhs(2)?, token),
            "%" => Operator::Mod(has_lhs(2)?, token),
            "^" => Operator::Pow(has_lhs(2)?, token),
            "==" => Operator::Equal(has_lhs(2)?, token),
            "!=" => Operator::NotEq(has_lhs(2)?, token),
            "<" => Operator::LessThan(has_lhs(2)?, token),
            "<=" => Operator::LessThanEq(has_lhs(2)?, token),
            ">" => Operator::GreaterThan(has_lhs(2)?, token),
            ">=" => Operator::GreaterThanEq(has_lhs(2)?, token),
            "&" => Operator::And(has_lhs(2)?, token),
            "|" => Operator::Or(has_lhs(2)?, token),
            "?" => Operator::Apply(has_lhs(2)?, true, token),
            "::" => Operator::Access(has_lhs(2)?, token),
            "as" => Operator::As(has_lhs(2)?, token),
            "|>" => Operator::PipeLine(has_lhs(2)?, token),
            ":=" => Operator::Assign(has_lhs(2)?, token),
            "+=" => Operator::AssignAdd(has_lhs(2)?, token),
            "-=" => Operator::AssignSub(has_lhs(2)?, token),
            "*=" => Operator::AssignMul(has_lhs(2)?, token),
            "/=" => Operator::AssignDiv(has_lhs(2)?, token),
            "%=" => Operator::AssignMod(has_lhs(2)?, token),
            "^=" => Operator::AssignPow(has_lhs(2)?, token),
            "~" => Operator::To(has_lhs(2)?, token),
            operator => {
                if operator.starts_with("`") && operator.ends_with("`") {
                    let operator = operator[1..operator.len() - 1].to_string();
                    Operator::Apply(
                        Expr::Infix(Box::new(Operator::Apply(
                            Expr::parse(&operator)?,
                            false,
                            has_lhs(2)?,
                        ))),
                        false,
                        token,
                    )
                } else {
                    Operator::Apply(has_lhs(1)?, false, token)
                }
            }
        })
    }

    pub fn replace(&self, from: &Expr, to: &Expr) -> Self {
        match self {
            Operator::Add(lhs, rhs) => Operator::Add(lhs.replace(from, to), rhs.replace(from, to)),
            Operator::Sub(lhs, rhs) => Operator::Sub(lhs.replace(from, to), rhs.replace(from, to)),
            Operator::Mul(lhs, rhs) => Operator::Mul(lhs.replace(from, to), rhs.replace(from, to)),
            Operator::Div(lhs, rhs) => Operator::Div(lhs.replace(from, to), rhs.replace(from, to)),
            Operator::Mod(lhs, rhs) => Operator::Mod(lhs.replace(from, to), rhs.replace(from, to)),
            Operator::Pow(lhs, rhs) => Operator::Pow(lhs.replace(from, to), rhs.replace(from, to)),
            Operator::Equal(lhs, rhs) => {
                Operator::Equal(lhs.replace(from, to), rhs.replace(from, to))
            }
            Operator::NotEq(lhs, rhs) => {
                Operator::NotEq(lhs.replace(from, to), rhs.replace(from, to))
            }
            Operator::LessThan(lhs, rhs) => {
                Operator::LessThan(lhs.replace(from, to), rhs.replace(from, to))
            }
            Operator::LessThanEq(lhs, rhs) => {
                Operator::LessThanEq(lhs.replace(from, to), rhs.replace(from, to))
            }
            Operator::GreaterThan(lhs, rhs) => {
                Operator::GreaterThan(lhs.replace(from, to), rhs.replace(from, to))
            }
            Operator::GreaterThanEq(lhs, rhs) => {
                Operator::GreaterThanEq(lhs.replace(from, to), rhs.replace(from, to))
            }
            Operator::And(lhs, rhs) => Operator::And(lhs.replace(from, to), rhs.replace(from, to)),
            Operator::Or(lhs, rhs) => Operator::Or(lhs.replace(from, to), rhs.replace(from, to)),
            Operator::Not(val) => Operator::Not(val.replace(from, to)),
            Operator::Access(lhs, rhs) => {
                Operator::Access(lhs.replace(from, to), rhs.replace(from, to))
            }
            Operator::As(lhs, rhs) => Operator::As(lhs.replace(from, to), rhs.replace(from, to)),
            Operator::Apply(lhs, is_lazy, rhs) => {
                Operator::Apply(lhs.replace(from, to), *is_lazy, rhs.replace(from, to))
            }
            Operator::Assign(lhs, rhs) => {
                Operator::Assign(lhs.replace(from, to), rhs.replace(from, to))
            }
            Operator::PipeLine(lhs, rhs) => {
                Operator::PipeLine(lhs.replace(from, to), rhs.replace(from, to))
            }
            Operator::AssignAdd(lhs, rhs) => {
                Operator::AssignAdd(lhs.replace(from, to), rhs.replace(from, to))
            }
            Operator::AssignSub(lhs, rhs) => {
                Operator::AssignSub(lhs.replace(from, to), rhs.replace(from, to))
            }
            Operator::AssignMul(lhs, rhs) => {
                Operator::AssignMul(lhs.replace(from, to), rhs.replace(from, to))
            }
            Operator::AssignDiv(lhs, rhs) => {
                Operator::AssignDiv(lhs.replace(from, to), rhs.replace(from, to))
            }
            Operator::AssignMod(lhs, rhs) => {
                Operator::AssignMod(lhs.replace(from, to), rhs.replace(from, to))
            }
            Operator::AssignPow(lhs, rhs) => {
                Operator::AssignPow(lhs.replace(from, to), rhs.replace(from, to))
            }
            Operator::To(lhs, rhs) => Operator::To(lhs.replace(from, to), rhs.replace(from, to)),
        }
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Operator::Add(lhs, rhs) => format!("{lhs} + {rhs}"),
                Operator::Sub(lhs, rhs) => format!("{lhs} - {rhs}"),
                Operator::Mul(lhs, rhs) => format!("{lhs} * {rhs}"),
                Operator::Div(lhs, rhs) => format!("{lhs} / {rhs}"),
                Operator::Mod(lhs, rhs) => format!("{lhs} % {rhs}"),
                Operator::Pow(lhs, rhs) => format!("{lhs} ^ {rhs}"),
                Operator::Equal(lhs, rhs) => format!("{lhs} == {rhs}"),
                Operator::NotEq(lhs, rhs) => format!("{lhs} != {rhs}"),
                Operator::LessThan(lhs, rhs) => format!("{lhs} < {rhs}"),
                Operator::LessThanEq(lhs, rhs) => format!("{lhs} <= {rhs}"),
                Operator::GreaterThan(lhs, rhs) => format!("{lhs} > {rhs}"),
                Operator::GreaterThanEq(lhs, rhs) => format!("{lhs} >= {rhs}"),
                Operator::And(lhs, rhs) => format!("{lhs} & {rhs}"),
                Operator::Or(lhs, rhs) => format!("{lhs} | {rhs}"),
                Operator::Not(val) => format!("!{val}"),
                Operator::Access(lhs, rhs) => format!("{lhs} :: {rhs}"),
                Operator::As(lhs, rhs) => format!("{lhs} as {rhs}",),
                Operator::Assign(lhs, rhs) => format!("{lhs} := {rhs}",),
                Operator::PipeLine(lhs, rhs) => format!("{lhs} |> {rhs}"),
                Operator::Apply(lhs, true, rhs) => format!("{lhs} ? {rhs}"),
                Operator::Apply(lhs, false, rhs) => format!("{lhs} {rhs}"),
                Operator::AssignAdd(lhs, rhs) => format!("{lhs} += {rhs}"),
                Operator::AssignSub(lhs, rhs) => format!("{lhs} -= {rhs}"),
                Operator::AssignMul(lhs, rhs) => format!("{lhs} *= {rhs}"),
                Operator::AssignDiv(lhs, rhs) => format!("{lhs} /= {rhs}"),
                Operator::AssignMod(lhs, rhs) => format!("{lhs} %= {rhs}"),
                Operator::AssignPow(lhs, rhs) => format!("{lhs} ^= {rhs}"),
                Operator::To(lhs, rhs) => format!("{lhs} ~ {rhs}",),
            }
        )
    }
}
