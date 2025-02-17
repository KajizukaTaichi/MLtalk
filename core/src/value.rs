use crate::*;

#[derive(Debug, Clone)]
pub enum Value {
    Num(f64),
    Str(String),
    List(Vec<Value>),
    Range(usize, usize),
    Func(Func),
    Type(Type),
    Dict(IndexMap<String, Value>),
    Null,
}

impl Value {
    pub fn eval(&self, engine: &mut Engine) -> Result<Value, Fault> {
        Expr::Value(self.clone()).eval(engine)
    }

    pub fn cast(&self, sig: &Type) -> Result<Value, Fault> {
        let err = Err(Fault::Cast(self.clone(), sig.clone()));
        Ok(match sig {
            Type::Num => Value::Num(match self {
                Value::Num(n) => n.to_owned(),
                Value::Str(s) => {
                    if let Ok(n) = s.trim().parse::<f64>() {
                        n
                    } else {
                        return err;
                    }
                }
                Value::Null => 0.0,
                _ => return err,
            }),
            Type::Str => Value::Str(match self {
                Value::Str(s) => s.to_string(),
                Value::Null => String::new(),
                _ => format!("{self}"),
            }),
            Type::List(typ) => {
                let value = Value::List(match self {
                    Value::List(list) => list.to_owned(),
                    Value::Str(str) => str.chars().map(|i| Value::Str(i.to_string())).collect(),
                    Value::Dict(strct) => strct
                        .iter()
                        .map(|(k, y)| Value::List(vec![Value::Str(k.to_owned()), y.to_owned()]))
                        .collect(),
                    Value::Range(start, end) => {
                        let mut range: Vec<Value> = vec![];
                        let mut current = *start;
                        while current < *end {
                            range.push(Value::Num(current as f64));
                            current += 1;
                        }
                        range
                    }
                    Value::Null => Vec::new(),
                    _ => return err,
                });
                if let Some(typ) = typ {
                    if *typ.clone() != value.type_of() {
                        return Err(Fault::Type(value, sig.clone()));
                    }
                }
                value
            }
            Type::Kind => Value::Type(Type::parse(&self.get_str()?)?),
            _ => return err,
        })
    }

    pub fn get_number(&self) -> Result<f64, Fault> {
        match self {
            Value::Num(n) => Ok(n.to_owned()),
            _ => Err(Fault::Type(self.clone(), Type::Num)),
        }
    }

    pub fn get_str(&self) -> Result<String, Fault> {
        match self {
            Value::Str(s) => Ok(s.to_string()),
            _ => Err(Fault::Type(self.clone(), Type::Str)),
        }
    }

    pub fn get_list(&self) -> Result<Vec<Value>, Fault> {
        match self {
            Value::List(list) => Ok(list.to_owned()),
            _ => Err(Fault::Type(self.clone(), Type::List(None))),
        }
    }

    pub fn get_dict(&self) -> Result<IndexMap<String, Value>, Fault> {
        match self {
            Value::Dict(list) => Ok(list.to_owned()),
            _ => Err(Fault::Type(self.clone(), Type::Dict(None))),
        }
    }

    pub fn get_type(&self) -> Result<Type, Fault> {
        match self {
            Value::Type(sig) => Ok(sig.to_owned()),
            _ => Err(Fault::Type(self.clone(), Type::Kind)),
        }
    }

    pub fn type_of(&self) -> Type {
        match self {
            Value::Num(_) => Type::Num,
            Value::Str(_) => Type::Str,
            Value::List(list) => {
                let mut type_list = vec![];
                for i in list {
                    let ityp = i.type_of();
                    if !type_list.contains(&ityp) {
                        type_list.push(ityp);
                    }
                }
                if type_list.len() == 0 {
                    Type::List(None)
                } else if type_list.len() == 1 {
                    Type::List(Some(Box::new(type_list[0].clone())))
                } else {
                    Type::List(Some(Box::new(Type::Union(type_list))))
                }
            }
            Value::Range(_, _) => Type::Range,
            Value::Func(Func::UserDefined(_, _, func_type)) => func_type.clone(),
            Value::Func(_) => Type::Func(None, Mode::Pure),
            Value::Type(_) => Type::Kind,
            Value::Dict(dict) => {
                let mut result = IndexMap::new();
                for (k, v) in dict {
                    result.insert(k.to_owned(), v.type_of());
                }
                Type::Dict(Some(result))
            }
            Value::Null => Type::Kind,
        }
    }

    pub fn modify_inside(
        &self,
        index: &Value,
        val: &Option<Value>,
        engine: &mut Engine,
    ) -> Result<Value, Fault> {
        let err = Err(Fault::Infix(Op::Access(
            Expr::Value(self.clone()),
            Expr::Value(index.clone()),
        )));
        Ok(match self.clone() {
            Value::List(mut list) => match index.clone() {
                Value::Num(index) => {
                    index_check!(list, index, self);
                    if let Some(val) = val {
                        list[index as usize] = val.clone();
                    } else {
                        list.remove(index as usize);
                    }
                    Value::List(list)
                }
                Value::Range(start, end) => {
                    for _ in start..end {
                        index_check!(list, start as f64, self);
                        list.remove(start);
                    }
                    if let Some(val) = val {
                        list.insert(start, val.clone());
                    }
                    Value::List(list)
                }
                Value::List(_) => self.modify_inside(
                    &Op::Access(Expr::Value(self.clone()), Expr::Value(index.clone()))
                        .eval(engine)?,
                    val,
                    engine,
                )?,
                _ => return err,
            },
            Value::Str(str) => match index.clone() {
                Value::Num(index) => {
                    let mut str = char_vec!(str);
                    index_check!(str, index, self);
                    if let Some(val) = val {
                        str[index as usize] = val.get_str()?;
                    } else {
                        str.remove(index as usize);
                    }
                    Value::Str(str.concat())
                }
                Value::Range(start, end) => {
                    let mut str = char_vec!(str);
                    for _ in start..end {
                        index_check!(str, start as f64, self);
                        str.remove(start);
                    }
                    if let Some(val) = val {
                        str.insert(start, val.get_str()?);
                    }
                    Value::Str(str.concat())
                }
                Value::Str(query) => {
                    if let Some(val) = val {
                        Value::Str(str.replace(&query, &val.get_str()?))
                    } else {
                        Value::Str(remove!(str, &query))
                    }
                }

                _ => return err,
            },
            Value::Dict(mut st) => match index {
                Value::Str(key) => {
                    if let Some(val) = val {
                        st.insert(key.clone(), val.clone());
                    } else {
                        st.shift_remove(key);
                    }
                    Value::Dict(st)
                }
                _ => return err,
            },
            _ => return err,
        })
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Value::Str(str) => format!(
                    "\"{}\"",
                    str.replace("\\", "\\\\")
                        .replace("'", "\\'")
                        .replace("\"", "\\\"")
                        .replace("`", "\\`")
                        .replace("\n", "\\n")
                        .replace("\t", "\\t")
                        .replace("\r", "\\r")
                ),
                Value::Num(n) => n.to_string(),
                Value::Null => "null".to_string(),
                Value::Func(Func::BuiltIn(obj)) => format!("λx.{obj:?}"),
                Value::Func(Func::UserDefined(arg, code, Type::Func(Some(anno), Mode::Pure))) =>
                    format!("(λ{arg}: {}. {code} -> {})", anno.0, anno.1),
                Value::Func(Func::UserDefined(arg, code, Type::Func(Some(anno), Mode::Effect))) =>
                    format!("(λ{arg}: {}. {code} -> {} + effect)", anno.0, anno.1),
                Value::Func(Func::UserDefined(arg, code, _)) => format!("(λ{arg}. {code})"),
                Value::List(l) => format!(
                    "[{}]",
                    l.iter()
                        .map(|i| format!("{i}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
                Value::Range(start, end) => format!("({start} ~ {end})",),
                Value::Type(kind) => format!("{kind}"),
                Value::Dict(val) => format!(
                    "{{ {} }}",
                    val.iter()
                        .map(|(k, v)| format!("{k}: {v}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
            }
        )
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        format!("{self}") == format!("{other}")
    }
}
