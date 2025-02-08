use crate::*;

#[derive(Debug, Clone)]
pub enum Type {
    Num,
    Str,
    List,
    Range,
    Func(Option<Box<(Type, Type)>>),
    Kind,
    Dict,
    Class(String),
}

impl Type {
    pub fn parse(token: &str) -> Result<Type, Fault> {
        let token = token.trim();
        Ok(match token {
            "num" => Type::Num,
            "str" => Type::Str,
            "list" => Type::List,
            "range" => Type::Range,
            "func" => Type::Func(None),
            "kind" => Type::Kind,
            "dict" => Type::Dict,
            _ => {
                if token.starts_with("func(") && token.contains("->") {
                    let token = trim!(token, "func(", ")");
                    let (arg, ret) = ok!(token.split_once("->"))?;
                    Type::Func(Some(Box::new((Type::parse(arg)?, Type::parse(ret)?))))
                } else if token.starts_with("#") {
                    let ident = remove!(token, "#");
                    if is_identifier(&ident) {
                        Type::Class(ident)
                    } else {
                        return Err(Fault::Syntax);
                    }
                } else {
                    return Err(Fault::Syntax);
                }
            }
        })
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Type::Num => "num".to_string(),
                Type::Str => "str".to_string(),
                Type::List => "list".to_string(),
                Type::Range => "range".to_string(),
                Type::Func(None) => "func".to_string(),
                Type::Func(Some(anno)) => format!("func({} -> {})", anno.0, anno.1),
                Type::Kind => "kind".to_string(),
                Type::Dict => "dict".to_string(),
                Type::Class(c) => format!("#{c}"),
            }
        )
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Type) -> bool {
        format!("{self}") == format!("{other}")
    }
}
