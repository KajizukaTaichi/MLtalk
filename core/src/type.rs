use crate::*;

#[derive(Debug, Clone)]
pub enum Type {
    Num,
    Str,
    List,
    Dict,
    Range,
    Func(Option<Box<(Type, Type)>>),
    Kind,
    Class(String),
    Any,
}

impl Type {
    pub fn parse(token: &str) -> Result<Type, Fault> {
        let token = token.trim();
        Ok(match token {
            "num" => Type::Num,
            "str" => Type::Str,
            "list" => Type::List,
            "dict" => Type::Dict,
            "range" => Type::Range,
            "fn" => Type::Func(None),
            "kind" => Type::Kind,
            "any" => Type::Any,
            _ => {
                if token.starts_with("fn(") && token.contains("->") {
                    let token = trim!(token, "fn(", ")");
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
                Type::Dict => "dict".to_string(),
                Type::Range => "range".to_string(),
                Type::Func(None) => "fn".to_string(),
                Type::Func(Some(anno)) => format!("fn({} -> {})", anno.0, anno.1),
                Type::Kind => "kind".to_string(),
                Type::Class(c) => format!("#{c}"),
                Type::Any => "any".to_string(),
            }
        )
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Type) -> bool {
        if let Type::Any = self {
            true
        } else if let (Type::Func(None), Type::Func(_)) = (self, other) {
            true
        } else {
            format!("{self}") == format!("{other}")
        }
    }
}
