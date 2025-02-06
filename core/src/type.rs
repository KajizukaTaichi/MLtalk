use crate::*;

#[derive(Debug, Clone)]
pub enum Type {
    Num,
    Str,
    List,
    Range,
    Func,
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
            "func" => Type::Func,
            "kind" => Type::Kind,
            "dict" => Type::Dict,
            _ => {
                if token.starts_with("#") {
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
                Type::Func => "func".to_string(),
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
