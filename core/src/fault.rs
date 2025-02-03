use crate::*;

#[derive(Error)]
pub enum Fault {
    #[error("can not apply Function because `{0}` is not lambda abstract")]
    Apply(Value),

    #[error("key `{0}` is not found in `{1}`")]
    Key(Value, Value),

    #[error("index `{0}` is out of `{1}`")]
    Index(Value, Value),

    #[error("`{0}` doesn't match in let statement")]
    Let(Expr),

    #[error("range specification order should be ascension")]
    Range,

    #[error("access is denied because it's protected memory area")]
    AccessDenied,

    #[error("can not access undefined variable `{0}`")]
    Refer(String),

    #[error("can not type cast `{0}` to {1}")]
    Cast(Value, Type),

    #[error("at the IO processing has problem")]
    IO,

    #[error("the value `{0}` is different to expected type `{1}`")]
    Value(Value, Type),

    #[error("missmatching of arguments length when function application")]
    ArgLen,

    #[error("the program is not able to parse. check out is the syntax correct")]
    Syntax,

    #[error("can not evaluate expression `{0}`")]
    Infix(Op),

    #[error("the logical operation `{0}` has bankruptcy")]
    Logic(Op),

    #[error("{}", _0.clone().unwrap_or("throwed by user-defined program".to_string()))]
    General(Option<String>),
}

impl Debug for Fault {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}
