use crate::*;

#[derive(Clone, Debug)]
pub enum Func {
    BuiltIn(fn(Value, &mut Engine) -> Result<Value, Fault>),
    UserDefined(String, Box<Expr>),
}

impl Func {
    pub fn parse(source: &str) -> Result<Self, Fault> {
        // Lambda abstract that original formula in the theory
        if source.starts_with("λ") && source.contains(".") {
            let source = remove!(source, "λ");
            let (arg, body) = ok!(source.split_once("."))?;
            Ok(Func::UserDefined(
                arg.to_string(),
                Box::new(Expr::parse(body)?),
            ))
        // Lambda abstract using back-slash instead of lambda mark
        } else if source.starts_with("\\") && source.contains(".") {
            let source = remove!(source, "\\");
            let (arg, body) = ok!(source.split_once("."))?;
            Ok(Func::UserDefined(
                arg.to_string(),
                Box::new(Expr::parse(body)?),
            ))
        } else {
            Err(Fault::Syntax)
        }
    }
}
