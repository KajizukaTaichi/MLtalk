use crate::*;

#[derive(Clone, Debug)]
pub enum Func {
    BuiltIn(fn(Value, &mut Engine) -> Result<Value, Fault>),
    UserDefined(String, Box<Expr>, Type),
}

impl Func {
    pub fn parse(source: &str) -> Result<Self, Fault> {
        // Lambda abstract that original formula in the theory
        if source.starts_with("λ") && source.contains(".") {
            let source = remove!(source, "λ");
            Self::common(&source)
        // Lambda abstract using back-slash instead of lambda mark
        } else if source.starts_with("\\") && source.contains(".") {
            let source = remove!(source, "\\");
            Self::common(&source)
        } else {
            Err(Fault::Syntax)
        }
    }

    fn common(source: &str) -> Result<Self, Fault> {
        let (arg, body) = ok!(source.split_once("."))?;
        let arg = arg.trim();
        if arg.is_empty() {
            return Err(Fault::Syntax);
        }
        let (arg, body, annotation) = if let (Some((arg, ano_arg)), Some((body, ano_ret))) =
            (arg.split_once(":"), body.rsplit_once("->"))
        {
            (
                arg,
                body,
                Some(Box::new((Type::parse(ano_arg)?, Type::parse(ano_ret)?))),
            )
        } else {
            (arg, body, None)
        };
        if !is_identifier(arg) {
            return Err(Fault::Syntax);
        }
        dbg!(&arg, &body, &annotation);
        Ok(Func::UserDefined(
            arg.to_string(),
            Box::new(Expr::parse(body)?),
            Type::Func(annotation),
        ))
    }
}
