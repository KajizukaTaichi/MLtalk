use crate::*;

pub trait Node {
    fn eval(&self, engine: &mut Engine) -> Result<Value, Fault>;
    fn is_pure(&self) -> bool;
    fn replace(&self, from: &Expr, to: &Expr) -> Self;
    fn parse(source: &str) -> Result<Self, Fault>
    where
        Self: Node + Sized;
}
