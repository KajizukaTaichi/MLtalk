use crate::*;

pub trait Node {
    fn eval(&self, engine: &mut Engine) -> Result<Value, Fault>;
    fn is_pure(&self, engine: &Engine) -> bool;
    fn replace(&self, from: &Expr, to: &Expr) -> Self;
    fn infer(&self, engine: &Engine) -> Type;
    fn parse(source: &str) -> Result<Self, Fault>
    where
        Self: Node + Sized;
}
