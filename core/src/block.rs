use crate::*;

#[derive(Debug, Clone)]
pub struct Block(pub Vec<Stmt>);

impl Block {
    pub fn parse(source: &str) -> Result<Block, Fault> {
        let mut program = Vec::new();
        for line in tokenize(source, &[";"])? {
            let line = line.trim();
            // Ignore empty line and comment
            if line.is_empty() || line.starts_with("//") {
                continue;
            }
            program.push(Stmt::parse(line)?);
        }
        Ok(Block(program))
    }

    pub fn eval(&self, engine: &mut Engine) -> Result<Value, Fault> {
        let mut result = Value::Null;
        for code in &self.0 {
            result = code.eval(engine)?
        }
        Ok(result)
    }

    pub fn replace(&self, from: &Expr, to: &Expr) -> Self {
        Block(self.0.iter().map(|i| Stmt::replace(i, from, to)).collect())
    }

    pub fn is_pure(&self) -> bool {
        self.0.iter().all(|i| i.is_pure())
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "begin {} end",
            self.0
                .iter()
                .map(|i| format!("{i}"))
                .collect::<Vec<String>>()
                .join("; ")
        )
    }
}
