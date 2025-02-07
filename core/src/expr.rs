use crate::*;

#[derive(Debug, Clone)]
pub enum Expr {
    Refer(String),
    Infix(Box<Op>),
    List(Vec<Expr>),
    Dict(Vec<(String, Expr)>),
    Block(Block),
    Value(Value),
}

impl Node for Expr {
    fn eval(&self, engine: &mut Engine) -> Result<Value, Fault> {
        Ok(match self {
            Expr::Refer(name) => {
                if name == "_" {
                    Value::Null
                } else {
                    engine.access(name.as_str())?
                }
            }
            Expr::Infix(infix) => (*infix).eval(engine)?,
            Expr::Block(block) => block.clone().eval(engine)?,
            Expr::List(list) => {
                let mut result = vec![];
                for i in list {
                    result.push(i.eval(engine)?)
                }
                Value::List(result)
            }
            Expr::Dict(st) => {
                let mut result = IndexMap::new();
                for (k, x) in st {
                    result.insert(k.to_string(), x.eval(engine)?);
                }
                Value::Dict(result)
            }
            Expr::Value(value) => value.clone(),
        })
    }

    fn parse(source: &str) -> Result<Expr, Fault> {
        let source = source.trim();
        if let Ok(func) = Func::parse(source) {
            return Ok(Expr::Value(Value::Func(func)));
        }

        let token_list: Vec<String> = tokenize(source.trim(), SPACE.as_ref(), true)?;
        if token_list.len() >= 2 {
            Ok(Expr::Infix(Box::new(Op::parse(source)?)))
        } else {
            let token = ok!(token_list.last())?.trim().to_string();
            Ok(if let Ok(n) = token.parse::<f64>() {
                Expr::Value(Value::Num(n))
            } else if let Ok(sig) = Type::parse(&token) {
                Expr::Value(Value::Type(sig))
            // Prefix not operator
            } else if token.starts_with("!") {
                let token = remove!(token, "!");
                Expr::Infix(Box::new(Op::Not(Expr::parse(&token)?)))
            } else if token.starts_with("(") && token.ends_with(")") {
                let token = trim!(token, "(", ")");
                Expr::parse(token)?
            } else if token.starts_with(BEGIN) && token.ends_with(END) {
                let token = trim!(token, BEGIN, END);
                Expr::Block(Block::parse(token)?).optimize()
            } else if token.starts_with("{") && token.ends_with("}") {
                let token = trim!(token, "{", "}");
                let mut result = Vec::new();
                for i in tokenize(token, &[","], false)? {
                    let i = i.trim();
                    if i.is_empty() {
                        continue;
                    }
                    let splited = tokenize(&i, &[":"], false)?;
                    let key = ok!(splited.first())?.trim().to_string();
                    if !is_identifier(&key) {
                        return Err(Fault::Syntax);
                    }
                    let value = if splited.len() >= 2 {
                        join!(ok!(splited.get(1..))?, ":")
                    } else {
                        key.clone()
                    };
                    result.push((key, Expr::parse(&value)?));
                }
                Expr::Dict(result)
            } else if token.starts_with("[") && token.ends_with("]") {
                let token = trim!(token, "[", "]");
                let mut list = vec![];
                for elm in tokenize(token, &[","], true)? {
                    let elm = elm.trim();
                    if elm.is_empty() {
                        continue;
                    }
                    list.push(Expr::parse(&elm)?);
                }
                Expr::List(list)
            } else if token.starts_with("\"") && token.ends_with("\"") {
                let str = trim!(token, "\"", "\"");
                Expr::Value(Value::Str(str_escape(str)))
            // Text formatting
            } else if token.starts_with("f\"") && token.ends_with('"') {
                let str = trim!(token, "f\"", "\"");
                let str = str_format(str)?;
                let mut result = Expr::Value(Value::Str(String::new()));
                for elm in str {
                    if elm.starts_with("{") && elm.ends_with("}") {
                        let elm = trim!(elm, "{", "}");
                        result = Expr::Infix(Box::new(Op::Add(
                            result,
                            Expr::Infix(Box::new(Op::As(
                                Expr::Block(Block::parse(elm)?).optimize(),
                                Expr::Value(Value::Type(Type::Str)),
                            ))),
                        )));
                    } else {
                        result = Expr::Infix(Box::new(Op::Add(
                            result,
                            Expr::Value(Value::Str(str_escape(&elm))),
                        )));
                    }
                }
                result
            // Imperative style syntactic sugar of list access by index
            } else if token.contains('(') && token.ends_with(')') {
                let token = trim!(token, "", ")");
                let (name, args) = ok!(token.split_once("("))?;
                let args: Vec<String> = tokenize(args, &vec![","], true)?;
                let mut result = Expr::Infix(Box::new(Op::Apply(
                    Expr::parse(name.trim())?,
                    false,
                    Expr::parse(ok!(args.first())?)?,
                )));
                for i in ok!(args.get(1..))? {
                    result =
                        Expr::Infix(Box::new(Op::Apply(result, false, Expr::parse(i.trim())?)));
                }
                result
            // Imperative style syntactic sugar of list access by index
            } else if token.contains('[') && token.ends_with(']') {
                let token = trim!(token, "", "]");
                let (name, args) = ok!(token.split_once("["))?;
                let args: Vec<String> = tokenize(args, &vec![","], true)?;
                let mut result = Expr::Infix(Box::new(Op::Access(
                    Expr::parse(name.trim())?,
                    Expr::parse(ok!(args.first())?)?,
                )));
                for i in ok!(args.get(1..))? {
                    result = Expr::Infix(Box::new(Op::Access(result, Expr::parse(i.trim())?)));
                }
                result
            // Object-oriented style syntactic sugar of access operator
            } else if token.matches(".").count() >= 1 {
                let (obj, key) = ok!(token.rsplit_once("."))?;
                Expr::Infix(Box::new(Op::Access(
                    Expr::parse(obj)?,
                    Expr::Value(Value::Str(key.trim().to_string())),
                )))
            } else if token == "null" {
                Expr::Value(Value::Null)
            } else if is_identifier(&token) {
                Expr::Refer(token)
            } else {
                // Funcize operator
                let source = format!("(λx. (λy. (x {token} y)))");
                let expr = Expr::parse(&source)?;
                if format!("{expr}") != source {
                    return Err(Fault::Syntax);
                }
                expr
            }
            .optimize())
        }
    }

    /// Beta reduction of constant arguments when apply Function
    fn replace(&self, from: &Expr, to: &Expr) -> Expr {
        match self {
            Expr::List(list) => Expr::List(
                list.iter()
                    .map(|i| i.replace(from, to))
                    .collect::<Vec<Expr>>(),
            ),
            Expr::Dict(st) => Expr::Dict(
                st.iter()
                    .map(|(k, x)| (k.to_owned(), x.replace(from, to)))
                    .collect::<Vec<(String, Expr)>>(),
            ),
            Expr::Infix(infix) => Expr::Infix(Box::new(infix.replace(from, to))),
            Expr::Block(block) => Expr::Block(block.replace(from, to)),
            Expr::Refer(val) => {
                if let Expr::Refer(from) = from {
                    if val == from {
                        to.clone()
                    } else {
                        self.clone()
                    }
                } else {
                    self.clone()
                }
            }
            Expr::Value(Value::Func(Func::UserDefined(arg, func))) => {
                Expr::Value(Value::Func(Func::UserDefined(
                    arg.to_string(),
                    // Protect from duplicate replacing
                    if format!("{from}") == "self" || format!("{from}") == *arg {
                        func.clone()
                    } else {
                        Box::new(func.replace(from, to))
                    },
                )))
            }
            Expr::Value(val) => Expr::Value(val.clone()),
        }
    }

    fn is_pure(&self, engine: &Engine) -> bool {
        match self {
            Expr::List(list) => list.iter().all(|i| i.is_pure(engine)),
            Expr::Dict(st) => st.iter().all(|(_, x)| x.is_pure(engine)),
            Expr::Infix(infix) => infix.is_pure(engine),
            Expr::Block(block) => block.is_pure(engine),
            Expr::Refer(val) => !engine.is_effective(&val.as_str()),
            Expr::Value(Value::Func(Func::UserDefined(_, func))) => func.is_pure(engine),
            Expr::Value(_) => true,
        }
    }
}

impl Expr {
    fn optimize_mut(&mut self) {
        if let Expr::Block(Block(vec)) = self {
            if vec.len() == 1 {
                if let Stmt::Expr(expr) = vec[0].clone() {
                    *self = expr;
                }
            }
        }
    }

    fn optimize(&self) -> Self {
        let mut expr = self.clone();
        expr.optimize_mut();
        expr
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Expr::Refer(refer) => refer.to_string(),
                Expr::List(list) => format!(
                    "[{}]",
                    list.iter()
                        .map(|i| format!("{i}"))
                        .collect::<Vec<String>>()
                        .join(", "),
                ),
                Expr::Infix(infix) => format!("({infix})"),
                Expr::Value(val) => format!("{val}"),
                Expr::Block(block) => format!("{block}"),
                Expr::Dict(st) => format!(
                    "{{ {} }}",
                    st.iter()
                        .map(|(k, v)| format!("{k}: {v}"))
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
            }
        )
    }
}
