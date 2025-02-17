use mltalk_core::{Block, Engine, Fault, Func, Node, Value};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = globalThis)]
    fn eval(js_code: &str) -> JsValue;
}

#[wasm_bindgen]
pub struct MLtalk {
    engine: Engine,
}

fn jsvalue_to_mltalk(result: JsValue) -> Result<Value, Fault> {
    if result.is_null() {
        Ok(Value::Null)
    } else if let Some(n) = result.as_f64() {
        Ok(Value::Num(n))
    } else if let Some(s) = result.as_string() {
        Ok(Value::Str(s))
    } else {
        Err(Fault::IO)
    }
}

#[wasm_bindgen]
impl MLtalk {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        MLtalk {
            engine: {
                let mut engine = Engine::new();
                let _ = engine.alloc(
                    &"jsEval".to_string(),
                    &Value::Func(Func::BuiltIn(|arg, _| {
                        let code = &arg.get_str()?;
                        if let Ok(result) = std::panic::catch_unwind(|| eval(code)) {
                            Ok(jsvalue_to_mltalk(result)?)
                        } else {
                            Err(Fault::IO)
                        }
                    })),
                );
                engine.set_effect("jsEval");
                engine
            },
        }
    }

    #[wasm_bindgen]
    pub fn eval(&mut self, code: &str) -> String {
        match Block::parse(code) {
            Ok(ast) => match ast.eval(&mut self.engine) {
                Ok(result) => result.to_string(),
                Err(e) => format!("Fault: {e}"),
            },
            Err(e) => format!("Fault: {e}"),
        }
    }
}
