use mltalk_core::{Block, Engine, Node};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct MLtalk {
    engine: Engine,
}

#[wasm_bindgen]
impl MLtalk {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        MLtalk {
            engine: Engine::new(),
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
