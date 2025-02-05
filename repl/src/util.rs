pub const NAME: &str = "MLtalk";
pub const ABOUT: &str = "ML-family programming language that you can write code smoothly";
pub const VERSION: &str = "0.1.0";

#[macro_export]
macro_rules! repl_print {
    ($color: ident, $value: expr) => {
        println!("{navi} {}", $value, navi = "=>".$color().bold())
    };
}

#[macro_export]
macro_rules! fault {
    ($e: expr) => {{
        print!("\x07");
        repl_print!(red, format!("Fault: {:?}", $e))
    }};
}

#[macro_export]
macro_rules! crash {
    ($result: expr) => {
        match $result {
            Ok(v) => v,
            Err(e) => {
                eprintln!("{}: {e:?}", "Fault".red());
                std::process::exit(1);
            }
        }
    };
}
