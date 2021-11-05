use std::error::Error;

use toy_lang::calculator::{interpreter::Interpreter, parser};

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = std::env::args().collect();

    let cmd = &args[1];
    let filename = &args[2];

    if cmd == "run" {
        let source = std::fs::read_to_string(filename)?;
        let program = parser::parse(source.as_str());
        let mut interpreter = Interpreter::new();
        let result = interpreter.call_main(program);
        Ok(println!("{}", result))
    } else {
        Ok(eprintln!("Example: toy run ./source/factorial.toy"))
    }
}
