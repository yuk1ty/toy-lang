use anyhow::{anyhow, Result};
use toy_lang::{interpreter::Interpreter, parser};

fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().collect();

    let cmd = &args[1];
    let filename = &args[2];

    if cmd == "run" {
        let source = std::fs::read_to_string(filename)?;
        let program = parser::parse(source.as_str())?;
        let mut interpreter = Interpreter::new();
        let _ = interpreter.call_main(program)?;
        Ok(())
    } else {
        Err(anyhow!("Example: toy run ./source/factorial.toy"))
    }
}
