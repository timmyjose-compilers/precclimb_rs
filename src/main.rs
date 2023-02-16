use std::{
    error::Error,
    io::{self, Write},
};

use precclimb_rs::{lexer::Lexer, parser::Parser};

const PROMPT: &'static str = ">> ";

fn get_input() -> Result<String, Box<dyn Error>> {
    let mut input = String::new();
    io::stdin().read_line(&mut input)?;
    Ok(input.trim().to_owned())
}

fn main() -> Result<(), Box<dyn Error>> {
    loop {
        print!("{PROMPT}");
        io::stdout().lock().flush()?;

        let input = get_input()?;
        let mut parser = Parser::new(Lexer::new(&input));
        println!("{:#?}", parser.parse()?);
    }
}
