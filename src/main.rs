use std::{fs::File, io::Read};

use crate::tokenizer::Tokenizer;

mod tokenizer;
mod parser;
mod ast;

const SOURCE_FILE_PATH: &'static str = "main.hh";

fn load_source_code_file(path: &str) -> std::io::Result<String> {
    let mut file = File::open(path)?;
    let mut buf = String::new();
    file.read_to_string(&mut buf)?;

    Ok(buf)
}

fn main() {
    let source_code = load_source_code_file(SOURCE_FILE_PATH).unwrap();
    let mut tokenizer = Tokenizer::new(source_code.to_owned());
    
    while let Some(token) = tokenizer.next_token() {
        println!("{:?}", token);
    }
}
