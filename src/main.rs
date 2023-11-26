use std::{fs::File, io::Read};

use parser::Parser;

use crate::interpreter::interprete_ast;

mod tokenizer;
mod parser;
mod ast;
mod interpreter;

const SOURCE_FILE_PATH: &'static str = "stdlib.hhl";

fn load_source_code_file(path: &str) -> std::io::Result<String> {
    let mut file = File::open(path)?;
    let mut buf = String::new();
    file.read_to_string(&mut buf)?;

    Ok(buf)
}

fn main() {
    let source_code = load_source_code_file(SOURCE_FILE_PATH).unwrap();
    if false {
        let mut tokenizer = tokenizer::Tokenizer::new(source_code.clone());
        while let Some(token) = tokenizer.next_token() {
            println!("{:?}", token);
        }    
    }

    let mut parser = Parser::new(&source_code);
    let ast_root = parser.parse();

    if let Ok(ast_root) = ast_root {
        let (err, context) = interprete_ast(ast_root);
        match &err {
            Err(err) => println!("{}", err.info()),
            _ => {}
        }
    }
}
