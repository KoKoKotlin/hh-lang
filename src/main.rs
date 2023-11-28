use std::{fs::File, io::Read};

use parser::Parser;
use preprocessor::preprocess;

use crate::interpreter::interprete_ast;

mod tokenizer;
mod parser;
mod ast;
mod interpreter;
mod preprocessor;

const SOURCE_FILE_PATH: &'static str = "main.hhl";

fn load_source_code_file(path: &str) -> std::io::Result<String> {
    let mut file = File::open(path)?;
    let mut buf = String::new();
    file.read_to_string(&mut buf)?;

    Ok(buf)
}

fn main() {
    let mut source_code = load_source_code_file(SOURCE_FILE_PATH).unwrap();
    // iterative because of import macro
    while source_code.contains('$') {
        source_code = match preprocess(&source_code) {
            Ok(s) => s,
            Err(err) => {
                println!("{:?}", err);
                return;
            },
        };
    };

    if false {
        println!("{}", source_code);
    }

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
