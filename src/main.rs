use std::{fs::File, io::Read};

use clap::Parser;
use preprocessor::preprocess;

use crate::interpreter::interprete_ast;

mod tokenizer;
mod parser;
mod ast;
mod interpreter;
mod preprocessor;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(short, long)]
    tokens: bool,
    #[arg(short, long)]
    preprocessor: bool,
    #[arg(short, long)]
    context: bool,

    file: String,
}


fn load_source_code_file(path: &str) -> std::io::Result<String> {
    let mut file = File::open(path)?;
    let mut buf = String::new();
    file.read_to_string(&mut buf)?;

    Ok(buf)
}

fn main() {
    let args = Args::parse();

    let mut source_code = load_source_code_file(&args.file).unwrap();
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

    if args.preprocessor {
        println!("{}", source_code);
    }

    if args.tokens {
        let mut tokenizer = tokenizer::Tokenizer::new(source_code.clone(), args.file.clone());
        while let Some(token) = tokenizer.next_token() {
            println!("{:?}", token);
        }    
    }

    let mut parser = parser::Parser::new(&source_code, args.file.clone());
    let ast_root = parser.parse();

    if let Ok(ast_root) = ast_root {
        let (err, context) = interprete_ast(ast_root);
        if args.context {
            println!("{:#?}", context);
        }
        match &err {
            Err(err) => println!("{}", err.info()),
            _ => {}
        }
    }
}
