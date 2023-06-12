mod codegen;
mod lexer;
mod parser;

use std::fs::File;
use std::io::Write;

use lexer::lexer::Lexer;
use parser::ast::Program;
use parser::parser::{Parse, Parser};

use crate::codegen::codegen::Generator;

fn main() {
    let source = r#"
        
        "#;

    let mut lexer = Lexer::new(source.to_string());
    let mut parser = Parser::new(&mut lexer);
    let program = Program::parse(&mut parser, None).unwrap();

    let mut generator = Generator::new(program);

    generator.visit();

    let buf = generator.generate();

    let mut file = File::create("out.wasm").unwrap();
    file.write(&buf).unwrap();
}
