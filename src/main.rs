mod codegen;
mod lexer;
mod parser;
mod types;

use std::env::args;
use std::fs::File;
use std::io;
use std::io::{Read, Write};
use std::path::PathBuf;

use lexer::lexer::Lexer;
use parser::ast::Program;
use parser::parser::{Parse, Parser};

use crate::codegen::codegen::Generator;

fn read_source_file(file_path: PathBuf) -> io::Result<String> {
    let mut file = File::open(file_path)?;
    let mut buf = String::new();
    file.read_to_string(&mut buf)?;

    Ok(buf)
}

fn main() {
    let mut args = args().into_iter();
    args.next();

    let source_file_path = args.next();

    let source = read_source_file(PathBuf::from(source_file_path.unwrap())).unwrap();

    let mut lexer = Lexer::new(source);
    let mut parser = Parser::new(&mut lexer);
    let program = Program::parse(&mut parser, None).unwrap();

    let mut generator = Generator::new(program);

    generator.visit().unwrap();

    let buf = generator.generate();

    let mut file = File::create("out.wasm").unwrap();
    file.write(&buf).unwrap();
}
