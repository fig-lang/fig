use florac::lexer::lexer::Lexer;
use florac::parser::ast::Program;
use florac::parser::parser::{Parse, Parser};
use std::env::args;
use std::fs::File;
use std::io;
use std::io::{Read, Write};
use std::path::PathBuf;

use crate::codegen::codegen::Context;

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

    let mut ctx = Context::new(program);
    ctx.bootstrap();
    ctx.visit().unwrap();

    let buf = ctx.generate();

    let output_path = args.next();

    let mut file = File::create(PathBuf::from(output_path.unwrap())).unwrap();
    file.write(&buf).unwrap();
}