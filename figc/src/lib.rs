pub mod codegen;
pub mod lexer;
pub mod parser;
pub mod types;

use wasm_bindgen::prelude::*;

use codegen::codegen::Context;
use lexer::lexer::Lexer;
use parser::ast::Program;
use parser::parser::Parser;

#[wasm_bindgen]
pub fn wasm_main(src: &str, starting_offset: i32) -> Vec<u8> {
    let mut lexer = Lexer::new(src.to_string());
    let mut parser = Parser::new(&mut lexer);
    let program = Program::parse(&mut parser, None);

    let mut ctx = Context::new(program, starting_offset);
    ctx.bootstrap();
    ctx.visit().unwrap();

    let buf = ctx.generate();

    buf
}
