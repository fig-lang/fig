mod lexer;
mod parser;
use lexer::lexer::Lexer;
use parser::ast::Program;
use parser::parser::{Parse, Parser, Precedence};

fn main() {
    let source = r#"10 + 10;"#;

    let mut lexer = Lexer::new(source.to_string());
    let mut parser = Parser::new(&mut lexer);
    let program = Program::parse(&mut parser, None).unwrap();
    println!("{:?}", program);
}
