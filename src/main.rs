mod lexer;
use lexer::lexer::Lexer;

fn main() {
    let lexer = Lexer::new("".to_owned());
    println!("{:?}", lexer.lex());
}
