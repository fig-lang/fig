use super::token::Token;

pub struct Lexer {
    pub source: String,
    pub index: u32,
}

impl Lexer {
    pub fn new(source: String) -> Self {
        Self { source, index: 0 }
    }

    fn lex(&self) -> Vec<Token> {
        let mut chars = self.source.chars().peekable();

        while let Some(c) = chars.peek() {
            match c {
                ' ' => {
                    chars.next();
                }

                'a'..='z' | '_' => {}

                _ => panic!(),
            }
        }

        todo!()
    }
}
