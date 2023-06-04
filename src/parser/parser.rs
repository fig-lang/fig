use crate::lexer::token;
pub struct Parser {
    tokens: Vec<token::Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<token::Token>) -> Self {
        Self { tokens, current: 0 }
    }
}

pub enum ParserError {
    Expected(String),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::token;

    #[test]
    fn test_parser() {
        use token::Token::*;

        let tokens = vec![Let, Ident("Hello".to_string()), Equal];
        let parser = Parser::new(tokens);
    }
}
