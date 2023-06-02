use super::token::Token;

pub struct Lexer {
    pub source: String,
    pub index: u32,
}

impl Lexer {
    pub fn new(source: String) -> Self {
        Self { source, index: 0 }
    }

    /// Returns the Some if the ident is reserved keyword
    fn get_keyword(input: &String) -> Option<Token> {
        match input.as_ref() {
            "fn" => Some(Token::Function),
            "let" => Some(Token::Let),
            "true" => Some(Token::True),
            "false" => Some(Token::False),

            _ => None,
        }
    }

    pub fn lex(&self) -> Vec<Token> {
        use Token::*;

        let mut chars = self.source.chars().peekable();
        let mut result = vec![];

        while let Some(c) = chars.peek() {
            match c {
                ' ' => {
                    chars.next();
                }

                '{' => {
                    chars.next();
                    result.push(LSquirly);
                }

                '}' => {
                    chars.next();
                    result.push(RSquirly);
                }

                '(' => {
                    chars.next();
                    result.push(Lparen);
                }

                ')' => {
                    chars.next();
                    result.push(Rparen);
                }

                '=' => {
                    chars.next();

                    let next_char = chars.peek().unwrap();

                    if next_char == &'=' {
                        result.push(EqualEqual);
                    } else {
                        result.push(Equal);
                    }
                }

                '!' => {
                    chars.next();

                    let next_char = chars.peek().unwrap();

                    if next_char == &'=' {
                        result.push(NotEqual);
                    } else {
                        result.push(Not);
                    }
                }

                ';' => {
                    chars.next();

                    result.push(Semicolon);
                }

                ',' => {
                    chars.next();

                    result.push(Comma);
                }

                '+' => {
                    chars.next();

                    result.push(Plus);
                }

                'a'..='z' | 'A'..='Z' | '_' => {
                    let mut ident = String::new();
                    while let Some(c) = chars.next() {
                        if !c.is_ascii_alphabetic() {
                            break;
                        }

                        ident.push(c.to_owned());
                    }

                    if let Some(keyword) = Self::get_keyword(&ident) {
                        result.push(keyword);
                    } else {
                        result.push(Ident(ident));
                    }
                }

                '0'..='9' => {
                    let mut number = String::new();
                    while let Some(c) = chars.next() {
                        if !c.is_ascii_digit() {
                            break;
                        }

                        number.push(c.to_owned());
                    }

                    result.push(Int(number));
                }

                _ => result.push(Illegal),
            }
        }

        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexer_test() {
        use Token::*;

        let source = String::from("true {}() helloWorld 1234 ; ,+ false , fn");

        let wanted_tokens = vec![
            True,
            LSquirly,
            RSquirly,
            Lparen,
            Rparen,
            Ident("helloWorld".to_string()),
            Int("1234".to_string()),
            Semicolon,
            Comma,
            Plus,
            False,
            Comma,
            Function,
        ];
        let lexer = Lexer::new(source);
        let tokens = lexer.lex();

        assert_eq!(tokens, wanted_tokens);
    }
}
