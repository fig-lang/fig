use super::parser::{PResult, Parse, Parser, ParserError, Precedence};
use crate::lexer::token::Token;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(Expression),
    Block(BlockStatement),
    Function(FunctionStatement),
}

impl<'a> Parse<'a> for Statement {
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> PResult<Self> {
        match &parser.current_token {
            Token::Let => Ok(Self::Let(LetStatement::parse(parser, precedence)?)),
            Token::Return => Ok(Self::Return(ReturnStatement::parse(parser, precedence)?)),
            Token::LSquirly => Ok(Self::Block(BlockStatement::parse(parser, precedence)?)),
            Token::Function => Ok(Self::Function(FunctionStatement::parse(
                parser, precedence,
            )?)),

            _el => {
                let expr = Self::Expression(Expression::parse(parser, Some(Precedence::Lowest))?);

                if parser.next_token_is(Token::Semicolon) {
                    parser.next_token();
                }
                Ok(expr)
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BlockStatement {
    pub(crate) statements: Vec<Statement>,
}

impl<'a> Parse<'a> for BlockStatement {
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> PResult<Self> {
        parser.next_token();

        let mut statements: Vec<Statement> = vec![];

        while !parser.current_token_is(Token::RSquirly) && !parser.current_token_is(Token::Eof) {
            let statement = Statement::parse(parser, precedence.clone())?;

            statements.push(statement);

            parser.next_token();
        }

        Ok(BlockStatement { statements })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LetStatement {
    pub(crate) name: Identifier,
    pub(crate) value: Expression,
}

impl<'a> Parse<'a> for LetStatement {
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> PResult<Self> {
        // pass the Let token
        parser.next_token();

        let ident = Identifier::parse(parser, precedence.clone())?;

        parser.expect_peek(Token::Assign)?;

        parser.next_token();

        let let_value = Expression::parse(parser, Some(Precedence::Lowest))?;

        if parser.next_token_is(Token::Semicolon) {
            parser.next_token();
        }

        Ok(LetStatement {
            name: ident,
            value: let_value,
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ReturnStatement {
    return_value: Expression,
}

impl<'a> Parse<'a> for ReturnStatement {
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> PResult<Self> {
        parser.next_token();

        let return_value = Expression::parse(parser, precedence)?;

        if parser.next_token_is(Token::Semicolon) {
            parser.next_token();
        }

        return Ok(ReturnStatement { return_value });
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expression {
    Identifier(Identifier),
    Integer(Integer),
    Prefix(PrefixExpr),
    Infix(InfixExpr),
    Boolean(BooleanExpr),
    If(IfExpr),
    Function(FunctionExpr),
    Call(CallExpr),
}

impl<'a> Parse<'a> for Expression {
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> PResult<Self> {
        let mut left_expr = match &parser.current_token {
            Token::Int(_) => Expression::Integer(Integer::parse(parser, precedence.clone())?),
            Token::Ident(_) => {
                Expression::Identifier(Identifier::parse(parser, precedence.clone())?)
            }
            Token::Minus | Token::Bang => {
                Expression::Prefix(PrefixExpr::parse(parser, precedence.clone())?)
            }

            Token::True | Token::False => {
                Expression::Boolean(BooleanExpr::parse(parser, precedence.clone())?)
            }

            Token::If => Expression::If(IfExpr::parse(parser, precedence.clone())?),
            Token::Function => {
                Expression::Function(FunctionExpr::parse(parser, precedence.clone())?)
            }

            // Parse grouped expressions
            Token::Lparen => {
                parser.next_token();
                let expression = Expression::parse(parser, Some(Precedence::Lowest))?;

                parser.expect_peek(Token::Rparen)?;

                expression
            }

            Token::Plus
            | Token::Minus
            | Token::Asterisk
            | Token::Equal
            | Token::NotEqual
            | Token::LessThan
            | Token::GreaterThan => {
                Expression::Prefix(PrefixExpr::parse(parser, precedence.clone())?)
            }

            tkn => panic!("{:?}", tkn),
        };

        let precedence = if let Some(p) = precedence {
            p
        } else {
            Precedence::Lowest
        };

        while let Some(p) = parser.next_precedence() {
            if precedence > p {
                break;
            }

            if parser.next_token_is(Token::Lparen) {
                parser.next_token();
                left_expr = Expression::Call(CallExpr::parse(parser, left_expr)?);
                break;
            }

            parser.next_token();
            left_expr = Expression::Infix(InfixExpr::parse(parser, left_expr)?);
        }

        Ok(left_expr)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PrefixExpr {
    operator: Token,
    right: Box<Expression>,
}

impl<'a> Parse<'a> for PrefixExpr {
    fn parse(parser: &mut Parser<'a>, _precedence: Option<Precedence>) -> PResult<Self> {
        let operator = parser.current_token.clone();

        parser.next_token();

        return Ok(PrefixExpr {
            operator,
            right: Box::new(Expression::parse(parser, Some(Precedence::Prefix))?),
        });
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct InfixExpr {
    pub(crate) left: Box<Expression>,
    pub(crate) operator: Token,
    pub(crate) right: Box<Expression>,
}

impl<'a> InfixExpr {
    fn parse(parser: &mut Parser<'a>, left_hand: Expression) -> PResult<Self> {
        let operator = parser.current_token.clone();

        let precedence = parser.current_precedence();
        parser.next_token();

        let right = Expression::parse(parser, precedence)?;

        Ok(InfixExpr {
            left: Box::new(left_hand),
            operator,
            right: Box::new(right),
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IfExpr {
    condition: Box<Expression>,
    consequence: BlockStatement,
    alternative: Option<BlockStatement>,
}

impl<'a> Parse<'a> for IfExpr {
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> PResult<Self> {
        parser.expect_peek(Token::Lparen)?;

        parser.next_token();

        let condition = Expression::parse(parser, Some(Precedence::Lowest))?;

        parser.expect_peek(Token::Rparen)?;
        parser.expect_peek(Token::LSquirly)?;

        let consequence = BlockStatement::parse(parser, precedence.clone())?;

        let alternative = if parser.next_token_is(Token::Else) {
            parser.next_token();
            parser.expect_peek(Token::LSquirly)?;

            Some(BlockStatement::parse(parser, precedence)?)
        } else {
            None
        };

        Ok(IfExpr {
            condition: Box::new(condition),
            consequence,
            alternative,
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunctionStatement {
    pub(crate) name: Identifier,
    pub(crate) params: Vec<Identifier>,
    pub(crate) body: BlockStatement,
}

impl<'a> Parse<'a> for FunctionStatement {
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> PResult<Self> {
        parser.next_token();

        let name = Identifier::parse(parser, precedence.clone())?;

        parser.expect_peek(Token::Lparen)?;

        let params = FunctionExpr::parse_function_params(parser)?;

        parser.expect_peek(Token::LSquirly)?;

        let body = BlockStatement::parse(parser, precedence)?;

        Ok(FunctionStatement { name, params, body })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunctionExpr {
    params: Vec<Identifier>,
    body: BlockStatement,
}

impl FunctionExpr {
    fn parse_function_params<'a>(parser: &mut Parser<'a>) -> PResult<Vec<Identifier>> {
        let mut params: Vec<Identifier> = vec![];

        if parser.next_token_is(Token::Rparen) {
            parser.next_token();

            return Ok(params);
        }

        parser.next_token();

        let param = Identifier::parse(parser, None)?;

        params.push(param);

        while parser.next_token_is(Token::Comma) {
            parser.next_token();
            parser.next_token();

            let param = Identifier::parse(parser, None)?;

            params.push(param);
        }

        parser.expect_peek(Token::Rparen)?;

        Ok(params)
    }
}

impl<'a> Parse<'a> for FunctionExpr {
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> PResult<Self> {
        parser.expect_peek(Token::Lparen)?;

        let params = Self::parse_function_params(parser)?;

        parser.expect_peek(Token::LSquirly)?;

        let body = BlockStatement::parse(parser, precedence)?;

        Ok(FunctionExpr { params, body })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CallExpr {
    pub(crate) function: Box<Expression>, // Identifier or Function
    pub(crate) arguments: Vec<Expression>,
}

impl CallExpr {
    fn parse<'a>(parser: &mut Parser<'a>, function: Expression) -> PResult<Self> {
        let args = Self::parse_args(parser)?;

        Ok(CallExpr {
            function: Box::new(function),
            arguments: args,
        })
    }

    fn parse_args<'a>(parser: &mut Parser<'a>) -> PResult<Vec<Expression>> {
        let mut args: Vec<Expression> = vec![];
        if parser.next_token_is(Token::Rparen) {
            parser.next_token();
            return Ok(args);
        }

        parser.next_token();

        let arg = Expression::parse(parser, Some(Precedence::Lowest))?;

        args.push(arg);

        while parser.next_token_is(Token::Comma) {
            parser.next_token();
            parser.next_token();

            let arg = Expression::parse(parser, Some(Precedence::Lowest))?;

            args.push(arg);
        }

        parser.expect_peek(Token::Rparen)?;

        Ok(args)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BooleanExpr {
    value: bool,
}

impl<'a> Parse<'a> for BooleanExpr {
    fn parse(parser: &mut Parser<'a>, _precedence: Option<Precedence>) -> PResult<Self> {
        let boolean = match parser.current_token {
            Token::True => true,
            Token::False => false,

            _ => unreachable!(),
        };

        return Ok(BooleanExpr { value: boolean });
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Integer {
    pub(crate) value: i32,
}

impl<'a> Parse<'a> for Integer {
    fn parse(parser: &mut Parser<'a>, _precedence: Option<Precedence>) -> PResult<Self> {
        match &parser.current_token {
            Token::Int(int) => Ok(Integer {
                value: int.parse::<i32>().unwrap(),
            }),

            el => Err(ParserError::expected("Integer".to_owned(), el.to_string())),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Identifier {
    pub(crate) value: String,
}

impl<'a> Parse<'a> for Identifier {
    fn parse(parser: &mut Parser<'a>, _precedence: Option<Precedence>) -> PResult<Self> {
        match &parser.current_token {
            Token::Ident(i) => Ok(Identifier {
                value: i.to_string(),
            }),

            el => Err(ParserError::expected(
                "Identifier".to_string(),
                el.to_string(),
            )),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Program {
    pub(crate) statements: Vec<Statement>,
}

impl<'a> Parse<'a> for Program {
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> PResult<Self> {
        let mut statements: Vec<Statement> = vec![];

        while !parser.current_token_is(Token::Eof) {
            let statement = Statement::parse(parser, precedence.clone())?;
            statements.push(statement);
            parser.next_token();
        }

        Ok(Self { statements })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parser::Precedence;
    use crate::Lexer;
    #[test]
    fn test_let() {
        let source = r#"
            let x = 1 * 1;
            let z = !2;
            let y = 2 / 3;
            let t = 2 - 3;
            "#;
        let mut lexer = Lexer::new(source.to_string());
        let mut parser = Parser::new(&mut lexer);
        let program = Program::parse(&mut parser, Some(Precedence::Lowest)).unwrap();

        let expected_statements = [
            Statement::Let(LetStatement {
                name: Identifier {
                    value: "x".to_string(),
                },
                value: Expression::Infix(InfixExpr {
                    left: Box::new(Expression::Integer(Integer { value: 1 })),
                    operator: Token::Asterisk,
                    right: Box::new(Expression::Integer(Integer { value: 1 })),
                }),
            }),
            Statement::Let(LetStatement {
                name: Identifier {
                    value: "z".to_string(),
                },
                value: Expression::Prefix(PrefixExpr {
                    operator: Token::Bang,
                    right: Box::new(Expression::Integer(Integer { value: 2 })),
                }),
            }),
            Statement::Let(LetStatement {
                name: Identifier {
                    value: "y".to_string(),
                },
                value: Expression::Infix(InfixExpr {
                    left: Box::new(Expression::Integer(Integer { value: 2 })),
                    operator: Token::ForwardSlash,
                    right: Box::new(Expression::Integer(Integer { value: 3 })),
                }),
            }),
            Statement::Let(LetStatement {
                name: Identifier {
                    value: "t".to_string(),
                },
                value: Expression::Infix(InfixExpr {
                    left: Box::new(Expression::Integer(Integer { value: 2 })),
                    operator: Token::Minus,
                    right: Box::new(Expression::Integer(Integer { value: 3 })),
                }),
            }),
        ];

        for i in 0..program.statements.len() {
            assert_eq!(program.statements[i], expected_statements[i]);
        }
    }

    #[test]
    fn test_return() {
        let source = r#"
            return !1;
            return 1 + 2;
            "#;
        let mut lexer = Lexer::new(source.to_string());
        let mut parser = Parser::new(&mut lexer);
        let program = Program::parse(&mut parser, None).unwrap();

        let expected_statements = [
            Statement::Return(ReturnStatement {
                return_value: Expression::Prefix(PrefixExpr {
                    operator: Token::Bang,
                    right: Box::new(Expression::Integer(Integer { value: 1 })),
                }),
            }),
            Statement::Return(ReturnStatement {
                return_value: Expression::Infix(InfixExpr {
                    left: Box::new(Expression::Integer(Integer { value: 1 })),
                    operator: Token::Plus,
                    right: Box::new(Expression::Integer(Integer { value: 2 })),
                }),
            }),
        ];

        for i in 0..program.statements.len() {
            assert_eq!(program.statements[i], expected_statements[i]);
        }
    }

    #[test]
    fn test_expression() {
        let source = r#"ident;1;!5;-10;10 * 10;true == false"#;

        let mut lexer = Lexer::new(source.to_string());
        let mut parser = Parser::new(&mut lexer);
        let program = Program::parse(&mut parser, Some(Precedence::Lowest)).unwrap();

        let expected_statements = [
            Statement::Expression(Expression::Identifier(Identifier {
                value: "ident".to_string(),
            })),
            Statement::Expression(Expression::Integer(Integer { value: 1 })),
            Statement::Expression(Expression::Prefix(PrefixExpr {
                operator: Token::Bang,
                right: Box::new(Expression::Integer(Integer { value: 5 })),
            })),
            Statement::Expression(Expression::Prefix(PrefixExpr {
                operator: Token::Minus,
                right: Box::new(Expression::Integer(Integer { value: 10 })),
            })),
            Statement::Expression(Expression::Infix(InfixExpr {
                left: Box::new(Expression::Integer(Integer { value: 10 })),
                operator: Token::Asterisk,
                right: Box::new(Expression::Integer(Integer { value: 10 })),
            })),
            Statement::Expression(Expression::Infix(InfixExpr {
                left: Box::new(Expression::Boolean(BooleanExpr { value: true })),
                operator: Token::Equal,
                right: Box::new(Expression::Boolean(BooleanExpr { value: false })),
            })),
        ];

        for i in 0..program.statements.len() {
            assert_eq!(program.statements[i], expected_statements[i]);
        }
    }

    #[test]
    fn test_if_statement_with_else() {
        // very usefull code
        let source = r#"let x = if (y == 1) {
            return y + 1; 
        } else {
            return 1;
        }"#;

        let mut lexer = Lexer::new(source.to_string());
        let mut parser = Parser::new(&mut lexer);
        let program = Program::parse(&mut parser, Some(Precedence::Lowest)).unwrap();

        // wtf is this ?
        let expected_statements = [Statement::Let(LetStatement {
            name: Identifier {
                value: "x".to_string(),
            },
            value: Expression::If(IfExpr {
                condition: Box::new(Expression::Infix(InfixExpr {
                    left: Box::new(Expression::Identifier(Identifier {
                        value: "y".to_string(),
                    })),
                    operator: Token::Equal,
                    right: Box::new(Expression::Integer(Integer { value: 1 })),
                })),
                consequence: BlockStatement {
                    statements: vec![Statement::Return(ReturnStatement {
                        return_value: Expression::Infix(InfixExpr {
                            left: Box::new(Expression::Identifier(Identifier {
                                value: "y".to_string(),
                            })),
                            operator: Token::Plus,
                            right: Box::new(Expression::Integer(Integer { value: 1 })),
                        }),
                    })],
                },
                alternative: Some(BlockStatement {
                    statements: vec![Statement::Return(ReturnStatement {
                        return_value: Expression::Integer(Integer { value: 1 }),
                    })],
                }),
            }),
        })];

        for i in 0..program.statements.len() {
            assert_eq!(program.statements[i], expected_statements[i]);
        }
    }

    #[test]
    fn test_if_statement_without_else() {
        // very usefull code
        let source = r#"let x = if (y == 1) {
            return y + 1; 
        }"#;

        let mut lexer = Lexer::new(source.to_string());
        let mut parser = Parser::new(&mut lexer);
        let program = Program::parse(&mut parser, Some(Precedence::Lowest)).unwrap();

        // wtf is this ?
        let expected_statements = [Statement::Let(LetStatement {
            name: Identifier {
                value: "x".to_string(),
            },
            value: Expression::If(IfExpr {
                condition: Box::new(Expression::Infix(InfixExpr {
                    left: Box::new(Expression::Identifier(Identifier {
                        value: "y".to_string(),
                    })),
                    operator: Token::Equal,
                    right: Box::new(Expression::Integer(Integer { value: 1 })),
                })),
                consequence: BlockStatement {
                    statements: vec![Statement::Return(ReturnStatement {
                        return_value: Expression::Infix(InfixExpr {
                            left: Box::new(Expression::Identifier(Identifier {
                                value: "y".to_string(),
                            })),
                            operator: Token::Plus,
                            right: Box::new(Expression::Integer(Integer { value: 1 })),
                        }),
                    })],
                },
                alternative: None,
            }),
        })];

        for i in 0..program.statements.len() {
            assert_eq!(program.statements[i], expected_statements[i]);
        }
    }

    #[test]
    fn test_grouped_expression() {
        let source = r#"1 + (2 + 3)"#;

        let mut lexer = Lexer::new(source.to_string());
        let mut parser = Parser::new(&mut lexer);
        let program = Program::parse(&mut parser, Some(Precedence::Lowest)).unwrap();

        let expected_statements = [Statement::Expression(Expression::Infix(InfixExpr {
            left: Box::new(Expression::Integer(Integer { value: 1 })),
            operator: Token::Plus,
            right: Box::new(Expression::Infix(InfixExpr {
                left: Box::new(Expression::Integer(Integer { value: 2 })),
                operator: Token::Plus,
                right: Box::new(Expression::Integer(Integer { value: 3 })),
            })),
        }))];

        for i in 0..program.statements.len() {
            assert_eq!(program.statements[i], expected_statements[i]);
        }
    }

    #[test]
    fn test_function_expr_with_empty_args() {
        let source = r#"let func = fn() {
            1;
        }"#;

        let mut lexer = Lexer::new(source.to_string());
        let mut parser = Parser::new(&mut lexer);
        let program = Program::parse(&mut parser, Some(Precedence::Lowest)).unwrap();

        let expected_statements = [Statement::Let(LetStatement {
            name: Identifier {
                value: "func".to_string(),
            },
            value: Expression::Function(FunctionExpr {
                params: vec![],
                body: BlockStatement {
                    statements: vec![Statement::Expression(Expression::Integer(Integer {
                        value: 1,
                    }))],
                },
            }),
        })];

        for i in 0..program.statements.len() {
            assert_eq!(program.statements[i], expected_statements[i]);
        }
    }

    #[test]
    fn test_function_expr_one_args() {
        let source = r#"let func = fn(x) {
        1;
    }"#;

        let mut lexer = Lexer::new(source.to_string());
        let mut parser = Parser::new(&mut lexer);
        let program = Program::parse(&mut parser, Some(Precedence::Lowest)).unwrap();

        let expected_statements = [Statement::Let(LetStatement {
            name: Identifier {
                value: "func".to_string(),
            },
            value: Expression::Function(FunctionExpr {
                params: vec![Identifier {
                    value: "x".to_string(),
                }],
                body: BlockStatement {
                    statements: vec![Statement::Expression(Expression::Integer(Integer {
                        value: 1,
                    }))],
                },
            }),
        })];

        for i in 0..program.statements.len() {
            assert_eq!(program.statements[i], expected_statements[i]);
        }
    }

    #[test]
    fn test_function_expr_some_args() {
        let source = r#"let func = fn(x, y, z) {
        1;
    }"#;

        let mut lexer = Lexer::new(source.to_string());
        let mut parser = Parser::new(&mut lexer);
        let program = Program::parse(&mut parser, Some(Precedence::Lowest)).unwrap();

        let expected_statements = [Statement::Let(LetStatement {
            name: Identifier {
                value: "func".to_string(),
            },
            value: Expression::Function(FunctionExpr {
                params: vec![
                    Identifier {
                        value: "x".to_string(),
                    },
                    Identifier {
                        value: "y".to_string(),
                    },
                    Identifier {
                        value: "z".to_string(),
                    },
                ],
                body: BlockStatement {
                    statements: vec![Statement::Expression(Expression::Integer(Integer {
                        value: 1,
                    }))],
                },
            }),
        })];

        for i in 0..program.statements.len() {
            assert_eq!(program.statements[i], expected_statements[i]);
        }
    }

    #[test]
    fn test_function_call() {
        let source = r#"hello();"#;

        let mut lexer = Lexer::new(source.to_string());
        let mut parser = Parser::new(&mut lexer);
        let program = Program::parse(&mut parser, Some(Precedence::Lowest)).unwrap();

        let expected_statements = [Statement::Expression(Expression::Call(CallExpr {
            function: Box::new(Expression::Identifier(Identifier {
                value: "hello".to_string(),
            })),
            arguments: vec![],
        }))];

        for i in 0..program.statements.len() {
            assert_eq!(program.statements[i], expected_statements[i]);
        }
    }

    #[test]
    fn test_function_call_with_args() {
        let source = r#"hello(1, 2 * 2);"#;

        let mut lexer = Lexer::new(source.to_string());
        let mut parser = Parser::new(&mut lexer);
        let program = Program::parse(&mut parser, Some(Precedence::Lowest)).unwrap();

        let expected_statements = [Statement::Expression(Expression::Call(CallExpr {
            function: Box::new(Expression::Identifier(Identifier {
                value: "hello".to_string(),
            })),
            arguments: vec![
                Expression::Integer(Integer { value: 1 }),
                Expression::Infix(InfixExpr {
                    left: Box::new(Expression::Integer(Integer { value: 2 })),
                    operator: Token::Asterisk,
                    right: Box::new(Expression::Integer(Integer { value: 2 })),
                }),
            ],
        }))];

        for i in 0..program.statements.len() {
            assert_eq!(program.statements[i], expected_statements[i]);
        }
    }
}