use super::parser::{PResult, Parse, Parser, ParserError, Precedence};
use crate::lexer::token::Token;
use crate::types::types::Type;

impl<'a> Parse<'a> for Type {
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> PResult<Self> {
        // skip ':' char
        parser.next_token();

        let type_ident = Identifier::parse(parser, precedence)?;

        let type_value = Type::from(type_ident.value.clone());

        if type_value == Type::NotDefined {
            return Err(ParserError::unexpected(type_ident.value));
        }

        Ok(type_value)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(Expression),
    Block(BlockStatement),
    Function(FunctionStatement),
    Export(ExportStatement),
    Loop(LoopStatement),
    Set(SetStatement),
    Break(BreakStatement),
    External(ExternalStatement),
    Builtin(BuiltinStatement),
}

impl Statement {
    fn parse_expression<'a>(
        parser: &mut Parser<'a>,
        precedence: Option<Precedence>,
    ) -> PResult<Self> {
        let expr = Self::Expression(Expression::parse(parser, precedence)?);

        if parser.next_token_is(Token::Semicolon) {
            parser.next_token();
        }

        Ok(expr)
    }
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
            Token::Export => Ok(Self::Export(ExportStatement::parse(parser, precedence)?)),
            Token::Loop => Ok(Self::Loop(LoopStatement::parse(parser, precedence)?)),
            Token::Break => Ok(Self::Break(BreakStatement::parse(parser, None)?)),
            Token::External => Ok(Self::External(ExternalStatement::parse(parser, None)?)),
            Token::Builtin => Ok(Self::Builtin(BuiltinStatement::parse(parser, None)?)),
            Token::Ident(_) => {
                if parser.next_token_is(Token::Assign) || parser.next_token_is(Token::LBrack) {
                    Ok(Self::Set(SetStatement::parse(parser, precedence)?))
                } else {
                    Self::parse_expression(parser, Some(Precedence::Lowest))
                }
            }

            _el => Self::parse_expression(parser, Some(Precedence::Lowest)),
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
    pub(crate) value_type: Type,
    pub(crate) name: Identifier,
    pub(crate) value: Expression,
}

impl<'a> Parse<'a> for LetStatement {
    // TODO: make the type optional
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> PResult<Self> {
        // pass the Let token
        parser.next_token();

        let ident = Identifier::parse(parser, precedence.clone())?;

        parser.next_token();

        let value_type = Type::parse(parser, precedence)?;

        parser.expect_peek(Token::Assign)?;
        parser.next_token();

        let let_value = Expression::parse(parser, Some(Precedence::Lowest))?;

        if parser.next_token_is(Token::Semicolon) {
            parser.next_token();
        }

        Ok(LetStatement {
            value_type,
            name: ident,
            value: let_value,
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ReturnStatement {
    pub(crate) return_value: Expression,
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
pub struct ExportStatement {
    pub(crate) value: Box<Statement>,
}

impl<'a> Parse<'a> for ExportStatement {
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> PResult<Self> {
        // Get the next value
        parser.next_token();

        // We assume the next token is function
        // We just support function export for now
        let function = FunctionStatement::parse(parser, precedence)?;

        Ok(ExportStatement {
            value: Box::new(Statement::Function(function)),
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LoopStatement {
    pub(crate) block: BlockStatement,
}

impl<'a> Parse<'a> for LoopStatement {
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> PResult<Self> {
        // skip loop token
        parser.next_token();

        // parse block
        let block = BlockStatement::parse(parser, precedence)?;

        Ok(LoopStatement { block })
    }
}

// for example
// let x = 1;
//
// This is the set statement
// x = 5;
//
// Also we have to provide the variable (memory) sets
// for example
// let y = malloc(5);
// y[0] = 4;
// y[1] = 3;
// y[2] = 11;
// y[10] = 50; // oops, overflow
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SetStatement {
    pub(crate) variable: Expression,
    pub(crate) expression: Expression,
}

impl<'a> Parse<'a> for SetStatement {
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> PResult<Self> {
        // so we want an left_expr
        // the left_expr must be an identifier which is the variable name
        let variable = Expression::parse(parser, None)?;

        if !(matches!(variable, Expression::Identifier(_) | Expression::Index(_))) {
            return Err(ParserError::expected(
                format!("variable name or index"),
                format!("{:?}", variable),
            ));
        }

        parser.expect_peek(Token::Assign)?;

        parser.next_token();

        let expression = Expression::parse(parser, precedence)?;

        parser.expect_peek(Token::Semicolon)?;

        Ok(SetStatement {
            variable,
            expression,
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BreakStatement;

impl<'a> Parse<'a> for BreakStatement {
    fn parse(parser: &mut Parser<'a>, _precedence: Option<Precedence>) -> PResult<Self> {
        parser.expect_peek(Token::Semicolon)?;
        Ok(BreakStatement)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExternalBody {
    pub(crate) function_types: Vec<FunctionMeta>,
}

impl<'a> Parse<'a> for ExternalBody {
    fn parse(parser: &mut Parser<'a>, _precedence: Option<Precedence>) -> PResult<Self> {
        parser.next_token();
        let mut function_types = vec![];
        while !parser.current_token_is(Token::RSquirly) && !parser.current_token_is(Token::Eof) {
            let fn_type = FunctionMeta::parse(parser, _precedence.clone())?;

            function_types.push(fn_type);

            parser.expect_peek(Token::Semicolon)?;
            parser.next_token();
        }

        Ok(ExternalBody { function_types })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ExternalStatement {
    pub(crate) module: Identifier,
    pub(crate) body: ExternalBody,
}

impl<'a> Parse<'a> for ExternalStatement {
    fn parse(parser: &mut Parser<'a>, _precedence: Option<Precedence>) -> PResult<Self> {
        // skip external token
        parser.next_token();

        // Get the module name
        let module = Identifier::parse(parser, None)?;

        parser.expect_peek(Token::LSquirly)?;

        let body = ExternalBody::parse(parser, None)?;

        //parser.expect_peek(Token::RSquirly)?;

        Ok(ExternalStatement { module, body })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BuiltinStatement {
    pub(crate) function_meta: FunctionMeta,
}

impl<'a> Parse<'a> for BuiltinStatement {
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> PResult<Self> {
        // skip builtin token
        parser.next_token();

        let function_meta = FunctionMeta::parse(parser, precedence)?;

        parser.expect_peek(Token::Semicolon)?;

        Ok(BuiltinStatement { function_meta })
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
    Call(CallExpr),
    String(StringExpr),
    Index(IndexExpr),
    Ref(RefValue),
    DeRef(DeRef),
}

impl<'a> Parse<'a> for Expression {
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> PResult<Self> {
        let mut left_expr = match &parser.current_token {
            Token::Int(_) => Expression::Integer(Integer::parse(parser, precedence.clone())?),

            //Token::LBrack => Expression::Index(IndexExpr::parse(parser, precedence.clone())?),
            Token::Ref => Expression::Ref(RefValue::parse(parser, precedence.clone())?),
            
            // BUG: Determine the *pointer with 1 * x
            // Or maybe we can use deref kind of keyword
            Token::Asterisk => Expression::DeRef(DeRef::parse(parser, precedence.clone())?),

            Token::Ident(_) => {
                if parser.next_token_is(Token::LBrack) {
                    Expression::Index(IndexExpr::parse(parser, precedence.clone())?)
                    //Expression::parse(parser, Some(Precedence::Lowest))?
                } else {
                    Expression::Identifier(Identifier::parse(parser, precedence.clone())?)
                }
            }

            Token::Minus | Token::Bang => {
                Expression::Prefix(PrefixExpr::parse(parser, precedence.clone())?)
            }

            Token::True | Token::False => {
                Expression::Boolean(BooleanExpr::parse(parser, precedence.clone())?)
            }

            Token::If => Expression::If(IfExpr::parse(parser, precedence.clone())?),

            Token::String(_) => Expression::String(StringExpr::parse(parser, None)?),

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
            | Token::Mod
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
                let fn_name = match left_expr {
                    Self::Identifier(ident) => Ok(ident),
                    _ => Err(ParserError::expected(
                        "function name".to_string(),
                        "Expression".to_string(),
                    )),
                }?;
                left_expr = Expression::Call(CallExpr::parse(parser, fn_name)?);
                break;
            }

            parser.next_token();
            left_expr = Expression::Infix(InfixExpr::parse(parser, left_expr)?);
        }

        Ok(left_expr)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StringExpr {
    pub(crate) string: String,
}

impl<'a> Parse<'a> for StringExpr {
    fn parse(parser: &mut Parser<'a>, _precedence: Option<Precedence>) -> PResult<Self> {
        match &parser.current_token {
            Token::String(s) => Ok(Self {
                string: s.to_owned(),
            }),
            _ => unreachable!(),
        }
    }
}

// TODO: what am i going to do with this ?
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RefType {
    pub(crate) ty: Type,
}

impl<'a> Parse<'a> for RefType {
    fn parse(parser: &mut Parser<'a>, _precedence: Option<Precedence>) -> PResult<Self> {
        // Skip the & char
        parser.next_token();

        // now parse the ref type
        let ty = Type::parse(parser, _precedence)?;

        Ok(Self { ty })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RefValue {
    pub(crate) value: Box<Expression>,
}

impl<'a> Parse<'a> for RefValue {
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> PResult<Self> {
        // Skip the & char
        parser.next_token();

        // now parse the ref type
        let value = Expression::parse(parser, precedence)?;

        Ok(Self {
            value: Box::new(value),
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DeRef {
    pub(crate) ident: Identifier,
}

impl<'a> Parse<'a> for DeRef {
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> PResult<Self> {
        // Skip the * char
        parser.next_token();

        // now parse the ref type
        let ident = Identifier::parse(parser, precedence)?;

        Ok(Self { ident })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IndexExpr {
    pub(crate) variable: Identifier,
    pub(crate) index: Box<Expression>,
}

impl<'a> Parse<'a> for IndexExpr {
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> PResult<Self> {
        let ident = Identifier::parse(parser, None)?;

        parser.expect_peek(Token::LBrack)?;
        parser.next_token();

        let index = Expression::parse(parser, precedence)?;

        parser.expect_peek(Token::RBrack)?;

        Ok(IndexExpr {
            variable: ident,
            index: Box::new(index),
        })
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
    pub(crate) condition: Box<Expression>,
    pub(crate) consequence: BlockStatement,
    pub(crate) alternative: Option<BlockStatement>,
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
pub struct FunctionMeta {
    pub(crate) name: Identifier,
    pub(crate) params: Vec<(Identifier, Type)>,
    pub(crate) return_type: Option<Type>,
}

impl FunctionMeta {
    fn parse_function_params<'a>(parser: &mut Parser<'a>) -> PResult<Vec<(Identifier, Type)>> {
        let mut params: Vec<(Identifier, Type)> = vec![];

        if parser.next_token_is(Token::Rparen) {
            parser.next_token();

            return Ok(params);
        }

        parser.next_token();

        let param = Identifier::parse(parser, None)?;

        parser.next_token();

        let param_type = Type::parse(parser, None)?;

        params.push((param, param_type));

        while parser.next_token_is(Token::Comma) {
            parser.next_token();
            parser.next_token();

            let param = Identifier::parse(parser, None)?;
            parser.next_token();
            let param_type = Type::parse(parser, None)?;

            params.push((param, param_type));
        }

        parser.expect_peek(Token::Rparen)?;

        Ok(params)
    }
}

impl<'a> Parse<'a> for FunctionMeta {
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> PResult<Self> {
        parser.next_token();

        let name = Identifier::parse(parser, precedence.clone())?;

        parser.expect_peek(Token::Lparen)?;

        let params = Self::parse_function_params(parser)?;

        let return_type = match parser.next_token.clone() {
            Token::Colon => {
                parser.next_token();
                Some(Type::parse(parser, None)?)
            }

            _ => None,
        };

        //parser.next_token();

        Ok(FunctionMeta {
            name,
            params,
            return_type,
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunctionStatement {
    pub(crate) meta: FunctionMeta,
    pub(crate) body: BlockStatement,
}

impl<'a> Parse<'a> for FunctionStatement {
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> PResult<Self> {
        let meta = FunctionMeta::parse(parser, None)?;

        parser.expect_peek(Token::LSquirly)?;

        let body = BlockStatement::parse(parser, precedence)?;

        Ok(FunctionStatement { meta, body })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CallExpr {
    pub(crate) function: Identifier, // Identifier or Function
    pub(crate) arguments: Vec<Expression>,
}

impl CallExpr {
    fn parse<'a>(parser: &mut Parser<'a>, function: Identifier) -> PResult<Self> {
        let args = Self::parse_args(parser)?;

        Ok(CallExpr {
            function,
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
    pub(crate) value: bool,
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

#[derive(Clone, Debug, PartialEq, Eq)]
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
            let x: i32 = 1 * 1;
            let z: i32 = !2;
            let y: i32 = 2 / 3;
            let t: i32 = 2 - 3;
            let b: string = "Hello World";
            "#;
        let mut lexer = Lexer::new(source.to_string());
        let mut parser = Parser::new(&mut lexer);
        let program = Program::parse(&mut parser, Some(Precedence::Lowest)).unwrap();

        let expected_statements = [
            Statement::Let(LetStatement {
                value_type: Type::I32,
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
                value_type: Type::I32,
                name: Identifier {
                    value: "z".to_string(),
                },
                value: Expression::Prefix(PrefixExpr {
                    operator: Token::Bang,
                    right: Box::new(Expression::Integer(Integer { value: 2 })),
                }),
            }),
            Statement::Let(LetStatement {
                value_type: Type::I32,
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
                value_type: Type::I32,
                name: Identifier {
                    value: "t".to_string(),
                },
                value: Expression::Infix(InfixExpr {
                    left: Box::new(Expression::Integer(Integer { value: 2 })),
                    operator: Token::Minus,
                    right: Box::new(Expression::Integer(Integer { value: 3 })),
                }),
            }),
            Statement::Let(LetStatement {
                value_type: Type::String,
                name: Identifier {
                    value: "b".to_string(),
                },
                value: Expression::String(StringExpr {
                    string: "Hello World".to_string(),
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
        let source = r#"let x: i32 = if (y == 1) {
            return y + 1; 
        } else {
            return 1;
        }"#;

        let mut lexer = Lexer::new(source.to_string());
        let mut parser = Parser::new(&mut lexer);
        let program = Program::parse(&mut parser, Some(Precedence::Lowest)).unwrap();

        // wtf is this ?
        let expected_statements = [Statement::Let(LetStatement {
            value_type: Type::I32,
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
        let source = r#"let x: i32 = if (y == 1) {
            return y + 1; 
        }"#;

        let mut lexer = Lexer::new(source.to_string());
        let mut parser = Parser::new(&mut lexer);
        let program = Program::parse(&mut parser, Some(Precedence::Lowest)).unwrap();

        // wtf is this ?
        let expected_statements = [Statement::Let(LetStatement {
            value_type: Type::I32,
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
        let source = r#"fn some(): i32 {
            1;
        }"#;

        let mut lexer = Lexer::new(source.to_string());
        let mut parser = Parser::new(&mut lexer);
        let program = Program::parse(&mut parser, Some(Precedence::Lowest)).unwrap();

        let expected_statements = [Statement::Function(FunctionStatement {
            meta: FunctionMeta {
                name: Identifier {
                    value: "some".to_string(),
                },
                params: vec![],
                return_type: Some(Type::I32),
            },
            body: BlockStatement {
                statements: vec![Statement::Expression(Expression::Integer(Integer {
                    value: 1,
                }))],
            },
        })];

        for i in 0..program.statements.len() {
            assert_eq!(program.statements[i], expected_statements[i]);
        }
    }

    #[test]
    fn test_function_expr_one_args() {
        let source = r#"fn some(x: i32) {}"#;

        let mut lexer = Lexer::new(source.to_string());
        let mut parser = Parser::new(&mut lexer);
        let program = Program::parse(&mut parser, Some(Precedence::Lowest)).unwrap();

        let expected_statements = [Statement::Function(FunctionStatement {
            meta: FunctionMeta {
                name: Identifier {
                    value: "some".to_string(),
                },
                params: vec![(
                    Identifier {
                        value: "x".to_string(),
                    },
                    Type::I32,
                )],
                return_type: None,
            },
            body: BlockStatement { statements: vec![] },
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
            function: Identifier {
                value: "hello".to_string(),
            },
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
            function: Identifier {
                value: "hello".to_string(),
            },
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

    #[test]
    fn test_expression_with_index() {
        let source = r#"let x: i32 = lhs[i] % 65536;"#;

        let mut lexer = Lexer::new(source.to_string());
        let mut parser = Parser::new(&mut lexer);
        let program = Program::parse(&mut parser, Some(Precedence::Lowest)).unwrap();

        let expected_statements = [Statement::Let(LetStatement {
            value_type: Type::I32,
            name: Identifier {
                value: "x".to_string(),
            },
            value: Expression::Infix(InfixExpr {
                left: Box::new(Expression::Index(IndexExpr {
                    variable: Identifier {
                        value: "lhs".to_string(),
                    },
                    index: Box::new(Expression::Identifier(Identifier {
                        value: "i".to_string(),
                    })),
                })),
                operator: Token::Mod,
                right: Box::new(Expression::Integer(Integer { value: 65536 })),
            }),
        })];

        for i in 0..program.statements.len() {
            assert_eq!(program.statements[i], expected_statements[i]);
        }
    }
}
