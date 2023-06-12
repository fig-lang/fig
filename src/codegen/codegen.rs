use wasm_encoder::{Instruction, Module, TypeSection, ValType};

use crate::{
    lexer::token::Token,
    parser::ast::{
        BlockStatement, Expression, FunctionStatement, InfixExpr, Integer, Program, Statement,
    },
};

pub trait WasmTypes {
    type Output;
    fn types(&self) -> Self::Output
    where
        Self: Sized;
}

impl WasmTypes for FunctionStatement {
    type Output = (Vec<ValType>, Vec<ValType>);

    fn types(&self) -> Self::Output {
        let mut param_type: Vec<ValType> = vec![];

        for param in self.params {
            param_type.push(ValType::I32);
        }

        (param_type, vec![ValType::I32])
    }
}

pub trait Instructions<'a> {
    fn generate_instructions(&self, gen: &'a mut Generator) -> Vec<Instruction>
    where
        Self: Sized;
}

impl<'a> Instructions<'a> for Token {
    fn generate_instructions(&self, gen: &'a mut Generator) -> Vec<Instruction> {
        match self {
            Token::Plus => vec![Instruction::I32Add],
            Token::Minus => vec![Instruction::I32Sub],
            Token::ForwardSlash => vec![Instruction::I32DivS],
            Token::Asterisk => vec![Instruction::I32Mul],

            _ => todo!(),
        }
    }
}

impl<'a> Instructions<'a> for InfixExpr {
    fn generate_instructions(&self, gen: &'a mut Generator) -> Vec<Instruction> {
        let mut result: Vec<Instruction> = vec![];
        let left_side = self.left.generate_instructions(gen);
        let operation = self.operator.generate_instructions(gen);
        let right_side = self.right.generate_instructions(gen);

        result.extend(left_side);
        result.extend(right_side);
        result.extend(operation);

        result
    }
}

impl<'a> Instructions<'a> for Integer {
    fn generate_instructions(&self, gen: &'a mut Generator) -> Vec<Instruction> {
        vec![Instruction::I32Const(self.value)]
    }
}

impl<'a> Instructions<'a> for Expression {
    fn generate_instructions(&self, gen: &'a mut Generator) -> Vec<Instruction> {
        match self {
            Expression::Integer(int) => int.generate_instructions(gen),
            Expression::Infix(infix) => infix.generate_instructions(gen),

            _ => todo!(),
        }
    }
}

impl<'a> Instructions<'a> for BlockStatement {
    fn generate_instructions(&self, gen: &'a mut Generator) -> Vec<Instruction> {
        let mut result: Vec<Instruction> = vec![];
        for statement in self.statements {
            result.extend(statement.generate_instructions(gen));
        }

        result
    }
}

impl<'a> Instructions<'a> for FunctionStatement {
    fn generate_instructions(&self, gen: &'a mut Generator) -> Vec<Instruction> {
        let mut result = self.body.generate_instructions(gen);
        result.push(Instruction::End);

        result
    }
}

impl<'a> Instructions<'a> for Statement {
    fn generate_instructions(&self, gen: &'a mut Generator) -> Vec<Instruction> {
        match self {
            Statement::Function(func) => func.generate_instructions(gen),
            _ => todo!(),
        }
    }
}

impl<'a> Instructions<'a> for Program {
    fn generate_instructions(&self, gen: &'a mut Generator) -> Vec<Instruction> {
        let mut result: Vec<Instruction> = vec![];
        for statement in self.statements {
            result.extend(statement.generate_instructions(gen));
        }

        result
    }
}

pub struct Generator {
    /// The result of the parser
    ast: Program,

    /// Final source
    module: Module,

    /// Manages types like function types
    type_manager: TypeManager,
}

impl Generator {
    /// Creates the new Generator
    pub fn new(program: Program) -> Self {
        Self {
            ast: program,
            type_manager: TypeManager::new(),
            module: Module::new(),
        }
    }

    pub fn visit(&mut self) -> Self {}
}

pub struct TypeManager {
    section: TypeSection,
    types_index: u32,
}

impl TypeManager {
    pub fn new() -> Self {
        Self {
            section: TypeSection::new(),
            types_index: 0,
        }
    }

    pub fn new_function_type(
        &mut self,
        param_type: Vec<ValType>,
        result_type: Vec<ValType>,
    ) -> u32 {
        self.section.function(param_type, result_type);

        let index = self.types_index;

        self.types_index += 1;

        index
    }
}
