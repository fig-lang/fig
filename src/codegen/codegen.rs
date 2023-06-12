use wasm_encoder::{
    CodeSection, Function, FunctionSection, Instruction, Module, TypeSection, ValType,
};

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

        for _param in &self.params {
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
        for statement in &self.statements {
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
            Statement::Function(func) => {
                let block = func.generate_instructions(gen);
                let types = func.types();
                let type_index = gen.type_manager.new_function_type(types.0, types.1);
                gen.function_manager.new_function(type_index);
                gen.code_manager.new_function_code(block);

                vec![]
            }

            Statement::Expression(expr) => expr.generate_instructions(gen),

            _ => todo!(),
        }
    }
}

impl<'a> Instructions<'a> for Program {
    fn generate_instructions(&self, gen: &'a mut Generator) -> Vec<Instruction> {
        let mut result: Vec<Instruction> = vec![];
        for statement in &self.statements {
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

    /// Manages functions
    function_manager: FunctionManager,

    /// Manages Codes
    code_manager: CodeManager,
}

impl Generator {
    /// Creates the new Generator
    pub fn new(program: Program) -> Self {
        Self {
            ast: program,
            type_manager: TypeManager::new(),
            module: Module::new(),
            code_manager: CodeManager::new(),
            function_manager: FunctionManager::new(),
        }
    }

    pub fn visit(&mut self) {
        let ast = self.ast.clone();
        ast.generate_instructions(self);
    }

    pub fn generate(&mut self) -> Vec<u8> {
        self.module.section(&self.type_manager.get_section());
        self.module.section(&self.function_manager.get_section());
        self.module.section(&self.code_manager.get_section());

        self.module.clone().finish()
    }
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

    pub fn get_section(&self) -> TypeSection {
        self.section.clone()
    }
}

pub struct FunctionManager {
    section: FunctionSection,
    functions_index: u32,
}

impl FunctionManager {
    pub fn new() -> Self {
        Self {
            section: FunctionSection::new(),
            functions_index: 0,
        }
    }

    pub fn new_function(&mut self, type_index: u32) {
        self.section.function(type_index);

        self.functions_index += 1;
    }

    pub fn get_section(&self) -> FunctionSection {
        self.section.clone()
    }
}

pub struct CodeManager {
    section: CodeSection,
}

impl CodeManager {
    pub fn new() -> Self {
        Self {
            section: CodeSection::new(),
        }
    }

    pub fn new_function_code(&mut self, instructions: Vec<Instruction>) {
        let mut func = Function::new(vec![]);

        for instruction in &instructions {
            func.instruction(instruction);
        }

        self.section.function(&func);
    }

    pub fn get_section(&self) -> CodeSection {
        self.section.clone()
    }
}
