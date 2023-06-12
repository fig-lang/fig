use std::collections::HashMap;

use wasm_encoder::{
    CodeSection, Function, FunctionSection, Instruction, Module, TypeSection, ValType,
};

use crate::{
    lexer::token::Token,
    parser::ast::{
        BlockStatement, Expression, FunctionStatement, Identifier, InfixExpr, Integer, Program,
        Statement,
    },
};

#[derive(Debug)]
pub enum CompilerError {
    NotDefined(String),
}

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

type CResult<T> = Result<T, CompilerError>;

pub trait Instructions<'a> {
    fn generate_instructions(&self, gen: &'a mut Generator) -> CResult<Vec<Instruction>>
    where
        Self: Sized;
}

impl<'a> Instructions<'a> for Token {
    fn generate_instructions(&self, gen: &'a mut Generator) -> CResult<Vec<Instruction>> {
        match self {
            Token::Plus => Ok(vec![Instruction::I32Add]),
            Token::Minus => Ok(vec![Instruction::I32Sub]),
            Token::ForwardSlash => Ok(vec![Instruction::I32DivS]),
            Token::Asterisk => Ok(vec![Instruction::I32Mul]),

            _ => todo!(),
        }
    }
}

impl<'a> Instructions<'a> for InfixExpr {
    fn generate_instructions(&self, gen: &'a mut Generator) -> CResult<Vec<Instruction>> {
        let mut result: Vec<Instruction> = vec![];
        let left_side = self.left.generate_instructions(gen)?;
        let operation = self.operator.generate_instructions(gen)?;
        let right_side = self.right.generate_instructions(gen)?;

        result.extend(left_side);
        result.extend(right_side);
        result.extend(operation);

        Ok(result)
    }
}

impl<'a> Instructions<'a> for Integer {
    fn generate_instructions(&self, _gen: &'a mut Generator) -> CResult<Vec<Instruction>> {
        Ok(vec![Instruction::I32Const(self.value)])
    }
}

impl<'a> Instructions<'a> for Expression {
    fn generate_instructions(&self, gen: &'a mut Generator) -> CResult<Vec<Instruction>> {
        match self {
            Expression::Integer(int) => Ok(int.generate_instructions(gen)?),
            Expression::Infix(infix) => Ok(infix.generate_instructions(gen)?),
            Expression::Identifier(ident) => Ok(ident.generate_instructions(gen)?),

            _ => todo!(),
        }
    }
}

impl<'a> Instructions<'a> for Identifier {
    fn generate_instructions(&self, gen: &'a mut Generator) -> CResult<Vec<Instruction>> {
        // Is it the let or function arg
        // TODO: remove unwrap
        let Some(func) = gen.function_manager.current_function() else {
            return Err(CompilerError::NotDefined("Function is not defined!".to_string()));
        };
        let Some(param) = func
            .params
            .into_iter()
            .find(|param| param.name == self.value) else {
                return Err(
                    CompilerError::NotDefined(format!("Variable with name {} is not defined!", self.value))
                );
            };

        Ok(vec![Instruction::LocalGet(param.id)])
    }
}

impl<'a> Instructions<'a> for BlockStatement {
    fn generate_instructions(&self, gen: &'a mut Generator) -> CResult<Vec<Instruction>> {
        let mut result: Vec<Instruction> = vec![];
        for statement in &self.statements {
            result.extend(statement.generate_instructions(gen)?);
        }

        Ok(result)
    }
}

impl<'a> Instructions<'a> for FunctionStatement {
    fn generate_instructions(&self, gen: &'a mut Generator) -> CResult<Vec<Instruction>> {
        let mut result = self.body.generate_instructions(gen)?;
        result.push(Instruction::End);

        Ok(result)
    }
}

impl<'a> Instructions<'a> for Statement {
    fn generate_instructions(&self, gen: &'a mut Generator) -> CResult<Vec<Instruction>> {
        match self {
            Statement::Function(func) => {
                let types = func.types();
                let type_index = gen.type_manager.new_function_type(types.0.clone(), types.1);

                let params = types
                    .0
                    .into_iter()
                    .zip(func.params.clone())
                    .enumerate()
                    .map(|(i, (t, param))| FunctionParam {
                        id: i as u32,
                        param_type: t,
                        name: param.value,
                    })
                    .collect::<Vec<FunctionParam>>();

                gen.function_manager
                    .new_function(type_index, func.name.value.clone(), params);

                let block = func.generate_instructions(gen)?;
                gen.code_manager.new_function_code(block);

                Ok(vec![])
            }

            Statement::Expression(expr) => expr.generate_instructions(gen),

            _ => todo!(),
        }
    }
}

impl<'a> Instructions<'a> for Program {
    fn generate_instructions(&self, gen: &'a mut Generator) -> CResult<Vec<Instruction>> {
        let mut result: Vec<Instruction> = vec![];
        for statement in &self.statements {
            result.extend(statement.generate_instructions(gen)?);
        }

        Ok(result)
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

    pub fn visit(&mut self) -> CResult<()>{
        let ast = self.ast.clone();

        ast.generate_instructions(self)?;

        Ok(())
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

#[derive(Debug, Clone)]
pub struct FunctionData {
    params: Vec<FunctionParam>,
    id: u32,
}

#[derive(Debug, Clone)]
pub struct FunctionParam {
    id: u32,
    name: String,
    param_type: ValType,
}

pub struct FunctionManager {
    functions: HashMap<String, FunctionData>,
    section: FunctionSection,
    functions_index: u32,
    current_function: Option<FunctionData>,
}

impl FunctionManager {
    pub fn new() -> Self {
        Self {
            section: FunctionSection::new(),
            functions_index: 0,
            functions: HashMap::new(),
            current_function: None,
        }
    }

    pub fn current_function(&self) -> Option<FunctionData> {
        self.current_function.clone()
    }

    pub fn new_function(&mut self, type_index: u32, name: String, params: Vec<FunctionParam>) {
        let new_fn = FunctionData {
            params,
            id: self.functions_index,
        };

        self.functions.insert(name, new_fn.clone());

        self.section.function(type_index);

        self.current_function = Some(new_fn);

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
