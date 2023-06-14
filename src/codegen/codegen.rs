use std::collections::HashMap;

use wasm_encoder::{
    CodeSection, ConstExpr, DataSection, Function, FunctionSection, Instruction, MemorySection,
    MemoryType, Module, TypeSection, ValType, TableSection, GlobalSection, ExportSection, ImportSection, ElementSection,
};

use crate::{
    lexer::token::Token,
    parser::ast::{
        BlockStatement, CallExpr, Expression, FunctionStatement, Identifier, InfixExpr, Integer,
        LetStatement, Program, ReturnStatement, Statement, StringExpr,
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
    type Output = CResult<(Vec<ValType>, Vec<ValType>)>;

    fn types(&self) -> Self::Output {
        let mut param_type: Vec<ValType> = vec![];

        for param in &self.params {
            param_type.push(param.1.clone().try_into()?);
        }

        Ok((param_type, vec![ValType::I32]))
    }
}

type CResult<T> = Result<T, CompilerError>;

pub trait Instructions<'a> {
    fn generate_instructions(&self, gen: &'a mut Generator) -> CResult<Vec<Instruction>>
    where
        Self: Sized;
}

impl<'a> Instructions<'a> for CallExpr {
    // TODO: First check if the function exists
    fn generate_instructions(&self, gen: &'a mut Generator) -> CResult<Vec<Instruction>> {
        let mut result: Vec<Instruction> = vec![];

        for arg in &self.arguments {
            result.extend(arg.generate_instructions(gen)?);
        }

        let Some(func) = gen.function_manager.get_function(&self.function.value) else {
            return Err(
                CompilerError::NotDefined(format!("Function with name {} is not defined!", self.function.value))
            );
        };

        result.push(Instruction::Call(func.id));

        Ok(result)
    }
}

impl<'a> Instructions<'a> for StringExpr {
    fn generate_instructions(&self, gen: &'a mut Generator) -> CResult<Vec<Instruction>> {
        let size = self.string.len() + 1;

        let mut string = self.string.to_owned();
        string.push('\0');

        gen.memory_manager
            .alloc(size as i32, string.as_bytes().to_vec());

        Ok(vec![Instruction::I32Const(1)])
    }
}

impl<'a> Instructions<'a> for Token {
    fn generate_instructions(&self, _gen: &'a mut Generator) -> CResult<Vec<Instruction>> {
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

impl<'a> Instructions<'a> for ReturnStatement {
    fn generate_instructions(&self, gen: &'a mut Generator) -> CResult<Vec<Instruction>> {
        let mut result = vec![];

        let expr = self.return_value.generate_instructions(gen)?;
        result.extend(expr);

        result.push(Instruction::Return);

        Ok(result)
    }
}

impl<'a> Instructions<'a> for Expression {
    fn generate_instructions(&self, gen: &'a mut Generator) -> CResult<Vec<Instruction>> {
        match self {
            Expression::Integer(int) => Ok(int.generate_instructions(gen)?),
            Expression::Infix(infix) => Ok(infix.generate_instructions(gen)?),
            Expression::Identifier(ident) => Ok(ident.generate_instructions(gen)?),
            Expression::Call(call) => Ok(call.generate_instructions(gen)?),
            Expression::String(s) => Ok(s.generate_instructions(gen)?),

            x => panic!("{:?}", x),
        }
    }
}

impl<'a> Instructions<'a> for Identifier {
    fn generate_instructions(&self, gen: &'a mut Generator) -> CResult<Vec<Instruction>> {
        let Some(id) = gen.local_manager.get_local_index(&self.value) else {
            return Err(
                CompilerError::NotDefined(format!("Variable with name {} is not defined!", self.value))
            );
        };

        Ok(vec![Instruction::LocalGet(id.clone())])
    }
}

impl<'a> Instructions<'a> for LetStatement {
    fn generate_instructions(&self, gen: &'a mut Generator) -> CResult<Vec<Instruction>> {
        let mut result: Vec<Instruction> = vec![];
        let let_value = self.value.generate_instructions(gen)?;

        result.extend(let_value);

        // create new local
        let local_index = gen.local_manager.new_local(self.name.value.clone());
        gen.code_manager
            .current_locals
            .push(self.value_type.clone().try_into()?);

        result.push(Instruction::LocalSet(local_index));

        Ok(result)
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
                let types = func.types()?;
                let type_index = gen.type_manager.new_function_type(types.0.clone(), types.1);

                let params = types
                    .0
                    .into_iter()
                    .zip(func.params.clone())
                    .enumerate()
                    .map(|(i, (t, param))| FunctionParam {
                        id: i as u32,
                        param_type: t,
                        name: param.0.value,
                    })
                    .collect::<Vec<FunctionParam>>();

                for param in &func.params {
                    gen.local_manager.new_local(param.0.value.to_owned());
                }

                gen.function_manager
                    .new_function(type_index, func.name.value.clone(), params);

                let block = func.generate_instructions(gen)?;
                gen.code_manager.new_function_code(block);

                gen.local_manager.reset();

                Ok(vec![])
            }

            Statement::Expression(expr) => expr.generate_instructions(gen),

            Statement::Let(var) => var.generate_instructions(gen),
            Statement::Return(ret) => ret.generate_instructions(gen),

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

    local_manager: LocalManager,

    memory_manager: MemoryManager,
}

impl Generator {
    /// Creates the new Generator
    pub fn new(program: Program) -> Self {
        let mem = MemoryType {
            minimum: 1,
            maximum: None,
            memory64: false,
            shared: false,
        };
        Self {
            ast: program,
            type_manager: TypeManager::new(),
            module: Module::new(),
            code_manager: CodeManager::new(),
            function_manager: FunctionManager::new(),
            local_manager: LocalManager::new(),
            memory_manager: MemoryManager::new(mem),
        }
    }

    pub fn visit(&mut self) -> CResult<()> {
        let ast = self.ast.clone();

        ast.generate_instructions(self)?;

        Ok(())
    }

    pub fn generate(&mut self) -> Vec<u8> {
        self.module.section(&self.type_manager.get_section());
        self.module.section(&ImportSection::new());
        self.module.section(&self.function_manager.get_section());
        self.module.section(&TableSection::new());

        let (mem_section, data_section) = &self.memory_manager.get_sections();

        self.module.section(mem_section);
        self.module.section(&GlobalSection::new());
        self.module.section(&ExportSection::new());
        self.module.section(&ElementSection::new());

        self.module.section(&self.code_manager.get_section());
        self.module.section(data_section);

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

    pub fn get_function(&self, function_name: &String) -> Option<&FunctionData> {
        self.functions.get(function_name)
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
    current_locals: Vec<ValType>,
}

impl CodeManager {
    pub fn new() -> Self {
        Self {
            section: CodeSection::new(),
            current_locals: vec![],
        }
    }

    pub fn new_function_code(&mut self, instructions: Vec<Instruction>) {
        let mut func = Function::new_with_locals_types(self.current_locals.clone());

        // idk is this ok?
        self.current_locals.clear();

        for instruction in &instructions {
            func.instruction(instruction);
        }

        self.section.function(&func);
    }

    pub fn get_section(&self) -> CodeSection {
        self.section.clone()
    }
}
pub struct LocalManager {
    /// name, id
    ///
    /// for example when new let were created
    /// new entry in this hasmap with (let name, index)
    ///
    /// wich first let index is 0 second is 1 and so on
    locals: HashMap<String, u32>,

    /// Index
    locals_index: u32,
}

impl LocalManager {
    /// Create new local manager
    pub fn new() -> Self {
        Self {
            locals: HashMap::new(),
            locals_index: 0,
        }
    }

    pub fn local_exists(&self, name: &String) -> bool {
        self.locals.contains_key(name)
    }

    pub fn get_local_index(&self, name: &String) -> Option<&u32> {
        self.locals.get(name)
    }

    /// When we create a new function
    pub fn reset(&mut self) {
        self.locals_index = 0;
    }

    /// Creates new local var
    ///
    /// and returns the index
    /// if its exists will overwrite it
    pub fn new_local(&mut self, name: String) -> u32 {
        let index = self.locals_index;
        self.locals.insert(name, self.locals_index.clone());

        self.locals_index += 1;

        index
    }
}

pub struct MemoryManager {
    memory_section: MemorySection,
    data_section: DataSection,

    offset: i32,
}

impl MemoryManager {
    pub fn new(memory: MemoryType) -> Self {
        let mut memory_section = MemorySection::new();
        memory_section.memory(memory);

        Self {
            memory_section,
            data_section: DataSection::new(),
            offset: 0,
        }
    }

    pub fn alloc<D>(&mut self, size: i32, data: D)
    where
        D: IntoIterator<Item = u8>,
        D::IntoIter: ExactSizeIterator,
    {
        let offset = ConstExpr::i32_const(self.offset as i32);

        self.data_section.active(0, &offset, data);

        self.offset += size;
    }

    pub fn get_sections(&self) -> (MemorySection, DataSection) {
        (self.memory_section.clone(), self.data_section.clone())
    }
}
