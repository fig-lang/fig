use std::{collections::HashMap, panic::RefUnwindSafe};

use wasm_encoder::{
    BlockType, CodeSection, ConstExpr, DataSection, ElementSection, EntityType, ExportKind,
    ExportSection, Function, FunctionSection, GlobalSection, ImportSection, Instruction, MemArg,
    MemorySection, MemoryType, Module, TableSection, TypeSection, ValType,
};

use crate::{
    lexer::token::Token,
    parser::ast::{
        BlockStatement, BreakStatement, CallExpr, ExportStatement, Expression, ExternalStatement,
        FunctionMeta, FunctionStatement, Identifier, IfExpr, IndexExpr, InfixExpr, Integer,
        LetStatement, LoopStatement, Program, ReturnStatement, SetStatement, Statement, StringExpr,
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

impl WasmTypes for FunctionMeta {
    type Output = CResult<(Vec<ValType>, Vec<ValType>)>;

    fn types(&self) -> Self::Output {
        let mut param_type: Vec<ValType> = vec![];

        for param in &self.params {
            param_type.push(param.1.clone().try_into()?);
        }

        let return_type: Vec<ValType> = match self.return_type.clone() {
            Some(ret_type) => vec![ret_type.try_into()?],

            None => vec![],
        };

        Ok((param_type, return_type))
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

        let ptr = gen
            .memory_manager
            .alloc(size as i32, string.as_bytes().to_vec());

        Ok(vec![Instruction::I32Const(ptr)])
    }
}

impl<'a> Instructions<'a> for Token {
    fn generate_instructions(&self, _gen: &'a mut Generator) -> CResult<Vec<Instruction>> {
        // Todo check type
        match self {
            Token::Plus => Ok(vec![Instruction::I32Add]),
            Token::Minus => Ok(vec![Instruction::I32Sub]),
            Token::ForwardSlash => Ok(vec![Instruction::I32DivS]),
            Token::Asterisk => Ok(vec![Instruction::I32Mul]),
            Token::Equal => Ok(vec![Instruction::I32Eq]),
            Token::NotEqual => Ok(vec![Instruction::I32Ne]),
            Token::LessThan => Ok(vec![Instruction::I32LeS]),
            Token::GreaterThan => Ok(vec![Instruction::I32GtS]),

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

impl<'a> Instructions<'a> for ExternalStatement {
    fn generate_instructions(&self, gen: &'a mut Generator) -> CResult<Vec<Instruction>> {
        for func in &self.body.function_types {
            let (param_types, result_type) = func.types()?;
            let type_id = gen
                .type_manager
                .new_function_type(param_types.clone(), result_type);

            let params = param_types
                .into_iter()
                .zip(func.params.clone())
                .enumerate()
                .map(|(i, (t, param))| FunctionParam {
                    id: i as u32,
                    param_type: t,
                    name: param.0.value,
                })
                .collect::<Vec<FunctionParam>>();

            gen.function_manager
                .new_external_function(func.name.value.clone(), params);

            gen.import_manager.import_func(
                &self.module.value,
                &func.name.value,
                EntityType::Function(type_id),
            );
        }
        Ok(vec![])
    }
}

impl<'a> Instructions<'a> for LoopStatement {
    fn generate_instructions(&self, gen: &'a mut Generator) -> CResult<Vec<Instruction>> {
        let mut result: Vec<Instruction> = vec![];
        result.push(Instruction::Block(BlockType::Empty));

        result.push(Instruction::Loop(BlockType::Empty));

        let block = self.block.generate_instructions(gen)?;
        result.extend(block);

        result.push(Instruction::Br(0));
        result.push(Instruction::End);
        result.push(Instruction::End);

        Ok(result)
    }
}

impl<'a> Instructions<'a> for BreakStatement {
    fn generate_instructions(&self, _gen: &'a mut Generator) -> CResult<Vec<Instruction>> {
        // TODO: not fix value
        // need a new manager :}
        Ok(vec![Instruction::Br(2)])
    }
}

impl IndexExpr {
    fn get_instruction<'a>(&self, gen: &'a mut Generator) -> CResult<Vec<Instruction>> {
        let mut offset = self.get_offset(gen)?;
        offset.push(Instruction::I32Load(MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        }));
        Ok(offset)
    }

    fn get_offset<'a>(&self, gen: &'a mut Generator) -> CResult<Vec<Instruction>> {
        let mut result: Vec<Instruction> = vec![];
        // First we want to get the offset,
        // we add the current offset with the self.index
        let Some(variable) = gen.local_manager.get_local_index(&self.variable.value) else {
            return Err(CompilerError::NotDefined(
                format!("Variable with name {} is not defined!", self.variable.value)
            ));
        };

        // Is this good solution ?
        result.push(Instruction::LocalGet(variable.clone()));
        result.push(Instruction::I32Const(self.index.value as i32));
        result.push(Instruction::I32Add);

        Ok(result)
    }
}

impl<'a> Instructions<'a> for IndexExpr {
    fn generate_instructions(&self, _gen: &'a mut Generator) -> CResult<Vec<Instruction>> {
        Ok(vec![Instruction::I32Store8(MemArg {
            offset: 0,
            align: 0,
            memory_index: 0,
        })])
    }
}

impl<'a> Instructions<'a> for SetStatement {
    fn generate_instructions(&self, gen: &'a mut Generator) -> CResult<Vec<Instruction>> {
        let mut result: Vec<Instruction> = vec![];
        let expression = self.expression.generate_instructions(gen)?;

        match &self.variable {
            Expression::Index(index_expr) => {
                result.extend(index_expr.get_offset(gen)?);
                result.extend(expression);
                result.extend(index_expr.generate_instructions(gen)?);
            }

            Expression::Identifier(ident) => {
                let Some(var_id) = gen.local_manager.get_local_index(&ident.value) else {
                    return Err(
                        CompilerError::NotDefined(
                            format!("Variable with name {} is not defined!", ident.value)
                        )
                    );
                };

                result.push(Instruction::LocalSet(var_id.to_owned()));
            }

            _ => unreachable!(),
        }

        Ok(result)
    }
}

impl<'a> Instructions<'a> for ExportStatement {
    fn generate_instructions(&self, gen: &'a mut Generator) -> CResult<Vec<Instruction>> {
        let function_instructions = self.value.generate_instructions(gen)?;
        let Some(current_function) = gen.function_manager.current_function() else {
            return Err(CompilerError::NotDefined("Function not defined!".to_string()));
        };

        gen.export_manager
            .export_function(&current_function.name, current_function.id);

        Ok(function_instructions)
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
            Expression::If(if_expr) => Ok(if_expr.generate_instructions(gen)?),
            Expression::Index(index_expr) => Ok(index_expr.get_instruction(gen)?),

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
        // create the local and set the active local
        let local_index = gen.local_manager.new_local(self.name.value.clone());
        gen.local_manager.set_active_local(local_index);

        let mut result: Vec<Instruction> = vec![];
        let let_value = self.value.generate_instructions(gen)?;

        result.extend(let_value);

        // create new local
        gen.code_manager
            .current_locals
            .push(self.value_type.clone().try_into()?);

        if !gen.local_manager.get_already_set() {
            result.push(Instruction::LocalSet(local_index));
        }

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

impl<'a> Instructions<'a> for IfExpr {
    fn generate_instructions(&self, gen: &'a mut Generator) -> CResult<Vec<Instruction>> {
        let mut result = vec![];
        let condition = self.condition.generate_instructions(gen)?;
        result.extend(condition);

        result.push(Instruction::If(wasm_encoder::BlockType::Empty));

        let block = self.consequence.generate_instructions(gen)?;

        result.extend(block);

        // If we are in a let statement
        if let Some(id) = gen.local_manager.get_active_local() {
            result.push(Instruction::LocalSet(id.to_owned()));

            gen.local_manager.already_set(true);
        }

        match &self.alternative {
            Some(alt) => {
                result.push(Instruction::Else);

                let block = alt.generate_instructions(gen)?;

                result.extend(block);

                if let Some(id) = gen.local_manager.get_active_local() {
                    result.push(Instruction::LocalSet(id.to_owned()));
                }
            }

            None => {}
        };

        result.push(Instruction::End);

        Ok(result)
    }
}

impl<'a> Instructions<'a> for Statement {
    fn generate_instructions(&self, gen: &'a mut Generator) -> CResult<Vec<Instruction>> {
        match self {
            Statement::Function(func) => {
                let types = func.meta.types()?;
                let type_index = gen.type_manager.new_function_type(types.0.clone(), types.1);

                let params = types
                    .0
                    .into_iter()
                    .zip(func.meta.params.clone())
                    .enumerate()
                    .map(|(i, (t, param))| FunctionParam {
                        id: i as u32,
                        param_type: t,
                        name: param.0.value,
                    })
                    .collect::<Vec<FunctionParam>>();

                for param in &func.meta.params {
                    gen.local_manager.new_local(param.0.value.to_owned());
                }

                gen.function_manager
                    .new_function(type_index, func.meta.name.value.clone(), params);

                let block = func.generate_instructions(gen)?;
                gen.code_manager.new_function_code(block);

                gen.local_manager.reset();

                Ok(vec![])
            }

            Statement::Expression(expr) => expr.generate_instructions(gen),

            Statement::Let(var) => {
                let let_statement = var.generate_instructions(gen);

                // Exit the let
                gen.local_manager.exit_active_local();
                gen.local_manager.already_set(false);

                return let_statement;
            }
            Statement::Return(ret) => ret.generate_instructions(gen),
            Statement::Export(export) => export.generate_instructions(gen),
            Statement::Loop(l) => l.generate_instructions(gen),
            Statement::Set(set) => set.generate_instructions(gen),
            Statement::Break(br) => br.generate_instructions(gen),
            Statement::External(external) => external.generate_instructions(gen),

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

    export_manager: ExportManager,

    import_manager: ImportManager,
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
            export_manager: ExportManager::new(),
            import_manager: ImportManager::new(),
            memory_manager: MemoryManager::new(mem),
        }
    }

    pub fn visit(&mut self) -> CResult<()> {
        let ast = self.ast.clone();

        ast.generate_instructions(self)?;

        Ok(())
    }

    pub fn generate(&mut self) -> Vec<u8> {
        // export memory
        self.export_manager.export_memory("memory", 0);

        //TODO
        self.module.section(&self.type_manager.get_section());
        self.module.section(&self.import_manager.get_sections());
        self.module.section(&self.function_manager.get_section());
        self.module.section(&TableSection::new());

        let (mem_section, data_section) = &self.memory_manager.get_sections();

        self.module.section(mem_section);
        self.module.section(&GlobalSection::new());
        self.module.section(&self.export_manager.get_sections());
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
    name: String,
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

    pub fn new_external_function(&mut self, name: String, params: Vec<FunctionParam>) {
        let new_fn = FunctionData {
            name: name.clone(),
            params,
            id: self.functions_index,
        };

        self.functions.insert(name, new_fn.clone());
        self.functions_index += 1;
    }

    pub fn new_function(&mut self, type_index: u32, name: String, params: Vec<FunctionParam>) {
        let new_fn = FunctionData {
            name: name.clone(),
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

    /// The current active local
    ///
    /// Mostly used for return value of the if statement
    /// to set to the active_local
    active_local: Option<u32>,

    /// Some times the let dont need to set
    /// the expression to the local
    ///
    /// and its already seted
    already_set: bool,
}

impl LocalManager {
    /// Create new local manager
    pub fn new() -> Self {
        Self {
            locals: HashMap::new(),
            locals_index: 0,
            active_local: None,
            already_set: false,
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

    pub fn exit_active_local(&mut self) {
        self.active_local = None;
    }

    pub fn get_active_local(&self) -> &Option<u32> {
        &self.active_local
    }

    pub fn set_active_local(&mut self, local_id: u32) {
        self.active_local = Some(local_id);
    }

    pub fn already_set(&mut self, new_value: bool) {
        self.already_set = new_value;
    }

    pub fn get_already_set(&self) -> &bool {
        &self.already_set
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
    /// Returns pointer to the data
    pub fn alloc<D>(&mut self, size: i32, data: D) -> i32
    where
        D: IntoIterator<Item = u8>,
        D::IntoIter: ExactSizeIterator,
    {
        let ptr = self.offset;
        let offset = ConstExpr::i32_const(ptr);

        self.data_section.active(0, &offset, data);

        self.offset += size;

        ptr
    }

    pub fn current_offset(&self) -> i32 {
        self.offset
    }

    pub fn get_sections(&self) -> (MemorySection, DataSection) {
        (self.memory_section.clone(), self.data_section.clone())
    }
}

pub struct ExportManager {
    section: ExportSection,
}

impl ExportManager {
    pub fn new() -> Self {
        Self {
            section: ExportSection::new(),
        }
    }

    pub fn export_memory(&mut self, name: &str, id: u32) {
        self.section.export(name, ExportKind::Memory, id);
    }

    pub fn export_function(&mut self, name: &String, id: u32) {
        self.section.export(name, ExportKind::Func, id);
    }

    pub fn get_sections(&self) -> ExportSection {
        self.section.clone()
    }
}

pub struct ImportManager {
    section: ImportSection,
}

impl ImportManager {
    pub fn new() -> Self {
        Self {
            section: ImportSection::new(),
        }
    }

    pub fn import_func(
        &mut self,
        module: &String,
        function_name: &String,
        function_type: EntityType,
    ) {
        self.section.import(module, function_name, function_type);
    }

    pub fn get_sections(&self) -> ImportSection {
        self.section.clone()
    }
}
