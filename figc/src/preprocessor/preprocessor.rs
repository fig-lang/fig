use std::{collections::HashMap, fs::File, io::Read};

use crate::{
    lexer::lexer::Lexer,
    parser::{
        ast::{ImportType, Program, Statement},
        parser::Parser,
    },
};

trait PreProcess<'a> {
    fn process(&self, prep: &'a mut Preprocessor) -> Vec<Self>
    where
        Self: Sized;
}

impl<'a> PreProcess<'a> for Statement {
    fn process(&self, prep: &'a mut Preprocessor) -> Vec<Self> {
        match self.clone() {
            Statement::Import(imp) => match imp.ty {
                ImportType::Mod(m) => {
                    let code = prep.modules.get(&m).unwrap();

                    let mut lexer = Lexer::new(code.clone());

                    let mut parser = Parser::new(&mut lexer);
                    let program = Program::parse(&mut parser, None);

                    let mut new_prep = prep.clone();

                    new_prep.program = program;

                    new_prep.process().statements
                }

                ImportType::Path(p) => {
                    let mut file = File::open(p).unwrap();

                    let mut code = String::new();

                    file.read_to_string(&mut code).unwrap();

                    let mut lexer = Lexer::new(code.clone());

                    let mut parser = Parser::new(&mut lexer);
                    let program = Program::parse(&mut parser, None);

                    let mut new_prep = prep.clone();

                    new_prep.program = program;

                    new_prep.process().statements
                }
            },

            other => vec![other],
        }
    }
}

#[derive(Clone)]
pub struct Preprocessor {
    program: Program,
    modules: HashMap<String, String>,
}

impl Preprocessor {
    pub fn new(program: Program) -> Self {
        Self {
            program,
            modules: HashMap::new(),
        }
    }

    pub fn add_module(&mut self, name: String, code: String) {
        self.modules.insert(name, code);
    }

    pub fn process(&mut self) -> Program {
        let mut new_program = Program {
            statements: vec![],
            errors: vec![],
        };

        for statement in self.program.statements.clone() {
            new_program.statements.extend(statement.process(self));
        }

        new_program
    }
}
