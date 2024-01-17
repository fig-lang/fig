use clap::{Args, Parser, Subcommand};
use figc::codegen::codegen::Context;
use figc::lexer::lexer::Lexer;
use figc::parser::ast::Program;
use figc::parser::parser::Parser as FigParser;
use std::fs::File;
use std::io::{Read, Write};
use std::path::PathBuf;
use std::process::exit;
use wasmer::{
    imports, Exports, Function, FunctionEnv, FunctionEnvMut, Imports, Instance, Memory, MemoryView,
    Module, Store, WasmPtr,
};
use wasmprinter::print_bytes;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Compiles Fig file to an Wasm module
    Compile(CompileArgs),

    Run(RunArgs),
}

#[derive(Args, Debug)]
struct RunArgs {
    /// Fig file path
    fig_file_path: PathBuf,
}

#[derive(Args, Debug)]
struct CompileArgs {
    /// Fig file path
    fig_file_path: PathBuf,

    #[arg(long, action)]
    /// Print Wat
    print_wat: bool,
}

/// Expects SourceCode returns Wasm binary
fn fig_compile_to_wasm(source: String, memory_offset: i32) -> Vec<u8> {
    let mut lexer = Lexer::new(source);
    let mut parser = FigParser::new(&mut lexer);
    let program = Program::parse(&mut parser, None);

    for error in program.get_errors() {
        println!("{}", error);
    }

    let mut ctx = Context::new(program, memory_offset);
    ctx.bootstrap();
    ctx.visit().unwrap();

    for error in ctx.get_errors() {
        println!("{}", error);
        exit(1);
    }

    let buf = ctx.generate();

    buf
}

struct Env {
    memory: Option<Memory>,
}

fn fig_print(mut env: FunctionEnvMut<Env>, p: WasmPtr<u8>) {
    let (env_data, store) = env.data_and_store_mut();
    let memory_view = env_data.memory.clone().unwrap().view(&store);

    println!("{}", p.read_utf8_string_with_nul(&memory_view).unwrap())
}

fn run_wasm(bytes: &Vec<u8>) {
    let mut store = Store::default();
    let module = Module::new(&store, bytes).unwrap();

    let env = FunctionEnv::new(&mut store, Env { memory: None });
    let import_object = imports! {
        "fig" => {
            "print" => Function::new_typed_with_env(&mut store, &env, fig_print),
        }
    };

    let instance = Instance::new(&mut store, &module, &import_object).unwrap();
    let memory = instance.exports.get_memory("memory").unwrap().clone();
    env.as_mut(&mut store).memory = Some(memory);
    let main_fn = instance.exports.get_function("main").unwrap();
    let _result = main_fn.call(&mut store, &[]).unwrap();
}

fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Compile(c_args) => {
            // Open file
            let mut buffer = String::new();
            {
                let mut file = File::open(&c_args.fig_file_path).unwrap();
                file.read_to_string(&mut buffer).unwrap();
                file.flush().unwrap();
            }

            // Now compile
            let final_wasm = fig_compile_to_wasm(buffer, 0x0);

            if c_args.print_wat {
                let wasm = print_bytes(&final_wasm)
                    .unwrap_or("Could't print compiled wasm to wat!".to_string());

                println!("{}", wasm);
            };

            // Create A Compiled wasm file
            let wasm_file_path = PathBuf::from(format!(
                "./{}.wasm",
                c_args.fig_file_path.file_stem().unwrap().to_str().unwrap()
            ));

            let mut wasm_file = File::create(wasm_file_path).unwrap();
            wasm_file.write_all(&*final_wasm).unwrap();
            wasm_file.flush().unwrap();
        }
        Commands::Run(r_args) => {
            let mut buffer = String::new();
            {
                let mut file = File::open(&r_args.fig_file_path).unwrap();
                file.read_to_string(&mut buffer).unwrap();
                file.flush().unwrap();
            }

            // Now compile
            let final_wasm = fig_compile_to_wasm(buffer, 0x0);

            run_wasm(&final_wasm);
        }
    }
}
