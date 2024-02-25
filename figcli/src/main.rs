use clap::{Args, Parser, Subcommand};
use figc::codegen::codegen::Context;
use figc::lexer::lexer::Lexer;
use figc::parser::ast::Program;
use figc::parser::parser::Parser as FigParser;
use figc::preprocessor::preprocessor::Preprocessor;
use std::fs::File;
use std::io::prelude::*;
use std::io::{BufReader, Read, Write};
use std::net::TcpListener;
use std::path::PathBuf;
use std::process::exit;
use wasmer::{
    imports, Function, FunctionEnv, FunctionEnvMut, Instance, Memory, Module, Store, TypedFunction,
    Value, WasmPtr,
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

    Server(ServerArgs),
}

#[derive(Args, Debug)]
struct RunArgs {
    /// Fig file path
    fig_file_path: PathBuf,
}

#[derive(Args, Debug)]
struct ServerArgs {
    /// Fig file path
    fig_file_path: PathBuf,

    #[arg(long, action)]
    addr: String,
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
fn fig_compile_to_wasm(source: String, memory_offset: i32) -> (Vec<u8>, i32) {
    let mut lexer = Lexer::new(source);
    let mut parser = FigParser::new(&mut lexer);
    let program = Program::parse(&mut parser, None);

    for error in program.get_errors() {
        println!("{}", error);
    }

    let mut preprocessor = Preprocessor::new(program);

    preprocessor.add_module(
        "std".to_string(),
        include_str!("../../std/std.fig").to_string(),
    );

    preprocessor.add_module(
        "server".to_string(),
        include_str!("../../std/server.fig").to_string(),
    );

    let program = preprocessor.process();

    let mut ctx = Context::new(program, memory_offset);
    ctx.bootstrap();
    ctx.visit().unwrap();

    for error in ctx.get_errors() {
        println!("{}", error);
        exit(1);
    }

    let buf = ctx.generate();

    (buf, ctx.memory_ctx.offset)
}

#[derive(Clone)]
struct Env {
    memory: Option<Memory>,
    alloc_func: Option<TypedFunction<i32, i32>>,
}

fn fig_print(mut env: FunctionEnvMut<Env>, p: WasmPtr<u8>) {
    let (env_data, store) = env.data_and_store_mut();
    let memory_view = env_data.memory.clone().unwrap().view(&store);

    println!("{}", p.read_utf8_string_with_nul(&memory_view).unwrap());
}

fn fig_print_int(int: i32) {
    println!("{}", int);
}

fn fig_print_char(c: i32) {
    println!("{}", char::from(c as u8));
}

fn fig_print_float(f: f32) {
    println!("{}", f);
}

fn fig_read_file(mut env: FunctionEnvMut<Env>, addr: WasmPtr<u8>) -> i32 {
    let (env_data, mut store) = env.data_and_store_mut();
    let mut file_content: Vec<u8> = Vec::new();

    {
        let memory_view = env_data.memory.clone().unwrap().view(&mut store);

        let addr_str = addr.read_utf8_string_with_nul(&memory_view).unwrap();

        let mut file = File::open(addr_str).unwrap();

        file.read_to_end(&mut file_content).unwrap();
    }

    let buf = env_data
        .alloc_func
        .clone()
        .unwrap()
        .call(&mut store, file_content.len() as i32 + 1)
        .unwrap();

    let memory_view = env_data.memory.clone().unwrap().view(&mut store);

    let buf_ptr = WasmPtr::<u8>::new(buf as u32);
    memory_view
        .write(buf_ptr.offset() as u64, &file_content)
        .unwrap();

    buf
}

fn run_wasm(bytes: &Vec<u8>) {
    let mut store = Store::default();
    let module = Module::new(&store, bytes).unwrap();

    let env = FunctionEnv::new(
        &mut store,
        Env {
            memory: None,
            alloc_func: None,
        },
    );
    let import_object = imports! {
        "io" => {
            "print_str" => Function::new_typed_with_env(&mut store, &env, fig_print),
            "print_int" => Function::new_typed(&mut store, fig_print_int),
            "print_char" => Function::new_typed(&mut store, fig_print_char),
            "print_float" => Function::new_typed(&mut store,  fig_print_float),
            "read_file" => Function::new_typed_with_env(&mut store, &env, fig_read_file),
        }
    };

    let instance = Instance::new(&mut store, &module, &import_object).unwrap();
    let memory = instance.exports.get_memory("memory").unwrap().clone();
    env.as_mut(&mut store).memory = Some(memory);
    let main_fn = instance.exports.get_function("main").unwrap();
    let _result = main_fn.call(&mut store, &[]).unwrap();
}

fn run_wasm_server<'a>(bytes: &Vec<u8>, addr: &'a str, mem_offset: i32) {
    let mut store = Store::default();
    let module = Module::new(&store, bytes).unwrap();

    let env = FunctionEnv::new(
        &mut store,
        Env {
            memory: None,
            alloc_func: None,
        },
    );
    let import_object = imports! {
        "io" => {
            "print_str" => Function::new_typed_with_env(&mut store, &env, fig_print),
            "print_int" => Function::new_typed(&mut store, fig_print_int),
            "print_char" => Function::new_typed(&mut store, fig_print_char),
            "print_float" => Function::new_typed(&mut store,  fig_print_float),
            "read_file" => Function::new_typed_with_env(&mut store, &env, fig_read_file),
        }
    };

    let listener = TcpListener::bind(addr).unwrap();

    for stream in listener.incoming() {
        let instance = Instance::new(&mut store, &module, &import_object).unwrap();
        let memory = instance.exports.get_memory("memory").unwrap().clone();
        let mem_offset_glob = instance.exports.get_global("mem_offset").unwrap();
        let alloc_func = instance
            .exports
            .get_typed_function(&store, "malloc")
            .unwrap();
        env.as_mut(&mut store).memory = Some(memory);
        env.as_mut(&mut store).alloc_func = Some(alloc_func.clone());
        let main_fn = instance.exports.get_function("main").unwrap();

        let mut stream = stream.unwrap();

        let buf_reader = BufReader::new(&mut stream);
        let http_request: String = buf_reader
            .lines()
            .map(|result| result.unwrap())
            .take_while(|line| !line.is_empty())
            .collect();

        mem_offset_glob
            .set(
                &mut store,
                Value::I32(mem_offset + http_request.len() as i32 + 1),
            )
            .unwrap();

        let mem = env.as_mut(&mut store).memory.clone().unwrap();
        let view = mem.view(&store);
        view.write(mem_offset as u64, &http_request.as_bytes())
            .unwrap();

        let response_value = main_fn.call(&mut store, &[Value::I32(mem_offset)]).unwrap();

        let response_ptr = WasmPtr::<u8>::new(response_value[0].i32().unwrap() as u32);

        let mem = env.as_mut(&mut store).memory.clone().unwrap();
        let view = mem.view(&store);
        let response = response_ptr.read_utf8_string_with_nul(&view).unwrap();

        stream.write(response.as_bytes()).unwrap();
        stream.flush().unwrap();
    }
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
            let (final_wasm, _offset) = fig_compile_to_wasm(buffer, 0x0);

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
            let (final_wasm, _offset) = fig_compile_to_wasm(buffer, 0x0);

            run_wasm(&final_wasm);
        }

        Commands::Server(server_args) => {
            let mut buffer = String::new();
            {
                let mut file = File::open(&server_args.fig_file_path).unwrap();
                file.read_to_string(&mut buffer).unwrap();
                file.flush().unwrap();
            }

            // Now compile
            let (final_wasm, offset) = fig_compile_to_wasm(buffer, 0x0);

            run_wasm_server(&final_wasm, &server_args.addr, offset);
        }
    }
}
