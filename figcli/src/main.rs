use clap::{Args, Parser, Subcommand};
use figc::codegen::codegen::Context;
use figc::lexer::lexer::Lexer;
use figc::parser::ast::Program;
use figc::parser::parser::Parser as FigParser;
use std::fs::File;
use std::io::{Read, Write};
use std::path::PathBuf;
use std::process::exit;
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
    }
}
