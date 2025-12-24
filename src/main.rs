use std::env;
use std::fs;
use std::path::{Path, PathBuf};

use crustc::bytecode::{compile_to_bytecode, read_bytecode, write_bytecode};
use crustc::lexer::lex_line;
use crustc::loader::load_program;
use crustc::typer::type_check;
use crustc::vm::run_bytecode;

fn main() {
    let mut args = env::args().skip(1).collect::<Vec<_>>();
    if args.is_empty() {
        eprintln!(
            "Usage: crustc <input.crust|input.crb> [-o output] [--run] [--emit=bytecode|none|ast|tokens]\n\
             Example source:\n\
             let x = 40 + 2\n\
             print x"
        );
        std::process::exit(1);
    }

    let input_path = PathBuf::from(args.remove(0));
    let mut output_path = None;
    let mut run = false;
    let mut emit = Emit::Bytecode;
    let mut i = 0;
    while i < args.len() {
        if args[i] == "-o" && i + 1 < args.len() {
            output_path = Some(PathBuf::from(&args[i + 1]));
            i += 2;
        } else if args[i] == "--run" {
            run = true;
            i += 1;
        } else if let Some(value) = args[i].strip_prefix("--emit=") {
            emit = match value {
                "bytecode" => Emit::Bytecode,
                "none" => Emit::None,
                "ast" => Emit::Ast,
                "tokens" => Emit::Tokens,
                _ => {
                    eprintln!("Unknown emit option: {}", value);
                    std::process::exit(1);
                }
            };
            i += 1;
        } else {
            eprintln!("Unknown argument: {}", args[i]);
            std::process::exit(1);
        }
    }

    if input_path.extension().and_then(|s| s.to_str()) == Some("crb") {
        if run {
            let bytecode = read_bytecode(&input_path);
            run_bytecode(&bytecode);
        } else {
            eprintln!("Input is bytecode. Use --run to execute it.");
            std::process::exit(1);
        }
        return;
    }

    let source = read_file(&input_path);
    let mut emit_text: Option<String> = None;
    if emit == Emit::Tokens {
        emit_text = Some(emit_tokens(&source));
    }

    let program = load_program(&input_path);
    if emit == Emit::Ast {
        emit_text = Some(emit_ast(&program));
    }
    let typed = type_check(program);
    let bytecode = compile_to_bytecode(&typed);

    if emit == Emit::Bytecode {
        let out_path = output_path.clone().unwrap_or_else(|| {
            let mut p = input_path.clone();
            p.set_extension("crb");
            p
        });
        write_bytecode(&out_path, &bytecode);
    } else if output_path.is_some() {
        eprintln!("'-o' is only valid when --emit=bytecode");
        std::process::exit(1);
    }

    if let Some(text) = emit_text {
        if let Some(path) = output_path {
            write_file(&path, &text);
        } else {
            print!("{}", text);
        }
    }

    if run {
        run_bytecode(&bytecode);
    } else if emit == Emit::None {
        eprintln!("Nothing to do. Use --run or --emit=bytecode.");
        std::process::exit(1);
    }
}

fn read_file(path: &Path) -> String {
    fs::read_to_string(path).unwrap_or_else(|e| {
        eprintln!("Failed to read {}: {}", path.display(), e);
        std::process::exit(1);
    })
}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Emit {
    Bytecode,
    None,
    Ast,
    Tokens,
}

fn emit_tokens(source: &str) -> String {
    let mut out = String::new();
    for (line_no, line) in source.lines().enumerate() {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }
        let tokens = lex_line(trimmed, line_no);
        if tokens.is_empty() {
            continue;
        }
        out.push_str(&format!("line {}: {:?}\n", line_no + 1, tokens));
    }
    out
}

fn emit_ast(program: &crustc::ast::Program) -> String {
    format!("{:#?}\n", program)
}

fn write_file(path: &Path, data: &str) {
    fs::write(path, data).unwrap_or_else(|e| {
        eprintln!("Failed to write {}: {}", path.display(), e);
        std::process::exit(1);
    })
}
