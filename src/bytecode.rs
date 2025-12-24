use std::fs::File;
use std::io::{Read, Write};
use std::path::Path;

use crate::ast::{Op, Type, TypedExpr, TypedExprKind, TypedProgram, TypedStmt};
use crate::util::die_simple;

#[derive(Debug, Clone)]
pub enum Instr {
    PushInt(i64),
    PushStr(String),
    LoadVar(String),
    StoreVar(String),
    Add,
    Sub,
    Mul,
    Div,
    PrintInt,
    PrintStr,
}

pub fn compile_to_bytecode(program: &TypedProgram) -> Vec<Instr> {
    let mut out = Vec::new();
    for stmt in &program.stmts {
        match stmt {
            TypedStmt::Print(expr) => {
                emit_expr(expr, &mut out);
                match expr.ty {
                    Type::Int => out.push(Instr::PrintInt),
                    Type::Str => out.push(Instr::PrintStr),
                }
            }
            TypedStmt::Let { name, expr } => {
                emit_expr(expr, &mut out);
                out.push(Instr::StoreVar(name.clone()));
            }
        }
    }
    out
}

fn emit_expr(expr: &TypedExpr, out: &mut Vec<Instr>) {
    match &expr.kind {
        TypedExprKind::Int(v) => out.push(Instr::PushInt(*v)),
        TypedExprKind::Str(s) => out.push(Instr::PushStr(s.clone())),
        TypedExprKind::Var(name) => out.push(Instr::LoadVar(name.clone())),
        TypedExprKind::Binary { left, op, right } => {
            emit_expr(left, out);
            emit_expr(right, out);
            out.push(match op {
                Op::Add => Instr::Add,
                Op::Sub => Instr::Sub,
                Op::Mul => Instr::Mul,
                Op::Div => Instr::Div,
            });
        }
    }
}

pub fn write_bytecode(path: &Path, code: &[Instr]) {
    let mut file = File::create(path).unwrap_or_else(|e| {
        eprintln!("Failed to write {}: {}", path.display(), e);
        std::process::exit(1);
    });

    file.write_all(b"CRBC").unwrap_or_else(|e| {
        eprintln!("Failed to write {}: {}", path.display(), e);
        std::process::exit(1);
    });
    file.write_all(&[1]).unwrap_or_else(|e| {
        eprintln!("Failed to write {}: {}", path.display(), e);
        std::process::exit(1);
    });
    write_u32(&mut file, code.len() as u32);
    for instr in code {
        write_instr(&mut file, instr);
    }
}

pub fn read_bytecode(path: &Path) -> Vec<Instr> {
    let mut file = File::open(path).unwrap_or_else(|e| {
        eprintln!("Failed to read {}: {}", path.display(), e);
        std::process::exit(1);
    });
    let mut magic = [0u8; 4];
    file.read_exact(&mut magic).unwrap_or_else(|_| {
        die_simple("Invalid bytecode file");
    });
    if &magic != b"CRBC" {
        die_simple("Invalid bytecode file");
    }
    let version = read_u8(&mut file);
    if version != 1 {
        die_simple("Unsupported bytecode version");
    }
    let count = read_u32(&mut file);
    let mut code = Vec::with_capacity(count as usize);
    for _ in 0..count {
        code.push(read_instr(&mut file));
    }
    code
}

fn write_instr(file: &mut File, instr: &Instr) {
    match instr {
        Instr::PushInt(v) => {
            write_u8(file, 1);
            write_i64(file, *v);
        }
        Instr::PushStr(s) => {
            write_u8(file, 2);
            write_string(file, s);
        }
        Instr::LoadVar(name) => {
            write_u8(file, 3);
            write_string(file, name);
        }
        Instr::StoreVar(name) => {
            write_u8(file, 4);
            write_string(file, name);
        }
        Instr::Add => write_u8(file, 5),
        Instr::Sub => write_u8(file, 6),
        Instr::Mul => write_u8(file, 7),
        Instr::Div => write_u8(file, 8),
        Instr::PrintInt => write_u8(file, 9),
        Instr::PrintStr => write_u8(file, 10),
    }
}

fn read_instr(file: &mut File) -> Instr {
    match read_u8(file) {
        1 => Instr::PushInt(read_i64(file)),
        2 => Instr::PushStr(read_string(file)),
        3 => Instr::LoadVar(read_string(file)),
        4 => Instr::StoreVar(read_string(file)),
        5 => Instr::Add,
        6 => Instr::Sub,
        7 => Instr::Mul,
        8 => Instr::Div,
        9 => Instr::PrintInt,
        10 => Instr::PrintStr,
        _ => die_simple("Invalid bytecode instruction"),
    }
}

fn write_u8(file: &mut File, value: u8) {
    file.write_all(&[value]).unwrap_or_else(|e| {
        eprintln!("Failed to write bytecode: {}", e);
        std::process::exit(1);
    });
}

fn read_u8(file: &mut File) -> u8 {
    let mut buf = [0u8; 1];
    file.read_exact(&mut buf).unwrap_or_else(|_| {
        die_simple("Unexpected EOF while reading bytecode");
    });
    buf[0]
}

fn write_u32(file: &mut File, value: u32) {
    file.write_all(&value.to_le_bytes()).unwrap_or_else(|e| {
        eprintln!("Failed to write bytecode: {}", e);
        std::process::exit(1);
    });
}

fn read_u32(file: &mut File) -> u32 {
    let mut buf = [0u8; 4];
    file.read_exact(&mut buf).unwrap_or_else(|_| {
        die_simple("Unexpected EOF while reading bytecode");
    });
    u32::from_le_bytes(buf)
}

fn write_i64(file: &mut File, value: i64) {
    file.write_all(&value.to_le_bytes()).unwrap_or_else(|e| {
        eprintln!("Failed to write bytecode: {}", e);
        std::process::exit(1);
    });
}

fn read_i64(file: &mut File) -> i64 {
    let mut buf = [0u8; 8];
    file.read_exact(&mut buf).unwrap_or_else(|_| {
        die_simple("Unexpected EOF while reading bytecode");
    });
    i64::from_le_bytes(buf)
}

fn write_string(file: &mut File, value: &str) {
    let bytes = value.as_bytes();
    write_u32(file, bytes.len() as u32);
    file.write_all(bytes).unwrap_or_else(|e| {
        eprintln!("Failed to write bytecode: {}", e);
        std::process::exit(1);
    });
}

fn read_string(file: &mut File) -> String {
    let len = read_u32(file) as usize;
    let mut buf = vec![0u8; len];
    file.read_exact(&mut buf).unwrap_or_else(|_| {
        die_simple("Unexpected EOF while reading bytecode");
    });
    String::from_utf8(buf).unwrap_or_else(|_| {
        die_simple("Invalid UTF-8 in bytecode");
    })
}
