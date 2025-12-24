use std::fs::File;
use std::io::{Read, Write};
use std::path::Path;

use std::collections::HashSet;

use crate::ast::{
    Op, Type, TypedExpr, TypedExprKind, TypedFunction, TypedProgram, TypedStmt,
};
use crate::util::die_simple;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instr {
    PushInt(i64),
    PushStr(String),
    PushBool(bool),
    ReadLine,
    ToInt,
    StrLen,
    Abs,
    Min,
    Max,
    FuncDef {
        name: String,
        params: Vec<String>,
    },
    Call {
        name: String,
        argc: usize,
    },
    Return,
    Halt,
    MakeStruct {
        name: String,
        fields: Vec<String>,
    },
    GetField {
        name: String,
    },
    LoadVar(String),
    StoreVar(String),
    Add,
    Sub,
    Mul,
    Div,
    ConcatStr,
    CmpEq,
    CmpNe,
    CmpLt,
    CmpLte,
    CmpGt,
    CmpGte,
    Jump(usize),
    JumpIfFalse(usize),
    PrintInt,
    PrintStr,
    PrintBool,
    PrintSpace,
    PrintNewline,
}

pub fn compile_to_bytecode(program: &TypedProgram) -> Vec<Instr> {
    let func_names: HashSet<String> = program
        .functions
        .iter()
        .map(|f| f.name.clone())
        .collect();
    let mut out = Vec::new();
    for stmt in &program.stmts {
        emit_stmt(stmt, &mut out, &func_names);
    }
    out.push(Instr::Halt);
    for func in &program.functions {
        emit_function(func, &mut out, &func_names);
    }
    out
}

fn emit_function(func: &TypedFunction, out: &mut Vec<Instr>, func_names: &HashSet<String>) {
    let params = func.params.iter().map(|p| p.name.clone()).collect();
    out.push(Instr::FuncDef {
        name: func.name.clone(),
        params,
    });
    for stmt in &func.body {
        emit_stmt(stmt, out, func_names);
    }
    if !matches!(func.body.last(), Some(TypedStmt::Return(_))) {
        match func.ret {
            Type::Int => out.push(Instr::PushInt(0)),
            Type::Str => out.push(Instr::PushStr(String::new())),
            Type::Bool => out.push(Instr::PushBool(false)),
            Type::Struct(_) => die_simple("Missing return in function"),
        }
        out.push(Instr::Return);
    }
}

fn emit_stmt(stmt: &TypedStmt, out: &mut Vec<Instr>, func_names: &HashSet<String>) {
    match stmt {
        TypedStmt::Print(exprs) => {
            for (idx, expr) in exprs.iter().enumerate() {
                emit_expr(expr, out, func_names);
                match expr.ty {
                    Type::Int => out.push(Instr::PrintInt),
                    Type::Str => out.push(Instr::PrintStr),
                    Type::Bool => out.push(Instr::PrintBool),
                    Type::Struct(_) => die_simple("Cannot print struct values"),
                }
                if idx + 1 < exprs.len() {
                    out.push(Instr::PrintSpace);
                }
            }
            out.push(Instr::PrintNewline);
        }
        TypedStmt::Let { name, expr } => {
            emit_expr(expr, out, func_names);
            out.push(Instr::StoreVar(name.clone()));
        }
        TypedStmt::Set { name, expr } => {
            emit_expr(expr, out, func_names);
            out.push(Instr::StoreVar(name.clone()));
        }
        TypedStmt::Return(expr) => {
            emit_expr(expr, out, func_names);
            out.push(Instr::Return);
        }
        TypedStmt::If {
            cond,
            then_body,
            else_body,
        } => {
            emit_expr(cond, out, func_names);
            let jump_false_pos = out.len();
            out.push(Instr::JumpIfFalse(0));
            for stmt in then_body {
                emit_stmt(stmt, out, func_names);
            }
            if else_body.is_empty() {
                let end = out.len();
                patch_jump(out, jump_false_pos, end);
            } else {
                let jump_end_pos = out.len();
                out.push(Instr::Jump(0));
                let else_start = out.len();
                patch_jump(out, jump_false_pos, else_start);
                for stmt in else_body {
                    emit_stmt(stmt, out, func_names);
                }
                let end = out.len();
                patch_jump(out, jump_end_pos, end);
            }
        }
        TypedStmt::While { cond, body } => {
            let loop_start = out.len();
            emit_expr(cond, out, func_names);
            let jump_false_pos = out.len();
            out.push(Instr::JumpIfFalse(0));
            for stmt in body {
                emit_stmt(stmt, out, func_names);
            }
            out.push(Instr::Jump(loop_start));
            let end = out.len();
            patch_jump(out, jump_false_pos, end);
        }
    }
}

fn emit_expr(expr: &TypedExpr, out: &mut Vec<Instr>, func_names: &HashSet<String>) {
    match &expr.kind {
        TypedExprKind::Int(v) => out.push(Instr::PushInt(*v)),
        TypedExprKind::Str(s) => out.push(Instr::PushStr(s.clone())),
        TypedExprKind::Bool(v) => out.push(Instr::PushBool(*v)),
        TypedExprKind::Var(name) => out.push(Instr::LoadVar(name.clone())),
        TypedExprKind::StructInit { name, fields } => {
            for (_, expr) in fields {
                emit_expr(expr, out, func_names);
            }
            let names = fields.iter().map(|(n, _)| n.clone()).collect();
            out.push(Instr::MakeStruct {
                name: name.clone(),
                fields: names,
            });
        }
        TypedExprKind::Field { base, name } => {
            emit_expr(base, out, func_names);
            out.push(Instr::GetField { name: name.clone() });
        }
        TypedExprKind::Call { name, args } => {
            if func_names.contains(name) {
                for arg in args {
                    emit_expr(arg, out, func_names);
                }
                out.push(Instr::Call {
                    name: name.clone(),
                    argc: args.len(),
                });
                return;
            }
            match name.as_str() {
            "input" | "std::io::read_line" => {
                if args.len() > 1 {
                    die_simple("read_line() takes zero or one argument");
                }
                if args.len() == 1 {
                    emit_expr(&args[0], out, func_names);
                    out.push(Instr::PrintStr);
                }
                out.push(Instr::ReadLine);
            }
            "int" => {
                if args.len() != 1 {
                    die_simple("int() expects exactly one argument");
                }
                emit_expr(&args[0], out, func_names);
                out.push(Instr::ToInt);
            }
            "std::io::read_int" => {
                if args.len() > 1 {
                    die_simple("read_int() takes zero or one argument");
                }
                if args.len() == 1 {
                    emit_expr(&args[0], out, func_names);
                    out.push(Instr::PrintStr);
                }
                out.push(Instr::ReadLine);
                out.push(Instr::ToInt);
            }
            "std::string::len" => {
                if args.len() != 1 {
                    die_simple("len() expects exactly one argument");
                }
                emit_expr(&args[0], out, func_names);
                out.push(Instr::StrLen);
            }
            "std::math::abs" => {
                if args.len() != 1 {
                    die_simple("abs() expects exactly one argument");
                }
                emit_expr(&args[0], out, func_names);
                out.push(Instr::Abs);
            }
            "std::math::min" => {
                if args.len() != 2 {
                    die_simple("min() expects exactly two arguments");
                }
                emit_expr(&args[0], out, func_names);
                emit_expr(&args[1], out, func_names);
                out.push(Instr::Min);
            }
            "std::math::max" => {
                if args.len() != 2 {
                    die_simple("max() expects exactly two arguments");
                }
                emit_expr(&args[0], out, func_names);
                emit_expr(&args[1], out, func_names);
                out.push(Instr::Max);
            }
            _ => die_simple(&format!("Unknown function '{}'", name)),
            }
        }
        TypedExprKind::Binary { left, op, right } => {
            emit_expr(left, out, func_names);
            emit_expr(right, out, func_names);
            out.push(match op {
                Op::Add => {
                    if expr.ty == Type::Str {
                        Instr::ConcatStr
                    } else {
                        Instr::Add
                    }
                }
                Op::Sub => Instr::Sub,
                Op::Mul => Instr::Mul,
                Op::Div => Instr::Div,
                Op::Eq => Instr::CmpEq,
                Op::Ne => Instr::CmpNe,
                Op::Lt => Instr::CmpLt,
                Op::Lte => Instr::CmpLte,
                Op::Gt => Instr::CmpGt,
                Op::Gte => Instr::CmpGte,
            });
        }
    }
}

fn patch_jump(out: &mut [Instr], pos: usize, target: usize) {
    match out.get_mut(pos) {
        Some(Instr::JumpIfFalse(t)) => *t = target,
        Some(Instr::Jump(t)) => *t = target,
        _ => die_simple("Invalid jump patch"),
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
    file.write_all(&[6]).unwrap_or_else(|e| {
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
    if version != 1 && version != 2 && version != 3 && version != 4 && version != 5 && version != 6 {
        die_simple("Unsupported bytecode version");
    }
    let count = read_u32(&mut file);
    let mut code = Vec::with_capacity(count as usize);
    for _ in 0..count {
        if version == 1 {
            let instr = read_instr_v1(&mut file);
            match instr {
                Instr::PrintInt | Instr::PrintStr => {
                    code.push(instr);
                    code.push(Instr::PrintNewline);
                }
                _ => code.push(instr),
            }
        } else if version == 2 {
            code.push(read_instr_v2(&mut file));
        } else if version == 3 {
            code.push(read_instr_v3(&mut file));
        } else if version == 4 {
            code.push(read_instr_v4(&mut file));
        } else if version == 5 {
            code.push(read_instr_v5(&mut file));
        } else {
            code.push(read_instr_v6(&mut file));
        }
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
        Instr::PushBool(v) => {
            write_u8(file, 14);
            write_u8(file, if *v { 1 } else { 0 });
        }
        Instr::ReadLine => write_u8(file, 24),
        Instr::ToInt => write_u8(file, 25),
        Instr::StrLen => write_u8(file, 26),
        Instr::Abs => write_u8(file, 27),
        Instr::Min => write_u8(file, 28),
        Instr::Max => write_u8(file, 29),
        Instr::FuncDef { name, params } => {
            write_u8(file, 30);
            write_string(file, name);
            write_u32(file, params.len() as u32);
            for param in params {
                write_string(file, param);
            }
        }
        Instr::Call { name, argc } => {
            write_u8(file, 31);
            write_string(file, name);
            write_u32(file, *argc as u32);
        }
        Instr::Return => write_u8(file, 32),
        Instr::Halt => write_u8(file, 33),
        Instr::MakeStruct { name, fields } => {
            write_u8(file, 34);
            write_string(file, name);
            write_u32(file, fields.len() as u32);
            for field in fields {
                write_string(file, field);
            }
        }
        Instr::GetField { name } => {
            write_u8(file, 35);
            write_string(file, name);
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
        Instr::ConcatStr => write_u8(file, 9),
        Instr::PrintInt => write_u8(file, 10),
        Instr::PrintStr => write_u8(file, 11),
        Instr::PrintSpace => write_u8(file, 12),
        Instr::PrintNewline => write_u8(file, 13),
        Instr::CmpEq => write_u8(file, 15),
        Instr::CmpNe => write_u8(file, 16),
        Instr::CmpLt => write_u8(file, 17),
        Instr::CmpLte => write_u8(file, 18),
        Instr::CmpGt => write_u8(file, 19),
        Instr::CmpGte => write_u8(file, 20),
        Instr::Jump(target) => {
            write_u8(file, 21);
            write_u32(file, *target as u32);
        }
        Instr::JumpIfFalse(target) => {
            write_u8(file, 22);
            write_u32(file, *target as u32);
        }
        Instr::PrintBool => write_u8(file, 23),
    }
}

fn read_instr_v1(file: &mut File) -> Instr {
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

fn read_instr_v2(file: &mut File) -> Instr {
    match read_u8(file) {
        1 => Instr::PushInt(read_i64(file)),
        2 => Instr::PushStr(read_string(file)),
        3 => Instr::LoadVar(read_string(file)),
        4 => Instr::StoreVar(read_string(file)),
        5 => Instr::Add,
        6 => Instr::Sub,
        7 => Instr::Mul,
        8 => Instr::Div,
        9 => Instr::ConcatStr,
        10 => Instr::PrintInt,
        11 => Instr::PrintStr,
        12 => Instr::PrintSpace,
        13 => Instr::PrintNewline,
        _ => die_simple("Invalid bytecode instruction"),
    }
}

fn read_instr_v3(file: &mut File) -> Instr {
    match read_u8(file) {
        1 => Instr::PushInt(read_i64(file)),
        2 => Instr::PushStr(read_string(file)),
        3 => Instr::LoadVar(read_string(file)),
        4 => Instr::StoreVar(read_string(file)),
        5 => Instr::Add,
        6 => Instr::Sub,
        7 => Instr::Mul,
        8 => Instr::Div,
        9 => Instr::ConcatStr,
        10 => Instr::PrintInt,
        11 => Instr::PrintStr,
        12 => Instr::PrintSpace,
        13 => Instr::PrintNewline,
        14 => Instr::PushBool(read_u8(file) != 0),
        15 => Instr::CmpEq,
        16 => Instr::CmpNe,
        17 => Instr::CmpLt,
        18 => Instr::CmpLte,
        19 => Instr::CmpGt,
        20 => Instr::CmpGte,
        21 => Instr::Jump(read_u32(file) as usize),
        22 => Instr::JumpIfFalse(read_u32(file) as usize),
        23 => Instr::PrintBool,
        _ => die_simple("Invalid bytecode instruction"),
    }
}

fn read_instr_v4(file: &mut File) -> Instr {
    match read_u8(file) {
        1 => Instr::PushInt(read_i64(file)),
        2 => Instr::PushStr(read_string(file)),
        3 => Instr::LoadVar(read_string(file)),
        4 => Instr::StoreVar(read_string(file)),
        5 => Instr::Add,
        6 => Instr::Sub,
        7 => Instr::Mul,
        8 => Instr::Div,
        9 => Instr::ConcatStr,
        10 => Instr::PrintInt,
        11 => Instr::PrintStr,
        12 => Instr::PrintSpace,
        13 => Instr::PrintNewline,
        14 => Instr::PushBool(read_u8(file) != 0),
        15 => Instr::CmpEq,
        16 => Instr::CmpNe,
        17 => Instr::CmpLt,
        18 => Instr::CmpLte,
        19 => Instr::CmpGt,
        20 => Instr::CmpGte,
        21 => Instr::Jump(read_u32(file) as usize),
        22 => Instr::JumpIfFalse(read_u32(file) as usize),
        23 => Instr::PrintBool,
        24 => Instr::ReadLine,
        25 => Instr::ToInt,
        _ => die_simple("Invalid bytecode instruction"),
    }
}

fn read_instr_v5(file: &mut File) -> Instr {
    match read_u8(file) {
        1 => Instr::PushInt(read_i64(file)),
        2 => Instr::PushStr(read_string(file)),
        3 => Instr::LoadVar(read_string(file)),
        4 => Instr::StoreVar(read_string(file)),
        5 => Instr::Add,
        6 => Instr::Sub,
        7 => Instr::Mul,
        8 => Instr::Div,
        9 => Instr::ConcatStr,
        10 => Instr::PrintInt,
        11 => Instr::PrintStr,
        12 => Instr::PrintSpace,
        13 => Instr::PrintNewline,
        14 => Instr::PushBool(read_u8(file) != 0),
        15 => Instr::CmpEq,
        16 => Instr::CmpNe,
        17 => Instr::CmpLt,
        18 => Instr::CmpLte,
        19 => Instr::CmpGt,
        20 => Instr::CmpGte,
        21 => Instr::Jump(read_u32(file) as usize),
        22 => Instr::JumpIfFalse(read_u32(file) as usize),
        23 => Instr::PrintBool,
        24 => Instr::ReadLine,
        25 => Instr::ToInt,
        26 => Instr::StrLen,
        27 => Instr::Abs,
        28 => Instr::Min,
        29 => Instr::Max,
        _ => die_simple("Invalid bytecode instruction"),
    }
}

fn read_instr_v6(file: &mut File) -> Instr {
    match read_u8(file) {
        1 => Instr::PushInt(read_i64(file)),
        2 => Instr::PushStr(read_string(file)),
        3 => Instr::LoadVar(read_string(file)),
        4 => Instr::StoreVar(read_string(file)),
        5 => Instr::Add,
        6 => Instr::Sub,
        7 => Instr::Mul,
        8 => Instr::Div,
        9 => Instr::ConcatStr,
        10 => Instr::PrintInt,
        11 => Instr::PrintStr,
        12 => Instr::PrintSpace,
        13 => Instr::PrintNewline,
        14 => Instr::PushBool(read_u8(file) != 0),
        15 => Instr::CmpEq,
        16 => Instr::CmpNe,
        17 => Instr::CmpLt,
        18 => Instr::CmpLte,
        19 => Instr::CmpGt,
        20 => Instr::CmpGte,
        21 => Instr::Jump(read_u32(file) as usize),
        22 => Instr::JumpIfFalse(read_u32(file) as usize),
        23 => Instr::PrintBool,
        24 => Instr::ReadLine,
        25 => Instr::ToInt,
        26 => Instr::StrLen,
        27 => Instr::Abs,
        28 => Instr::Min,
        29 => Instr::Max,
        30 => {
            let name = read_string(file);
            let count = read_u32(file);
            let mut params = Vec::with_capacity(count as usize);
            for _ in 0..count {
                params.push(read_string(file));
            }
            Instr::FuncDef { name, params }
        }
        31 => {
            let name = read_string(file);
            let argc = read_u32(file) as usize;
            Instr::Call { name, argc }
        }
        32 => Instr::Return,
        33 => Instr::Halt,
        34 => {
            let name = read_string(file);
            let count = read_u32(file);
            let mut fields = Vec::with_capacity(count as usize);
            for _ in 0..count {
                fields.push(read_string(file));
            }
            Instr::MakeStruct { name, fields }
        }
        35 => Instr::GetField {
            name: read_string(file),
        },
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
