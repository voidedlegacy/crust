use std::collections::HashMap;
use std::io::{self, Write};

use crate::bytecode::Instr;
use crate::util::die_simple;

#[derive(Debug, Clone)]
enum Value {
    Int(i64),
    Str(String),
}

pub fn run_bytecode(code: &[Instr]) {
    let mut stdout = io::stdout();
    run_bytecode_with_writer(code, &mut stdout);
}

pub fn run_bytecode_with_writer<W: Write>(code: &[Instr], out: &mut W) {
    let mut stack: Vec<Value> = Vec::new();
    let mut vars: HashMap<String, Value> = HashMap::new();

    for instr in code {
        match instr {
            Instr::PushInt(v) => stack.push(Value::Int(*v)),
            Instr::PushStr(s) => stack.push(Value::Str(s.clone())),
            Instr::LoadVar(name) => {
                let value = vars
                    .get(name)
                    .cloned()
                    .unwrap_or_else(|| die_simple(&format!("Unknown variable '{}'", name)));
                stack.push(value);
            }
            Instr::StoreVar(name) => {
                let value = stack.pop().unwrap_or_else(|| {
                    die_simple("Stack underflow on StoreVar");
                });
                vars.insert(name.clone(), value);
            }
            Instr::Add => bin_int_op(&mut stack, |a, b| a + b),
            Instr::Sub => bin_int_op(&mut stack, |a, b| a - b),
            Instr::Mul => bin_int_op(&mut stack, |a, b| a * b),
            Instr::Div => bin_int_op(&mut stack, |a, b| a / b),
            Instr::ConcatStr => {
                let right = stack.pop().unwrap_or_else(|| {
                    die_simple("Stack underflow on ConcatStr");
                });
                let left = stack.pop().unwrap_or_else(|| {
                    die_simple("Stack underflow on ConcatStr");
                });
                match (left, right) {
                    (Value::Str(a), Value::Str(b)) => stack.push(Value::Str(a + &b)),
                    _ => die_simple("Type error in string concatenation"),
                }
            }
            Instr::PrintInt => {
                let value = stack.pop().unwrap_or_else(|| {
                    die_simple("Stack underflow on PrintInt");
                });
                match value {
                    Value::Int(v) => write_out(out, &v.to_string()),
                    _ => die_simple("Type error on PrintInt"),
                }
            }
            Instr::PrintStr => {
                let value = stack.pop().unwrap_or_else(|| {
                    die_simple("Stack underflow on PrintStr");
                });
                match value {
                    Value::Str(s) => write_out(out, &s),
                    _ => die_simple("Type error on PrintStr"),
                }
            }
            Instr::PrintSpace => write_out(out, " "),
            Instr::PrintNewline => {
                write_out(out, "\n");
                out.flush().ok();
            }
        }
    }
}

fn bin_int_op(stack: &mut Vec<Value>, op: fn(i64, i64) -> i64) {
    let right = stack.pop().unwrap_or_else(|| {
        die_simple("Stack underflow on binary op");
    });
    let left = stack.pop().unwrap_or_else(|| {
        die_simple("Stack underflow on binary op");
    });
    match (left, right) {
        (Value::Int(a), Value::Int(b)) => stack.push(Value::Int(op(a, b))),
        _ => die_simple("Type error in arithmetic"),
    }
}

fn write_out<W: Write>(out: &mut W, text: &str) {
    out.write_all(text.as_bytes()).unwrap_or_else(|_| {
        die_simple("Failed to write output");
    });
}
