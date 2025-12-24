use std::collections::HashMap;
use std::io::{self, Write};

use crate::bytecode::Instr;
use crate::util::die_simple;

#[derive(Debug, Clone)]
enum Value {
    Int(i64),
    Str(String),
    Bool(bool),
}

pub fn run_bytecode(code: &[Instr]) {
    let mut stdout = io::stdout();
    run_bytecode_with_writer(code, &mut stdout);
}

pub fn run_bytecode_with_writer<W: Write>(code: &[Instr], out: &mut W) {
    let mut stack: Vec<Value> = Vec::new();
    let mut vars: HashMap<String, Value> = HashMap::new();
    let mut ip: usize = 0;
    while ip < code.len() {
        match &code[ip] {
            Instr::PushInt(v) => stack.push(Value::Int(*v)),
            Instr::PushStr(s) => stack.push(Value::Str(s.clone())),
            Instr::PushBool(v) => stack.push(Value::Bool(*v)),
            Instr::ReadLine => {
                out.flush().ok();
                let line = read_line_from_stdin();
                stack.push(Value::Str(line));
            }
            Instr::ToInt => {
                let value = stack.pop().unwrap_or_else(|| {
                    die_simple("Stack underflow on ToInt");
                });
                match value {
                    Value::Int(v) => stack.push(Value::Int(v)),
                    Value::Str(s) => {
                        let trimmed = s.trim();
                        let v: i64 = trimmed.parse().unwrap_or_else(|_| {
                            die_simple("int() failed to parse input");
                        });
                        stack.push(Value::Int(v));
                    }
                    _ => die_simple("Type error in int()"),
                }
            }
            Instr::StrLen => {
                let value = stack.pop().unwrap_or_else(|| {
                    die_simple("Stack underflow on StrLen");
                });
                match value {
                    Value::Str(s) => stack.push(Value::Int(s.chars().count() as i64)),
                    _ => die_simple("Type error in len()"),
                }
            }
            Instr::Abs => {
                let value = stack.pop().unwrap_or_else(|| {
                    die_simple("Stack underflow on Abs");
                });
                match value {
                    Value::Int(v) => stack.push(Value::Int(v.abs())),
                    _ => die_simple("Type error in abs()"),
                }
            }
            Instr::Min => {
                let right = stack.pop().unwrap_or_else(|| {
                    die_simple("Stack underflow on Min");
                });
                let left = stack.pop().unwrap_or_else(|| {
                    die_simple("Stack underflow on Min");
                });
                match (left, right) {
                    (Value::Int(a), Value::Int(b)) => stack.push(Value::Int(a.min(b))),
                    _ => die_simple("Type error in min()"),
                }
            }
            Instr::Max => {
                let right = stack.pop().unwrap_or_else(|| {
                    die_simple("Stack underflow on Max");
                });
                let left = stack.pop().unwrap_or_else(|| {
                    die_simple("Stack underflow on Max");
                });
                match (left, right) {
                    (Value::Int(a), Value::Int(b)) => stack.push(Value::Int(a.max(b))),
                    _ => die_simple("Type error in max()"),
                }
            }
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
            Instr::CmpEq => cmp_eq(&mut stack),
            Instr::CmpNe => cmp_ne(&mut stack),
            Instr::CmpLt => cmp_int(&mut stack, |a, b| a < b),
            Instr::CmpLte => cmp_int(&mut stack, |a, b| a <= b),
            Instr::CmpGt => cmp_int(&mut stack, |a, b| a > b),
            Instr::CmpGte => cmp_int(&mut stack, |a, b| a >= b),
            Instr::Jump(target) => {
                ip = *target;
                continue;
            }
            Instr::JumpIfFalse(target) => {
                let value = stack.pop().unwrap_or_else(|| {
                    die_simple("Stack underflow on JumpIfFalse");
                });
                match value {
                    Value::Bool(false) => {
                        ip = *target;
                        continue;
                    }
                    Value::Bool(true) => {}
                    _ => die_simple("Type error on JumpIfFalse"),
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
            Instr::PrintBool => {
                let value = stack.pop().unwrap_or_else(|| {
                    die_simple("Stack underflow on PrintBool");
                });
                match value {
                    Value::Bool(v) => write_out(out, if v { "true" } else { "false" }),
                    _ => die_simple("Type error on PrintBool"),
                }
            }
            Instr::PrintSpace => write_out(out, " "),
            Instr::PrintNewline => {
                write_out(out, "\n");
                out.flush().ok();
            }
        }
        ip += 1;
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

fn cmp_int(stack: &mut Vec<Value>, op: fn(i64, i64) -> bool) {
    let right = stack.pop().unwrap_or_else(|| {
        die_simple("Stack underflow on comparison");
    });
    let left = stack.pop().unwrap_or_else(|| {
        die_simple("Stack underflow on comparison");
    });
    match (left, right) {
        (Value::Int(a), Value::Int(b)) => stack.push(Value::Bool(op(a, b))),
        _ => die_simple("Type error in comparison"),
    }
}

fn cmp_eq(stack: &mut Vec<Value>) {
    let right = stack.pop().unwrap_or_else(|| {
        die_simple("Stack underflow on comparison");
    });
    let left = stack.pop().unwrap_or_else(|| {
        die_simple("Stack underflow on comparison");
    });
    let result = match (left, right) {
        (Value::Int(a), Value::Int(b)) => a == b,
        (Value::Str(a), Value::Str(b)) => a == b,
        (Value::Bool(a), Value::Bool(b)) => a == b,
        _ => die_simple("Type error in comparison"),
    };
    stack.push(Value::Bool(result));
}

fn cmp_ne(stack: &mut Vec<Value>) {
    let right = stack.pop().unwrap_or_else(|| {
        die_simple("Stack underflow on comparison");
    });
    let left = stack.pop().unwrap_or_else(|| {
        die_simple("Stack underflow on comparison");
    });
    let result = match (left, right) {
        (Value::Int(a), Value::Int(b)) => a != b,
        (Value::Str(a), Value::Str(b)) => a != b,
        (Value::Bool(a), Value::Bool(b)) => a != b,
        _ => die_simple("Type error in comparison"),
    };
    stack.push(Value::Bool(result));
}

fn write_out<W: Write>(out: &mut W, text: &str) {
    out.write_all(text.as_bytes()).unwrap_or_else(|_| {
        die_simple("Failed to write output");
    });
}

fn read_line_from_stdin() -> String {
    let mut line = String::new();
    io::stdin().read_line(&mut line).unwrap_or_else(|_| {
        die_simple("Failed to read input");
    });
    if line.ends_with('\n') {
        line.pop();
        if line.ends_with('\r') {
            line.pop();
        }
    }
    line
}
