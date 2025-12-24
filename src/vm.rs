use std::collections::HashMap;

use crate::bytecode::Instr;
use crate::util::die_simple;

#[derive(Debug, Clone)]
enum Value {
    Int(i64),
    Str(String),
}

pub fn run_bytecode(code: &[Instr]) {
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
            Instr::PrintInt => {
                let value = stack.pop().unwrap_or_else(|| {
                    die_simple("Stack underflow on PrintInt");
                });
                match value {
                    Value::Int(v) => println!("{}", v),
                    _ => die_simple("Type error on PrintInt"),
                }
            }
            Instr::PrintStr => {
                let value = stack.pop().unwrap_or_else(|| {
                    die_simple("Stack underflow on PrintStr");
                });
                match value {
                    Value::Str(s) => println!("{}", s),
                    _ => die_simple("Type error on PrintStr"),
                }
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
