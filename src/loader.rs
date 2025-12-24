use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};

use crate::ast::{Expr, Function, Program, Stmt, StructDef, Type};
use crate::parser::parse_program;

pub fn load_program(entry: &Path) -> Program {
    let mut visited: HashSet<(PathBuf, String)> = HashSet::new();
    let mut structs = Vec::new();
    let mut functions = Vec::new();
    let mut stmts = Vec::new();
    if let Some(prelude) = find_prelude(entry) {
        let prelude_program = load_file(&prelude, "", &mut visited);
        structs.extend(prelude_program.structs);
        functions.extend(prelude_program.functions);
        stmts.extend(prelude_program.stmts);
    }
    let program = load_file(entry, "", &mut visited);
    structs.extend(program.structs);
    functions.extend(program.functions);
    stmts.extend(program.stmts);
    Program {
        structs,
        functions,
        stmts,
    }
}

fn load_file(
    path: &Path,
    module_name: &str,
    visited: &mut HashSet<(PathBuf, String)>,
) -> Program {
    let canonical = path.canonicalize().unwrap_or_else(|e| {
        eprintln!("Failed to resolve {}: {}", path.display(), e);
        std::process::exit(1);
    });
    let key = (canonical.clone(), module_name.to_string());
    if !visited.insert(key) {
        return Program {
            structs: Vec::new(),
            functions: Vec::new(),
            stmts: Vec::new(),
        };
    }
    let source = read_file(&canonical);
    let program = parse_program(&source);
    expand_uses(&canonical, program, visited)
}

fn read_file(path: &Path) -> String {
    fs::read_to_string(path).unwrap_or_else(|e| {
        eprintln!("Failed to read {}: {}", path.display(), e);
        std::process::exit(1);
    })
}

fn expand_uses(
    base: &Path,
    program: Program,
    visited: &mut HashSet<(PathBuf, String)>,
) -> Program {
    let mut structs = program.structs;
    let mut functions = program.functions;
    let mut stmts = Vec::new();

    for stmt in program.stmts {
        match stmt {
            Stmt::Use { path, alias } => {
                let include_path = resolve_use_path(base, &path);
                let module_name = alias.unwrap_or_else(|| module_alias(&path, &include_path));
                let mut module = load_file(&include_path, &module_name, visited);
                prefix_program(&mut module, &module_name);
                structs.extend(module.structs);
                functions.extend(module.functions);
                stmts.extend(module.stmts);
            }
            other => stmts.push(other),
        }
    }
    Program {
        structs,
        functions,
        stmts,
    }
}

fn resolve_use_path(base: &Path, include: &str) -> PathBuf {
    let include_path = PathBuf::from(include);
    if include_path.is_absolute() {
        include_path
    } else if include.contains("::") {
        let relative = include.replace("::", "/") + ".crust";
        if let Some(found) = search_ancestors(base.parent(), &relative) {
            return found;
        }
        if let Ok(cwd) = std::env::current_dir() {
            let candidate = cwd.join(&relative);
            if candidate.exists() {
                return candidate;
            }
        }
        base.parent()
            .unwrap_or_else(|| Path::new("."))
            .join(relative)
    } else {
        base.parent()
            .unwrap_or_else(|| Path::new("."))
            .join(include_path)
    }
}

fn search_ancestors(start: Option<&Path>, relative: &str) -> Option<PathBuf> {
    let mut current = start.map(|p| p.to_path_buf());
    while let Some(dir) = current {
        let candidate = dir.join(relative);
        if candidate.exists() {
            return Some(candidate);
        }
        current = dir.parent().map(|p| p.to_path_buf());
    }
    None
}

fn find_prelude(entry: &Path) -> Option<PathBuf> {
    let entry_dir = entry.parent().unwrap_or_else(|| Path::new("."));
    let candidate = entry_dir.join("std").join("prelude.crust");
    if candidate.exists() {
        return Some(candidate);
    }
    let cwd = std::env::current_dir().ok()?;
    let candidate = cwd.join("std").join("prelude.crust");
    if candidate.exists() {
        return Some(candidate);
    }
    None
}

fn module_alias(path: &str, resolved: &Path) -> String {
    if path.contains("::") {
        return path.to_string();
    }
    resolved
        .file_stem()
        .and_then(|s| s.to_str())
        .filter(|s| !s.is_empty())
        .map(|s| s.to_string())
        .unwrap_or_else(|| {
            eprintln!("Failed to derive module name from {}", resolved.display());
            std::process::exit(1);
        })
}

fn prefix_program(program: &mut Program, prefix: &str) {
    for st in &mut program.structs {
        prefix_struct_def(st, prefix);
    }
    for func in &mut program.functions {
        prefix_function(func, prefix);
    }
    for stmt in &mut program.stmts {
        prefix_stmt(stmt, prefix);
    }
}

fn prefix_struct_def(def: &mut StructDef, prefix: &str) {
    if !def.name.contains("::") {
        def.name = format!("{}::{}", prefix, def.name);
    }
    for field in &mut def.fields {
        prefix_type(&mut field.ty, prefix);
    }
}

fn prefix_function(func: &mut Function, prefix: &str) {
    if !func.name.contains("::") {
        func.name = format!("{}::{}", prefix, func.name);
    }
    for param in &mut func.params {
        prefix_type(&mut param.ty, prefix);
        if !param.name.contains("::") {
            param.name = format!("{}::{}", prefix, param.name);
        }
    }
    prefix_type(&mut func.ret, prefix);
    for stmt in &mut func.body {
        prefix_stmt(stmt, prefix);
    }
}

fn prefix_stmt(stmt: &mut Stmt, prefix: &str) {
    match stmt {
        Stmt::Print(exprs) => {
            for expr in exprs {
                prefix_expr(expr, prefix);
            }
        }
        Stmt::Let { name, expr } | Stmt::Set { name, expr } => {
            if !name.contains("::") {
                *name = format!("{}::{}", prefix, name);
            }
            prefix_expr(expr, prefix);
        }
        Stmt::Return(expr) => {
            prefix_expr(expr, prefix);
        }
        Stmt::If {
            cond,
            then_body,
            else_body,
        } => {
            prefix_expr(cond, prefix);
            for stmt in then_body {
                prefix_stmt(stmt, prefix);
            }
            for stmt in else_body {
                prefix_stmt(stmt, prefix);
            }
        }
        Stmt::While { cond, body } => {
            prefix_expr(cond, prefix);
            for stmt in body {
                prefix_stmt(stmt, prefix);
            }
        }
        Stmt::Use { .. } => {}
    }
}

fn prefix_expr(expr: &mut Expr, prefix: &str) {
    match expr {
        Expr::Var(name) => {
            if !name.contains("::") {
                *name = format!("{}::{}", prefix, name);
            }
        }
        Expr::Call { name, args } => {
            if name != "input" && name != "int" && !name.contains("::") {
                *name = format!("{}::{}", prefix, name);
            }
            for arg in args {
                prefix_expr(arg, prefix);
            }
        }
        Expr::StructInit { name, fields } => {
            if !name.contains("::") {
                *name = format!("{}::{}", prefix, name);
            }
            for (_, expr) in fields {
                prefix_expr(expr, prefix);
            }
        }
        Expr::Field { base, .. } => {
            prefix_expr(base, prefix);
        }
        Expr::Binary { left, right, .. } => {
            prefix_expr(left, prefix);
            prefix_expr(right, prefix);
        }
        Expr::Int(_) | Expr::Str(_) | Expr::Bool(_) => {}
    }
}

fn prefix_type(ty: &mut Type, prefix: &str) {
    if let Type::Struct(name) = ty {
        if !name.contains("::") {
            *name = format!("{}::{}", prefix, name);
        }
    }
}
