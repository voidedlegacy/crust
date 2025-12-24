use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};

use crate::ast::{Expr, Program, Stmt};
use crate::parser::parse_program;

pub fn load_program(entry: &Path) -> Program {
    let mut visited = HashSet::new();
    let mut stmts = Vec::new();
    if let Some(prelude) = find_prelude(entry) {
        let prelude_program = load_file(&prelude, &mut visited);
        stmts.extend(prelude_program.stmts);
    }
    let program = load_file(entry, &mut visited);
    stmts.extend(program.stmts);
    Program { stmts }
}

fn load_file(path: &Path, visited: &mut HashSet<PathBuf>) -> Program {
    let canonical = path.canonicalize().unwrap_or_else(|e| {
        eprintln!("Failed to resolve {}: {}", path.display(), e);
        std::process::exit(1);
    });
    if !visited.insert(canonical.clone()) {
        return Program { stmts: Vec::new() };
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

fn expand_uses(base: &Path, program: Program, visited: &mut HashSet<PathBuf>) -> Program {
    let mut out = Vec::new();
    for stmt in program.stmts {
        match stmt {
            Stmt::Use { path, alias } => {
                let include_path = resolve_use_path(base, &path);
                let mut module = load_file(&include_path, visited);
                let module_name = alias.unwrap_or_else(|| module_name(&include_path));
                prefix_program(&mut module, &module_name);
                out.extend(module.stmts);
            }
            other => out.push(other),
        }
    }
    Program { stmts: out }
}

fn resolve_use_path(base: &Path, include: &str) -> PathBuf {
    let include_path = PathBuf::from(include);
    if include_path.is_absolute() {
        include_path
    } else {
        base.parent()
            .unwrap_or_else(|| Path::new("."))
            .join(include_path)
    }
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

fn module_name(path: &Path) -> String {
    path.file_stem()
        .and_then(|s| s.to_str())
        .filter(|s| !s.is_empty())
        .map(|s| s.to_string())
        .unwrap_or_else(|| {
            eprintln!("Failed to derive module name from {}", path.display());
            std::process::exit(1);
        })
}

fn prefix_program(program: &mut Program, prefix: &str) {
    for stmt in &mut program.stmts {
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
        Expr::Binary { left, right, .. } => {
            prefix_expr(left, prefix);
            prefix_expr(right, prefix);
        }
        Expr::Int(_) | Expr::Str(_) | Expr::Bool(_) => {}
    }
}
