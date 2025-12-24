use std::collections::HashMap;

use crate::ast::{Expr, Stmt, Type, TypedExpr, TypedExprKind, TypedProgram, TypedStmt};
use crate::util::die_simple;

pub fn type_check(program: crate::ast::Program) -> TypedProgram {
    let mut env = HashMap::new();
    let mut stmts = Vec::new();

    for stmt in program.stmts {
        match stmt {
            Stmt::Print(expr) => {
                let typed = type_expr(&expr, &env);
                stmts.push(TypedStmt::Print(typed));
            }
            Stmt::Let { name, expr } => {
                if env.contains_key(&name) {
                    die_simple(&format!("Variable '{}' already defined", name));
                }
                let typed = type_expr(&expr, &env);
                env.insert(name.clone(), typed.ty.clone());
                stmts.push(TypedStmt::Let { name, expr: typed });
            }
        }
    }

    TypedProgram { stmts }
}

fn type_expr(expr: &Expr, env: &HashMap<String, Type>) -> TypedExpr {
    match expr {
        Expr::Int(v) => TypedExpr {
            kind: TypedExprKind::Int(*v),
            ty: Type::Int,
        },
        Expr::Str(s) => TypedExpr {
            kind: TypedExprKind::Str(s.clone()),
            ty: Type::Str,
        },
        Expr::Var(name) => match env.get(name) {
            Some(ty) => TypedExpr {
                kind: TypedExprKind::Var(name.clone()),
                ty: ty.clone(),
            },
            None => die_simple(&format!("Unknown variable '{}'", name)),
        },
        Expr::Binary { left, op, right } => {
            let left_t = type_expr(left, env);
            let right_t = type_expr(right, env);
            match (left_t.ty.clone(), right_t.ty.clone(), op) {
                (Type::Int, Type::Int, _) => TypedExpr {
                    kind: TypedExprKind::Binary {
                        left: Box::new(left_t),
                        op: *op,
                        right: Box::new(right_t),
                    },
                    ty: Type::Int,
                },
                _ => die_simple("Only integer arithmetic is supported in expressions"),
            }
        }
    }
}
