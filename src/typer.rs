use std::collections::HashMap;

use crate::ast::{Expr, Op, Stmt, Type, TypedExpr, TypedExprKind, TypedProgram, TypedStmt};
use crate::util::die_simple;

pub fn type_check(program: crate::ast::Program) -> TypedProgram {
    let mut env = HashMap::new();
    let mut stmts = Vec::new();

    for stmt in program.stmts {
        match stmt {
            Stmt::Print(exprs) => {
                let mut typed_exprs = Vec::new();
                for expr in exprs {
                    typed_exprs.push(type_expr(&expr, &env));
                }
                stmts.push(TypedStmt::Print(typed_exprs));
            }
            Stmt::Let { name, expr } => {
                let typed = type_expr(&expr, &env);
                match env.get(&name) {
                    Some(existing) if *existing != typed.ty => {
                        die_simple(&format!("Type mismatch assigning '{}'", name));
                    }
                    _ => {}
                }
                env.insert(name.clone(), typed.ty.clone());
                stmts.push(TypedStmt::Let { name, expr: typed });
            }
            Stmt::If {
                cond,
                then_body,
                else_body,
            } => {
                let typed_cond = type_expr(&cond, &env);
                if typed_cond.ty != Type::Bool {
                    die_simple("if condition must be boolean");
                }
                let typed_then = type_block(&then_body, env.clone());
                let typed_else = type_block(&else_body, env.clone());
                stmts.push(TypedStmt::If {
                    cond: typed_cond,
                    then_body: typed_then,
                    else_body: typed_else,
                });
            }
            Stmt::While { cond, body } => {
                let typed_cond = type_expr(&cond, &env);
                if typed_cond.ty != Type::Bool {
                    die_simple("while condition must be boolean");
                }
                let typed_body = type_block(&body, env.clone());
                stmts.push(TypedStmt::While {
                    cond: typed_cond,
                    body: typed_body,
                });
            }
        }
    }

    TypedProgram { stmts }
}

fn type_block(stmts: &[Stmt], mut env: HashMap<String, Type>) -> Vec<TypedStmt> {
    let mut out = Vec::new();
    for stmt in stmts {
        match stmt {
            Stmt::Print(exprs) => {
                let mut typed_exprs = Vec::new();
                for expr in exprs {
                    typed_exprs.push(type_expr(expr, &env));
                }
                out.push(TypedStmt::Print(typed_exprs));
            }
            Stmt::Let { name, expr } => {
                let typed = type_expr(expr, &env);
                match env.get(name) {
                    Some(existing) if *existing != typed.ty => {
                        die_simple(&format!("Type mismatch assigning '{}'", name));
                    }
                    _ => {}
                }
                env.insert(name.clone(), typed.ty.clone());
                out.push(TypedStmt::Let {
                    name: name.clone(),
                    expr: typed,
                });
            }
            Stmt::If {
                cond,
                then_body,
                else_body,
            } => {
                let typed_cond = type_expr(cond, &env);
                if typed_cond.ty != Type::Bool {
                    die_simple("if condition must be boolean");
                }
                let typed_then = type_block(then_body, env.clone());
                let typed_else = type_block(else_body, env.clone());
                out.push(TypedStmt::If {
                    cond: typed_cond,
                    then_body: typed_then,
                    else_body: typed_else,
                });
            }
            Stmt::While { cond, body } => {
                let typed_cond = type_expr(cond, &env);
                if typed_cond.ty != Type::Bool {
                    die_simple("while condition must be boolean");
                }
                let typed_body = type_block(body, env.clone());
                out.push(TypedStmt::While {
                    cond: typed_cond,
                    body: typed_body,
                });
            }
        }
    }
    out
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
        Expr::Bool(v) => TypedExpr {
            kind: TypedExprKind::Bool(*v),
            ty: Type::Bool,
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
            let (ty, ok) = match (left_t.ty.clone(), right_t.ty.clone(), op) {
                (Type::Int, Type::Int, Op::Add)
                | (Type::Int, Type::Int, Op::Sub)
                | (Type::Int, Type::Int, Op::Mul)
                | (Type::Int, Type::Int, Op::Div) => (Type::Int, true),
                (Type::Str, Type::Str, Op::Add) => (Type::Str, true),
                (Type::Int, Type::Int, Op::Eq)
                | (Type::Int, Type::Int, Op::Ne)
                | (Type::Int, Type::Int, Op::Lt)
                | (Type::Int, Type::Int, Op::Lte)
                | (Type::Int, Type::Int, Op::Gt)
                | (Type::Int, Type::Int, Op::Gte) => (Type::Bool, true),
                (Type::Str, Type::Str, Op::Eq) | (Type::Str, Type::Str, Op::Ne) => {
                    (Type::Bool, true)
                }
                (Type::Bool, Type::Bool, Op::Eq) | (Type::Bool, Type::Bool, Op::Ne) => {
                    (Type::Bool, true)
                }
                _ => (Type::Int, false),
            };
            if !ok {
                die_simple("Type error in expression");
            }
            TypedExpr {
                kind: TypedExprKind::Binary {
                    left: Box::new(left_t),
                    op: *op,
                    right: Box::new(right_t),
                },
                ty,
            }
        }
    }
}
