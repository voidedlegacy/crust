use std::collections::HashMap;

use crate::ast::{
    Expr, Function, Op, Param, Program, StructDef, TypedExpr, TypedExprKind, TypedFunction,
    TypedProgram, TypedStmt, Type,
};
use crate::util::die_simple;

#[derive(Debug, Clone)]
struct FuncSig {
    params: Vec<Type>,
    ret: Type,
}

pub fn type_check(program: Program) -> TypedProgram {
    let struct_map = collect_structs(&program.structs);
    let func_map = collect_functions(&program.functions, &struct_map);

    let mut typed_functions = Vec::new();
    for func in &program.functions {
        typed_functions.push(type_function(func, &struct_map, &func_map));
    }

    let mut env = HashMap::new();
    let stmts = type_block(&program.stmts, &mut env, &struct_map, &func_map, None);

    TypedProgram {
        structs: program.structs,
        functions: typed_functions,
        stmts,
    }
}

fn collect_structs(structs: &[StructDef]) -> HashMap<String, StructDef> {
    let mut map = HashMap::new();
    for def in structs {
        if map.contains_key(&def.name) {
            die_simple(&format!("Struct '{}' already defined", def.name));
        }
        map.insert(def.name.clone(), def.clone());
    }
    map
}

fn collect_functions(
    functions: &[Function],
    struct_map: &HashMap<String, StructDef>,
) -> HashMap<String, FuncSig> {
    let mut map = HashMap::new();
    for func in functions {
        if map.contains_key(&func.name) {
            die_simple(&format!("Function '{}' already defined", func.name));
        }
        for param in &func.params {
            validate_type(&param.ty, struct_map);
        }
        validate_type(&func.ret, struct_map);
        let sig = FuncSig {
            params: func.params.iter().map(|p| p.ty.clone()).collect(),
            ret: func.ret.clone(),
        };
        map.insert(func.name.clone(), sig);
    }
    map
}

fn validate_type(ty: &Type, struct_map: &HashMap<String, StructDef>) {
    if let Type::Struct(name) = ty {
        if !struct_map.contains_key(name) {
            die_simple(&format!("Unknown type '{}'", name));
        }
    }
}

fn type_function(
    func: &Function,
    struct_map: &HashMap<String, StructDef>,
    func_map: &HashMap<String, FuncSig>,
) -> TypedFunction {
    let mut env = HashMap::new();
    for Param { name, ty } in &func.params {
        env.insert(name.clone(), ty.clone());
    }
    let body = type_block(&func.body, &mut env, struct_map, func_map, Some(&func.ret));
    TypedFunction {
        name: func.name.clone(),
        params: func.params.clone(),
        ret: func.ret.clone(),
        body,
    }
}

fn type_block(
    stmts: &[crate::ast::Stmt],
    env: &mut HashMap<String, Type>,
    struct_map: &HashMap<String, StructDef>,
    func_map: &HashMap<String, FuncSig>,
    ret_type: Option<&Type>,
) -> Vec<TypedStmt> {
    let mut out = Vec::new();
    for stmt in stmts {
        match stmt {
            crate::ast::Stmt::Print(exprs) => {
                let mut typed_exprs = Vec::new();
                for expr in exprs {
                    typed_exprs.push(type_expr(expr, env, struct_map, func_map));
                }
                out.push(TypedStmt::Print(typed_exprs));
            }
            crate::ast::Stmt::Let { name, expr } => {
                if env.contains_key(name) {
                    die_simple(&format!("Variable '{}' already defined", name));
                }
                let typed = type_expr(expr, env, struct_map, func_map);
                env.insert(name.clone(), typed.ty.clone());
                out.push(TypedStmt::Let {
                    name: name.clone(),
                    expr: typed,
                });
            }
            crate::ast::Stmt::Set { name, expr } => {
                let existing = env
                    .get(name)
                    .cloned()
                    .unwrap_or_else(|| die_simple(&format!("Unknown variable '{}'", name)));
                let typed = type_expr(expr, env, struct_map, func_map);
                if typed.ty != existing {
                    die_simple(&format!("Type mismatch assigning '{}'", name));
                }
                out.push(TypedStmt::Set {
                    name: name.clone(),
                    expr: typed,
                });
            }
            crate::ast::Stmt::Return(expr) => {
                let ret = ret_type.unwrap_or_else(|| {
                    die_simple("return is only valid inside functions");
                });
                let typed = type_expr(expr, env, struct_map, func_map);
                if &typed.ty != ret {
                    die_simple("return type does not match function signature");
                }
                out.push(TypedStmt::Return(typed));
            }
            crate::ast::Stmt::Use { .. } => {
                die_simple("use statements must be resolved before type checking");
            }
            crate::ast::Stmt::If {
                cond,
                then_body,
                else_body,
            } => {
                let typed_cond = type_expr(cond, env, struct_map, func_map);
                if typed_cond.ty != Type::Bool {
                    die_simple("if condition must be boolean");
                }
                let typed_then =
                    type_block(then_body, &mut env.clone(), struct_map, func_map, ret_type);
                let typed_else =
                    type_block(else_body, &mut env.clone(), struct_map, func_map, ret_type);
                out.push(TypedStmt::If {
                    cond: typed_cond,
                    then_body: typed_then,
                    else_body: typed_else,
                });
            }
            crate::ast::Stmt::While { cond, body } => {
                let typed_cond = type_expr(cond, env, struct_map, func_map);
                if typed_cond.ty != Type::Bool {
                    die_simple("while condition must be boolean");
                }
                let typed_body =
                    type_block(body, &mut env.clone(), struct_map, func_map, ret_type);
                out.push(TypedStmt::While {
                    cond: typed_cond,
                    body: typed_body,
                });
            }
        }
    }
    out
}

fn type_expr(
    expr: &Expr,
    env: &HashMap<String, Type>,
    struct_map: &HashMap<String, StructDef>,
    func_map: &HashMap<String, FuncSig>,
) -> TypedExpr {
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
        Expr::Call { name, args } => {
            if let Some(sig) = func_map.get(name) {
                if sig.params.len() != args.len() {
                    die_simple("Wrong number of arguments in call");
                }
                let mut typed_args = Vec::new();
                for (arg, param_ty) in args.iter().zip(sig.params.iter()) {
                    let typed = type_expr(arg, env, struct_map, func_map);
                    if &typed.ty != param_ty {
                        die_simple("Argument type mismatch in call");
                    }
                    typed_args.push(typed);
                }
                return TypedExpr {
                    kind: TypedExprKind::Call {
                        name: name.clone(),
                        args: typed_args,
                    },
                    ty: sig.ret.clone(),
                };
            }

            let typed_args: Vec<TypedExpr> = args
                .iter()
                .map(|a| type_expr(a, env, struct_map, func_map))
                .collect();
            match name.as_str() {
                "input" | "std::io::read_line" => {
                    if typed_args.len() > 1 {
                        die_simple("read_line() takes zero or one argument");
                    }
                    if typed_args.len() == 1 && typed_args[0].ty != Type::Str {
                        die_simple("read_line() prompt must be a string");
                    }
                    TypedExpr {
                        kind: TypedExprKind::Call {
                            name: name.clone(),
                            args: typed_args,
                        },
                        ty: Type::Str,
                    }
                }
                "int" => {
                    if typed_args.len() != 1 {
                        die_simple("int() expects exactly one argument");
                    }
                    match typed_args[0].ty {
                        Type::Int | Type::Str => {}
                        _ => die_simple("int() expects a string or int argument"),
                    }
                    TypedExpr {
                        kind: TypedExprKind::Call {
                            name: name.clone(),
                            args: typed_args,
                        },
                        ty: Type::Int,
                    }
                }
                "std::io::read_int" => {
                    if typed_args.len() > 1 {
                        die_simple("read_int() takes zero or one argument");
                    }
                    if typed_args.len() == 1 && typed_args[0].ty != Type::Str {
                        die_simple("read_int() prompt must be a string");
                    }
                    TypedExpr {
                        kind: TypedExprKind::Call {
                            name: name.clone(),
                            args: typed_args,
                        },
                        ty: Type::Int,
                    }
                }
                "std::string::len" => {
                    if typed_args.len() != 1 {
                        die_simple("len() expects exactly one argument");
                    }
                    if typed_args[0].ty != Type::Str {
                        die_simple("len() expects a string");
                    }
                    TypedExpr {
                        kind: TypedExprKind::Call {
                            name: name.clone(),
                            args: typed_args,
                        },
                        ty: Type::Int,
                    }
                }
                "std::math::abs" => {
                    if typed_args.len() != 1 {
                        die_simple("abs() expects exactly one argument");
                    }
                    if typed_args[0].ty != Type::Int {
                        die_simple("abs() expects an int");
                    }
                    TypedExpr {
                        kind: TypedExprKind::Call {
                            name: name.clone(),
                            args: typed_args,
                        },
                        ty: Type::Int,
                    }
                }
                "std::math::min" | "std::math::max" => {
                    if typed_args.len() != 2 {
                        die_simple("min/max expects exactly two arguments");
                    }
                    if typed_args[0].ty != Type::Int || typed_args[1].ty != Type::Int {
                        die_simple("min/max expects int arguments");
                    }
                    TypedExpr {
                        kind: TypedExprKind::Call {
                            name: name.clone(),
                            args: typed_args,
                        },
                        ty: Type::Int,
                    }
                }
                _ => die_simple(&format!("Unknown function '{}'", name)),
            }
        }
        Expr::StructInit { name, fields } => {
            let def = struct_map
                .get(name)
                .unwrap_or_else(|| die_simple(&format!("Unknown struct '{}'", name)));
            let mut field_map = HashMap::new();
            for field in &def.fields {
                field_map.insert(field.name.clone(), field.ty.clone());
            }
            let mut typed_fields = Vec::new();
            for (field_name, expr) in fields {
                let expected = field_map.get(field_name).unwrap_or_else(|| {
                    die_simple(&format!("Unknown field '{}' on '{}'", field_name, name))
                });
                let typed = type_expr(expr, env, struct_map, func_map);
                if &typed.ty != expected {
                    die_simple("Struct field type mismatch");
                }
                typed_fields.push((field_name.clone(), typed));
                field_map.remove(field_name);
            }
            if !field_map.is_empty() {
                die_simple("Missing fields in struct initializer");
            }
            TypedExpr {
                kind: TypedExprKind::StructInit {
                    name: name.clone(),
                    fields: typed_fields,
                },
                ty: Type::Struct(name.clone()),
            }
        }
        Expr::Field { base, name } => {
            let typed_base = type_expr(base, env, struct_map, func_map);
            let struct_name = match &typed_base.ty {
                Type::Struct(s) => s.clone(),
                _ => die_simple("Field access on non-struct"),
            };
            let def = struct_map
                .get(&struct_name)
                .unwrap_or_else(|| die_simple(&format!("Unknown struct '{}'", struct_name)));
            let field_ty = def
                .fields
                .iter()
                .find(|f| f.name == *name)
                .map(|f| f.ty.clone())
                .unwrap_or_else(|| die_simple("Unknown field"));
            TypedExpr {
                kind: TypedExprKind::Field {
                    base: Box::new(typed_base),
                    name: name.clone(),
                },
                ty: field_ty,
            }
        }
        Expr::Binary { left, op, right } => {
            let left_t = type_expr(left, env, struct_map, func_map);
            let right_t = type_expr(right, env, struct_map, func_map);
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
