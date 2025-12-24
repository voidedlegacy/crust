#[derive(Debug)]
pub struct Program {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub enum Stmt {
    Print(Expr),
    Let { name: String, expr: Expr },
}

#[derive(Debug, Clone)]
pub enum Expr {
    Int(i64),
    Str(String),
    Var(String),
    Binary {
        left: Box<Expr>,
        op: Op,
        right: Box<Expr>,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone)]
pub enum Type {
    Int,
    Str,
}

#[derive(Debug)]
pub struct TypedProgram {
    pub stmts: Vec<TypedStmt>,
}

#[derive(Debug)]
pub enum TypedStmt {
    Print(TypedExpr),
    Let { name: String, expr: TypedExpr },
}

#[derive(Debug, Clone)]
pub struct TypedExpr {
    pub kind: TypedExprKind,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub enum TypedExprKind {
    Int(i64),
    Str(String),
    Var(String),
    Binary {
        left: Box<TypedExpr>,
        op: Op,
        right: Box<TypedExpr>,
    },
}
