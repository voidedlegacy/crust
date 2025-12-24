#[derive(Debug)]
pub struct Program {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub enum Stmt {
    Print(Vec<Expr>),
    Let { name: String, expr: Expr },
    Set { name: String, expr: Expr },
    If {
        cond: Expr,
        then_body: Vec<Stmt>,
        else_body: Vec<Stmt>,
    },
    While { cond: Expr, body: Vec<Stmt> },
}

#[derive(Debug, Clone)]
pub enum Expr {
    Int(i64),
    Str(String),
    Bool(bool),
    Var(String),
    Call {
        name: String,
        args: Vec<Expr>,
    },
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
    Eq,
    Ne,
    Lt,
    Lte,
    Gt,
    Gte,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Str,
    Bool,
}

#[derive(Debug)]
pub struct TypedProgram {
    pub stmts: Vec<TypedStmt>,
}

#[derive(Debug)]
pub enum TypedStmt {
    Print(Vec<TypedExpr>),
    Let { name: String, expr: TypedExpr },
    Set { name: String, expr: TypedExpr },
    If {
        cond: TypedExpr,
        then_body: Vec<TypedStmt>,
        else_body: Vec<TypedStmt>,
    },
    While { cond: TypedExpr, body: Vec<TypedStmt> },
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
    Bool(bool),
    Var(String),
    Call {
        name: String,
        args: Vec<TypedExpr>,
    },
    Binary {
        left: Box<TypedExpr>,
        op: Op,
        right: Box<TypedExpr>,
    },
}
