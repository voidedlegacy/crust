#[derive(Debug)]
pub struct Program {
    pub structs: Vec<StructDef>,
    pub functions: Vec<Function>,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<Field>,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub params: Vec<Param>,
    pub ret: Type,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug)]
pub enum Stmt {
    Print(Vec<Expr>),
    Let { name: String, expr: Expr },
    Set { name: String, expr: Expr },
    Use { path: String, alias: Option<String> },
    Return(Expr),
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
    StructInit {
        name: String,
        fields: Vec<(String, Expr)>,
    },
    Field {
        base: Box<Expr>,
        name: String,
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
    Struct(String),
}

#[derive(Debug)]
pub struct TypedProgram {
    pub structs: Vec<StructDef>,
    pub functions: Vec<TypedFunction>,
    pub stmts: Vec<TypedStmt>,
}

#[derive(Debug)]
pub struct TypedFunction {
    pub name: String,
    pub params: Vec<Param>,
    pub ret: Type,
    pub body: Vec<TypedStmt>,
}

#[derive(Debug)]
pub enum TypedStmt {
    Print(Vec<TypedExpr>),
    Let { name: String, expr: TypedExpr },
    Set { name: String, expr: TypedExpr },
    Return(TypedExpr),
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
    StructInit {
        name: String,
        fields: Vec<(String, TypedExpr)>,
    },
    Field {
        base: Box<TypedExpr>,
        name: String,
    },
    Binary {
        left: Box<TypedExpr>,
        op: Op,
        right: Box<TypedExpr>,
    },
}
