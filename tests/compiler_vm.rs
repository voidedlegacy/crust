use crustc::ast::{Expr, Stmt, Type, TypedExprKind, TypedStmt};
use crustc::bytecode::{compile_to_bytecode, Instr};
use crustc::parser::parse_program;
use crustc::typer::type_check;
use crustc::vm::run_bytecode_with_writer;

#[test]
fn parse_print_list() {
    let program = parse_program("print 1, 2\n");
    match program.stmts.as_slice() {
        [Stmt::Print(exprs)] => match exprs.as_slice() {
            [Expr::Int(1), Expr::Int(2)] => {}
            _ => panic!("Unexpected print expressions: {:?}", exprs),
        },
        _ => panic!("Expected a single print statement"),
    }
}

#[test]
fn type_check_string_concat() {
    let program = parse_program("let x = \"a\" + \"b\"\nprint x\n");
    let typed = type_check(program);
    match typed.stmts.as_slice() {
        [TypedStmt::Let { expr, .. }, TypedStmt::Print(exprs)] => {
            assert!(matches!(expr.ty, Type::Str));
            assert!(matches!(expr.kind, TypedExprKind::Binary { .. }));
            assert_eq!(exprs.len(), 1);
        }
        _ => panic!("Unexpected typed program shape"),
    }
}

#[test]
fn bytecode_print_spacing() {
    let program = parse_program("print \"a\", \"b\"\n");
    let typed = type_check(program);
    let code = compile_to_bytecode(&typed);
    let expected = vec![
        Instr::PushStr("a".to_string()),
        Instr::PrintStr,
        Instr::PrintSpace,
        Instr::PushStr("b".to_string()),
        Instr::PrintStr,
        Instr::PrintNewline,
    ];
    assert_eq!(code, expected);
}

#[test]
fn vm_runs_and_formats_output() {
    let program = parse_program(
        "let first = \"Hello\"\n\
         let second = \"world\"\n\
         let message = first + \", \" + second + \"!\"\n\
         print message, 123\n",
    );
    let typed = type_check(program);
    let code = compile_to_bytecode(&typed);
    let mut out = Vec::new();
    run_bytecode_with_writer(&code, &mut out);
    let output = String::from_utf8(out).expect("utf8");
    assert_eq!(output, "Hello, world! 123\n");
}

#[test]
fn vm_arithmetic_output() {
    let program = parse_program("print 1 + 2 * 3\n");
    let typed = type_check(program);
    let code = compile_to_bytecode(&typed);
    let mut out = Vec::new();
    run_bytecode_with_writer(&code, &mut out);
    let output = String::from_utf8(out).expect("utf8");
    assert_eq!(output, "7\n");
}

#[test]
fn vm_multiple_prints() {
    let program = parse_program("let x = 6\nlet y = 7\nprint x * y\nprint \"done\"\n");
    let typed = type_check(program);
    let code = compile_to_bytecode(&typed);
    let mut out = Vec::new();
    run_bytecode_with_writer(&code, &mut out);
    let output = String::from_utf8(out).expect("utf8");
    assert_eq!(output, "42\ndone\n");
}

#[test]
fn vm_if_else() {
    let program = parse_program(
        "let x = 3\n\
         if x > 2:\n\
         print \"yes\"\n\
         else:\n\
         print \"no\"\n\
         end\n",
    );
    let typed = type_check(program);
    let code = compile_to_bytecode(&typed);
    let mut out = Vec::new();
    run_bytecode_with_writer(&code, &mut out);
    let output = String::from_utf8(out).expect("utf8");
    assert_eq!(output, "yes\n");
}

#[test]
fn vm_while_loop() {
    let program = parse_program(
        "let i = 0\n\
         while i < 3:\n\
         print i\n\
         let i = i + 1\n\
         end\n",
    );
    let typed = type_check(program);
    let code = compile_to_bytecode(&typed);
    let mut out = Vec::new();
    run_bytecode_with_writer(&code, &mut out);
    let output = String::from_utf8(out).expect("utf8");
    assert_eq!(output, "0\n1\n2\n");
}
