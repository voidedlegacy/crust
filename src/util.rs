pub fn die(line_no: usize, msg: &str) -> ! {
    eprintln!("Parse error on line {}: {}", line_no + 1, msg);
    std::process::exit(1);
}

pub fn die_simple(msg: &str) -> ! {
    eprintln!("Error: {}", msg);
    std::process::exit(1);
}
