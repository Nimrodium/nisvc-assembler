mod constant;
mod data;
mod package;
mod program_parser;

struct AssemblyError {
    code: u16,
    reason: Option<String>,
    severity: Severity,
}

fn main() {
    println!("Hello, world!");
}
