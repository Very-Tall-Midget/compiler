mod lexer;
use lexer::*;

mod ast;
use ast::*;

mod generate;
use generate::*;

use std::env;
use std::process::Command;
use std::{fs, io::Write};

fn get_code_and_out_file() -> Result<(String, String), String> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        return Err("Usage: compiler <filename> <out>".to_string());
    }

    let filename = &args[1];
    let out = args[2].clone();

    let code = fs::read_to_string(filename).expect("Failed to read from input file");

    Ok((code, out))
}

fn write_assembly(asm: String) {
    let mut file = fs::File::create("assembly.s").expect("Failed to create file 'assembly.s'");
    file.write_all(asm.as_bytes())
        .expect("Failed to write to file 'assembly.s'");
}

fn compile_assembly(out: String) {
    Command::new("gcc")
        .arg("assembly.s")
        .arg("-o")
        .arg(out)
        .output()
        .expect("Failed to call gcc");
}

fn main() -> Result<(), String> {
    let (code, out) = get_code_and_out_file()?;

    let res = lex(&code)?;
    //println!("{:?}", res);
    let tree = ast(&res)?;
    //println!("{:#?}", tree);
    let asm = generate(&tree);
    println!("{}", asm);

    write_assembly(asm);
    compile_assembly(out);
    fs::remove_file("assembly.s").unwrap();

    Ok(())
}
