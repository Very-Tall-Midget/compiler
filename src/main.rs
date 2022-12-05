mod lexer;
use lexer::*;

mod ast;
use ast::*;

mod generate;
use generate::*;

mod assembly;

use clap::arg;
use std::{
    collections::{HashMap, HashSet},
    fs,
    io::Write,
};

fn cli() -> clap::Command<'static> {
    clap::Command::new("compiler")
        .about("Compiler for a basic version of C/C++")
        .version(clap::crate_version!())
        .arg_required_else_help(true)
        .subcommand_required(true)
        .subcommand(
            clap::Command::new("build")
                .arg(arg!(input: -i <FILE> "Input file").required(true))
                .arg(arg!(output: -o <FILE> "Output file").required(true))
                .arg(arg!(assembly: -S "Output assembly").required(false))
                .arg(
                    arg!(optimise_assembly: -O "Further optimise assembly (experimental)")
                        .required(false),
                )
                .arg_required_else_help(true),
        )
        .subcommand(
            clap::Command::new("check")
                .arg(arg!(input: -i <FILE> "Input file").required(true))
                .arg_required_else_help(true),
        )
}

fn write_assembly(asm: String, file: String) {
    fs::File::create(&file)
        .unwrap_or_else(|_| panic!("Failed to create file '{}'", file))
        .write_all(asm.as_bytes())
        .unwrap_or_else(|_| panic!("Failed to write to file '{}'", file));
}

fn compile_assembly(out: String) -> Result<(), String> {
    if std::process::Command::new("cmd")
        .args([
            "/c",
            "C:\\Program Files\\Microsoft Visual Studio\\2022\\Community\\VC\\Auxiliary\\Build\\vcvars64.bat",
            "&&",
            "nasm",
            "-fwin64",
            "-o",
            "out.obj",
            "assembly.asm",
            "&&",
            "link",
            "/subsystem:console",
            "/entry:_start",
            &format!("/out:{}", out),
            "out.obj",
            "kernel32.lib",
        ])
        .spawn()
        .map_err(|e| e.to_string())?
        .wait()
        .map_err(|e| e.to_string())?.success() {
        let _ = std::fs::remove_file("out.obj");
        Ok(())
    } else {
        Err("Failed to assemble and link".to_string())
    }
}

fn main() -> Result<(), String> {
    let matches = cli().get_matches();
    match matches.subcommand() {
        Some(("build", args)) => {
            let code = fs::read_to_string(args.get_one::<String>("input").unwrap())
                .expect("Failed to read from input file");
            let out = args.get_one::<String>("output").unwrap();

            let res = lex(code)?;
            //println!("{:?}", res);
            let tree = ast(&res)?;
            //println!("{:#?}", tree);
            let mut asm = generate(&tree)?;
            //println!("{}", asm);

            if args.contains_id("optimise_assembly") {
                asm = asm.optimised();
            }
            let asm = asm.to_string()?;

            if args.contains_id("assembly") {
                write_assembly(asm, out.clone());
            } else {
                write_assembly(asm, "assembly.asm".to_string());
                compile_assembly(out.clone())?;
                fs::remove_file("assembly.asm").unwrap();
            }

            println!("Successfully compiled to '{}'", out);
            Ok(())
        }
        Some(("check", args)) => {
            let code = fs::read_to_string(args.get_one::<String>("input").unwrap())
                .expect("Failed to read from input file");

            let res = lex(code)?;
            let tree = ast(&res)?;

            let mut ctx = CheckContext {
                var_map: HashSet::new(),
                func_map: HashMap::new(),
                scope: HashSet::new(),
                break_label: false,
                continue_label: false,
            };
            tree.check(&mut ctx)?;
            println!("No errors found");
            Ok(())
        }
        _ => unreachable!(),
    }
}
