mod lexer;
use lexer::*;

mod ast;
use ast::*;

mod generate;
use generate::*;

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
    let mut file = fs::File::create(file).expect("Failed to create file 'assembly.s'");
    file.write_all(asm.as_bytes())
        .expect("Failed to write to file 'assembly.s'");
}

fn compile_assembly(out: String) -> Result<(), String> {
    let res = std::process::Command::new("gcc")
        .arg("assembly.s")
        .arg("-o")
        .arg(out)
        .output()
        .expect("Failed to call gcc");
    if !res.status.success() {
        Err(format!(
            "gcc error: {}",
            String::from_utf8(res.stderr).unwrap()
        ))
    } else {
        Ok(())
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
            //println!("{:?}", tree);
            let asm = generate(&tree, args.contains_id("optimise_assembly"))?;
            //println!("{}", asm);

            if args.contains_id("assembly") {
                write_assembly(asm, out.clone());
            } else {
                write_assembly(asm, "assembly.s".to_string());
                compile_assembly(out.clone())?;
                fs::remove_file("assembly.s").unwrap();
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
