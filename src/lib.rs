//! # rs2c
//! 
//! rs2c is a very WIP Rust to C/OpenCL C transpiler. Far from all Rust functionality is implemented currently (see todo!()s).
//! This transpiler uses the language similarities between Rust and C to build C source code from a Rust
//! syntax tree. Basic language features will become available over time, but complex features like generics
//! or macros will likely not be possible without actual code analysis.
//! 
//! R2C includes an optional feature `opencl` to transpile Rust functions into OpenCL kernels.
//! 
//! More information available on GitHub: https://github.com/PipInSpace/rs2c

use syn::Expr;

mod convert;
#[cfg(test)]
mod tests;
#[cfg(feature = "opencl")]
pub mod opencl;

use convert::*;

/// Transpiles a single Rust function provided in `source` to C
pub fn convert_to_c_function(source: &str) -> String {
    let c_sig = convert_to_c_signature(get_fn_signature(source));
    
    let expr = syn::parse_str::<Expr>(&get_fn_block(source)).unwrap();
    //println!("{:#?}", expr);
    let c_block = convert_expr(expr).to_string();

    format!("{} {}", c_sig, c_block)
}

/// Converts a Rust function signature into a C function signature
pub fn convert_to_c_signature(sig: String) -> String {
    let c_return_type = match sig.split("->").nth(1) {
        Some(rs_return_type) => convert_to_c_type(rs_return_type).unwrap(),
        None => "void".to_string(),
    };
    let fn_name: String = sig.split('(').next().expect("msg").replace("fn", "").replace([' ', '\t', '\n'], "");
    let args: Vec<&str> = sig.split('(').nth(1).expect("msg").split(')').next().expect("msg").split(',').collect();
    let mut c_args = String::new();
    for (i, arg) in args.iter().enumerate() {
        let name = arg.split(':').next().expect("msg").trim().to_string();
        let is_const = !arg.split(':').nth(1).expect("msg").contains("mut ");
        let c_type = convert_to_c_type(&arg.split(':').nth(1).expect("msg").replace("mut ", "")).unwrap();
        if is_const {
            c_args += "const ";
        }
        if c_type.contains("arrname") {
            c_args += &c_type.replace("arrname", &name);
        } else {
            c_args += &format!("{} {}", c_type, name);
        }
        if i != args.len()-1 {
            c_args += ", ";
        }
    }
    let c_sig = format!("{} {}({})", c_return_type, fn_name, c_args);
    c_sig
}

/// Returns the function block from a Rust function
fn get_fn_block(source: &str) -> String {
    let block = "{".to_string() + source.split_once('{').expect("msg").1;
    block
}

/// Returns the function signature from a Rust function
fn get_fn_signature(source: &str) -> String {
    let signature = source.split('{').next().expect("String should contain signature").trim();
    signature.to_string()
}

/// Provides indentation for C source code
pub fn indent_c(source: String) -> String {
    let lines: Vec<&str> = source.split('\n').collect();
    let mut new_source = String::new(); 
    let mut ind = 0;
    for line in lines {
        if line.trim().chars().next() == Some('}') {
            for _ in 1..ind {new_source += "    ";}
        } else {
            for _ in 0..ind {new_source += "    ";}
        }
        new_source += line;
        new_source += "\n";
        ind += line.matches('{').count();
        ind -= line.matches('}').count()
    }
    new_source.trim().to_string()
}
