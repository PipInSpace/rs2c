use crate::*;

/// Transpiles a single Rust function provided in `source` into an OpenCL kernel function
pub fn convert_to_cl_kernel(source: &str) -> String {
    let cl_sig = convert_to_cl_signature(get_fn_signature(source)).unwrap();

    let expr = syn::parse_str::<Expr>(&get_fn_block(source)).unwrap();
    //println!("{:#?}", expr);
    let c_block = convert_expr(expr).to_string();

    format!("{} {}", cl_sig, c_block)
}

/// Converts a Rust function signature into an OpenCL kernel function signature
fn convert_to_cl_signature(sig: String) -> Result<String, String> {
    match sig.split("->").nth(1) {
        Some(_) => {return Err("Kernel functions can't return values.".to_string());},
        None => {},
    };
    let fn_name: String = sig.split('(').next().expect("msg").replace("fn", "").trim().to_string();
    let args: Vec<&str> = sig.split('(').nth(1).expect("msg").split(')').next().expect("msg").split(',').collect();
    let mut c_args = String::new();
    for (i, arg) in args.iter().enumerate() {
        let name = arg.split(':').next().expect("msg").trim().to_string();
        let is_const = !arg.split(':').nth(1).expect("msg").contains("mut ");
        let c_type = convert_to_c_type(&arg.split(':').nth(1).expect("msg").replace("mut ", "")).unwrap();
        let is_buffer = c_type.contains('*');
        if is_const {
            c_args += "const ";
        }
        if is_buffer {
            c_args += "global ";
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
    let c_sig = format!("__kernel void {}({})", fn_name, c_args);
    Ok(c_sig)
}