use syn::{Expr, Pat, Stmt, Type};

#[allow(dead_code)]
/// Simple Rust Code for verification
const SOURCE: &str = "
fn foo(y: f32, argument: &mut [u32; 3]) -> f32 {
    if n>=def_N {
        return;
    } else if n = 2{ 
        barfoo();
    } else {
        foobar();
    }
    let x: [f32; 3] = [y, 1.0, argument[0] as f32];
    let t: f32 = bar(&mut x);
    return x[0] * t;
}";

#[allow(dead_code)]
/// Complex Rust code for verification 
/// Taken from https://github.com/PipInSpace/IonSolver/blob/main/src/kernels/sim_kernels.cl
const SOURCE2: &str = "
fn update_fields(fi: &Vec<f32>, rho: &mut Vec<f32>, u: &mut Vec<f32>, flags: &Vec<u8>, t: u64, fx: f32, fy: f32, fz: f32) {
    let n: u32 = get_global_id(0); // n = x+(y+z*Ny)*Nx
    if n>=def_N as u32 || is_halo(n) { return; } // don't execute update_fields() on halo
    let flagsn: u8 = flags[n];
    let flagsn_bo: u8=flagsn&TYPE_BO;
    let flagsn_su: u8=flagsn&TYPE_SU; // extract boundary and surface flags
    if flagsn_bo==TYPE_S || flagsn_su==TYPE_G { return; } // don't update fields for boundary or gas lattice points

    let mut j: [u32; def_velocity_set]; // neighbor indices
    neighbors(n, &mut j); // calculate neighbor indices
    let mut fhn: [f32; def_velocity_set]; // local DDFs
    load_f(n, &mut fhn, &fi, &j, t); // perform streaming (part 2)

    // calculate local density and velocity for collision
    let mut rhon: f32;
    let mut uxn: f32;
    let mut uyn: f32;
    let mut uzn: f32;
    calculate_rho_u(fhn, &mut rhon, &mut uxn, &mut uyn, &uzn); // calculate density and velocity fields from fi
    let fxn: f32 = fx;
    let fyn: f32 = fy;
    let fzn: f32 = fz; // force starts as constant volume force, can be modified before call of calculate_forcing_terms(...)
    {
        uxn = clamp(uxn, -def_c, def_c); // limit velocity (for stability purposes)
        uyn = clamp(uyn, -def_c, def_c); // force term: F*dt/(2*rho)
        uzn = clamp(uzn, -def_c, def_c);
    }

    rho[               n] = rhon; // update density field
    u[                 n] = uxn; // update velocity field
    u[    def_N+n as u64] = uyn;
    u[2  *def_N+n as u64] = uzn;
} // update_fields()";

fn main() {
    println!("Converting Rust function to C function:\n");
    let c = convert_to_c_function(SOURCE);
    println!("{}", indent_c(c))
}

/// Transpiles a single Rust function provided in `source` to C
fn convert_to_c_function(source: &str) -> String {
    let c_sig = convert_signature(get_fn_signature(source));
    
    let expr = syn::parse_str::<Expr>(&get_fn_block(source)).unwrap();
    println!("{:#?}", expr);
    let c_block = convert_expr(expr).to_string();

    format!("{} {}", c_sig, c_block)
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

/// Converts a Rust function signature into a C function signature
fn convert_signature(sig: String) -> String {
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

#[rustfmt::skip]
/// Converts Rust data types into C data types. Compatible with Vectors, Arrays and primitive data types.
fn convert_to_c_type(rs_type: &str) -> Result<String, String> {
    //return Ok("");
    let san = rs_type.replace([' ', '\t'], "");
    if san.contains("Vec<") { // Type is a Vector, use C pointer
        let rs_buftype  = san.split_once("Vec<").expect("msg").1.split('>').next().expect("msg").to_string();
        match convert_to_c_type(&rs_buftype) {
            Ok(c_type) => return Ok(format!("{}*", c_type)),
            Err(_) => return Err(format!("Error converting Rust vector \"{}\" to C pointer", rs_type)),
        }
    }
    if san.starts_with('&') { // Is reference, use pointer
        if san.contains('[') { // Type is fixed lenght array ([u32; 3]), use C pointer
            let rs_buftype  = san.split('[').nth(1).expect("").split(';').next().expect("Should contain value").to_string();
            match convert_to_c_primitive_type(rs_buftype) {
                Ok(c_type) => return Ok(format!("{}*", c_type)),
                Err(_) => return Err(format!("Error converting Rust vector \"{}\" to C pointer", rs_type)),
            }
        }
    } else { // Moves value
        if san.contains('[') { // Type is fixed lenght array ([u32; 3]), use C pointer
            let rs_arr  = san.split('[').nth(1).expect("");
            let rs_arrtype = rs_arr.split(';').next().expect("Should contain value").to_string();
            let rs_arrlen = rs_arr.split(';').nth(1).expect("msg").split(']').next().expect("msg").to_string();
            match convert_to_c_primitive_type(rs_arrtype) {
                Ok(c_type) => return Ok(format!("{} arrname[{}]", c_type, rs_arrlen)),
                Err(_) => return Err(format!("Error converting Rust vector \"{}\" to C pointer", rs_type)),
            }
        }
    }
    if san.contains('[') { // Type is fixed lenght array ([u32; 3]), use C pointer
        let rs_buftype  = san.replace('[', "").split(';').next().expect("Should contain value").to_string();
        match convert_to_c_primitive_type(rs_buftype) {
            Ok(c_type) => return Ok(format!("{}*", c_type)),
            Err(_) => return Err(format!("Error converting Rust vector \"{}\" to C pointer", rs_type)),
        }
    }
    convert_to_c_primitive_type(san)
}

/// Converts Rust primitive data types into C primitive data types.
fn convert_to_c_primitive_type(rs_type: String) -> Result<String, String> {
    match rs_type.as_ref() {
        "i8"  => Ok("char".to_string()),
        "i16" => Ok("short".to_string()),
        "i32" => Ok("int".to_string()),
        "i64" => Ok("long".to_string()),
        "u8"  => Ok("unsigned char".to_string()),
        "u16" => Ok("unsigned short".to_string()),
        "u32" => Ok("unsigned int".to_string()),
        "u64" => Ok("unsigned long".to_string()),
        "f16" => Ok("half".to_string()),
        "f32" => Ok("float".to_string()),
        "f64" => Ok("double".to_string()),
        _ => Err(format!("Error converting Rust primitive type \"{}\" to C type", rs_type))
    }
}

// -- Recursively traverse syntax tree and build C source code --
// Far from all Rust functionality is implemented currently (see todo!()s). This transpiler uses the language similarities between 
// Rust and C to build C source code from a Rust syntax tree. Basic language features will become available over time, but complex
// features like generics or macros will likely not be possible without actual code analysis.
//
/// Recursively transpiles Expressions from Rust to C. Returns a source string.
fn convert_expr(expr: Expr) -> String {
    match expr {
        Expr::Array(arr) => {
            let mut elems = String::new();
            for (i, exp) in arr.elems.iter().enumerate() {
                elems += &convert_expr(exp.clone());
                if i != arr.elems.len() - 1 {
                    elems += ", "
                }
            }

            format!("{{{}}}", elems)
        },
        Expr::Assign(assexp) => {
            format!("{} = {}", convert_expr(*assexp.left), convert_expr(*assexp.right))
        },
        Expr::Async(_) => todo!(),
        Expr::Await(_) => todo!(),
        Expr::Binary(binexp) => {
            let op = match binexp.op {
                syn::BinOp::Add(_) => "+",
                syn::BinOp::Sub(_) => "-",
                syn::BinOp::Mul(_) => "*",
                syn::BinOp::Div(_) => "/",
                syn::BinOp::Rem(_) => "%",
                syn::BinOp::And(_) => "&&",
                syn::BinOp::Or(_) => "||",
                syn::BinOp::BitXor(_) => "^",
                syn::BinOp::BitAnd(_) => "&",
                syn::BinOp::BitOr(_) => "|",
                syn::BinOp::Shl(_) => "<<",
                syn::BinOp::Shr(_) => ">>",
                syn::BinOp::Eq(_) => "==",
                syn::BinOp::Lt(_) => "<",
                syn::BinOp::Le(_) => "<=",
                syn::BinOp::Ne(_) => "!=",
                syn::BinOp::Ge(_) => ">=",
                syn::BinOp::Gt(_) => ">",
                syn::BinOp::AddAssign(_) => "+=",
                syn::BinOp::SubAssign(_) => "-=",
                syn::BinOp::MulAssign(_) => "*=",
                syn::BinOp::DivAssign(_) => "/=",
                syn::BinOp::RemAssign(_) => "%=",
                syn::BinOp::BitXorAssign(_) => "^=",
                syn::BinOp::BitAndAssign(_) => "&=",
                syn::BinOp::BitOrAssign(_) => "|=",
                syn::BinOp::ShlAssign(_) => "<<=",
                syn::BinOp::ShrAssign(_) => ">>=",
                _ => todo!(),
            };
            
            format!("{} {} {}", convert_expr(*binexp.left), op, convert_expr(*binexp.right))
        },
        Expr::Block(block) => {
            let mut stmts = String::new();
            for stmt in block.block.stmts {
                stmts += &convert_stmt(stmt);
            }
            format!("{{\n{}}}", stmts)
        },
        Expr::Break(_) => todo!(),
        Expr::Call(callexp) => {
            let mut args = String::new();
            for (i, arg) in callexp.args.iter().enumerate() {
                args += &convert_expr(arg.clone());
                if i != callexp.args.len() - 1 {
                    args += ", ";
                }
            }
            format!("{}({})", convert_expr(*callexp.func), args)
        },
        Expr::Cast(castexp) => {
            format!("({}){}", convert_to_c_type(&convert_type(*castexp.ty)).unwrap(), convert_expr(*castexp.expr))
        },
        Expr::Closure(_) => todo!(),
        Expr::Const(_) => todo!(),
        Expr::Continue(_) => todo!(),
        Expr::Field(_) => todo!(),
        Expr::ForLoop(forexp) => {
            let ind_name = convert_pat(*forexp.pat);
            let range = convert_expr(*forexp.expr).replace("ind_name", &ind_name);
            let mut stmts = String::new();
            for stmt in forexp.body.stmts {
                stmts += &convert_stmt(stmt);
            }
            format!("for ({}) {{\n{}}}", range, stmts)
        },
        Expr::Group(_) => todo!(),
        Expr::If(ifexp) => {
            let mut stmts = String::new();
            for stmt in ifexp.then_branch.stmts {
                stmts += &convert_stmt(stmt);
            }
            match ifexp.else_branch {
                Some(branch) => format!("if ({}) {{\n{}}} else {}", convert_expr(*ifexp.cond), stmts, convert_expr(*branch.1)),
                None => format!("if ({}) {{{}}}", convert_expr(*ifexp.cond), stmts),
            }
        },
        Expr::Index(ind) => {
            format!("{}[{}]", convert_expr(*ind.expr), convert_expr(*ind.index))
        },
        Expr::Infer(_) => todo!(),
        Expr::Let(_) => todo!(),
        Expr::Lit(elit) => {
            match elit.lit {
                syn::Lit::Str(_) => todo!(),
                syn::Lit::ByteStr(_) => todo!(),
                syn::Lit::CStr(_) => todo!(),
                syn::Lit::Byte(_) => todo!(),
                syn::Lit::Char(cl) => {
                    format!("'{}'", cl.value())
                },
                syn::Lit::Int(il) => {
                    il.to_string().split(char::is_alphabetic).next().expect("msg").to_string()
                },
                syn::Lit::Float(fl) => {
                    format!("{}f", fl.to_string().split(char::is_alphabetic).next().expect("msg"))
                },
                syn::Lit::Bool(bl) => {
                    bl.value.to_string()
                },
                syn::Lit::Verbatim(_) => todo!(),
                _ => todo!(),
            }
        },
        Expr::Loop(_) => todo!(),
        Expr::Macro(_) => todo!(),
        Expr::Match(_) => todo!(),
        Expr::MethodCall(_) => todo!(),
        Expr::Paren(parexp) => {
            format!("({})", convert_expr(*parexp.expr))
        },
        Expr::Path(pathexp) => {
            format!("{}", pathexp.path.segments[0].ident)
        },
        Expr::Range(rangeexp) => {
            let s = convert_expr(*rangeexp.start.expect("Start of range should be set"));
            let e = convert_expr(*rangeexp.end.expect("End of range should be set"));
            match rangeexp.limits {
                syn::RangeLimits::HalfOpen(_) => format!("int ind_name = {}; ind_name < {}; ++ind_name", s, e),
                syn::RangeLimits::Closed(_) => format!("int ind_name = {}; ind_name <= {}; ++ind_name", s, e),
            }
        },
        Expr::Reference(refexp) => {
            convert_expr(*refexp.expr).to_string()
        },
        Expr::Repeat(_) => todo!(),
        Expr::Return(rtrnexp) => {
            match rtrnexp.expr {
                Some(expr) => format!("return {}", convert_expr(*expr)),
                None => "return".to_string(),
            }
        },
        Expr::Struct(_) => todo!(),
        Expr::Try(_) => todo!(),
        Expr::TryBlock(_) => todo!(),
        Expr::Tuple(_) => todo!(),
        Expr::Unary(unexp) => {
            let op = match unexp.op {
                syn::UnOp::Deref(_) => "*",
                syn::UnOp::Not(_) => "!",
                syn::UnOp::Neg(_) => "-",
                _ => todo!(),
            };
            format!("{}{}", op, convert_expr(*unexp.expr))
        },
        Expr::Unsafe(_) => todo!(),
        Expr::Verbatim(_) => todo!(),
        Expr::While(_) => todo!(),
        Expr::Yield(_) => todo!(),
        _ => todo!(),
    }
    //String::new()
}

/// Recursively transpiles Statements from Rust to C
fn convert_stmt(stmt: Stmt) -> String {
    match stmt {
        syn::Stmt::Local(local) => {
            match local.init {
                Some(linit) => format!("{} = {};\n", convert_pat(local.pat), convert_expr(*linit.expr)),
                None => format!("{};\n", convert_pat(local.pat)),
            }
        },
        syn::Stmt::Item(_) => todo!(),
        syn::Stmt::Expr(expr, _) => {
            format!("{};\n", convert_expr(expr))
        },
        syn::Stmt::Macro(_) => todo!(),
    }
}

/// Recursively transpiles Patterns from Rust to C
fn convert_pat(pat: Pat) -> String {
    match pat {
        Pat::Const(_) => todo!(),
        Pat::Ident(ident) => {
            ident.ident.to_string()
        },
        Pat::Lit(_) => todo!(),
        Pat::Macro(_) => todo!(),
        Pat::Or(_) => todo!(),
        Pat::Paren(_) => todo!(),
        Pat::Path(_) => todo!(),
        Pat::Range(_) => todo!(),
        Pat::Reference(_) => todo!(),
        Pat::Rest(_) => todo!(),
        Pat::Slice(_) => todo!(),
        Pat::Struct(_) => todo!(),
        Pat::Tuple(_) => todo!(),
        Pat::TupleStruct(_) => todo!(),
        Pat::Type(ptype) => {
            let ctype = convert_to_c_type(&convert_type(*ptype.ty)).unwrap();
            let ident = convert_pat(*ptype.pat.clone());
            let is_const = match *ptype.pat {
                Pat::Ident(i) => i.mutability.is_none(),
                _ => true,
            };
            let mut consts = String::new();
            if is_const { consts += "const "; }
            if ctype.contains("arrname") {
                format!("{}{}", consts, ctype.replace("arrname", &ident))
            } else {
                format!("{}{} {}", consts, ctype, ident)
            }
        },
        Pat::Verbatim(_) => todo!(),
        Pat::Wild(_) => todo!(),
        _ => todo!(),
    }
}

/// Recursively transpiles Types from Rust to C
fn convert_type(ty: Type) -> String {
    match ty {
        Type::Array(arr) => {
           format!("[{}; {}]", convert_type(*arr.elem), convert_expr(arr.len))
        },
        Type::BareFn(_) => todo!(),
        Type::Group(_) => todo!(),
        Type::ImplTrait(_) => todo!(),
        Type::Infer(_) => todo!(),
        Type::Macro(_) => todo!(),
        Type::Never(_) => todo!(),
        Type::Paren(_) => todo!(),
        Type::Path(path) => {
            path.path.segments[0].ident.to_string()
        },
        Type::Ptr(_) => todo!(),
        Type::Reference(_) => todo!(),
        Type::Slice(_) => todo!(),
        Type::TraitObject(_) => todo!(),
        Type::Tuple(_) => todo!(),
        Type::Verbatim(_) => todo!(),
        _ => todo!(),
    }
}

/// Provides indentation for C code
fn indent_c(source: String) -> String {
    let lines: Vec<&str> = source.split('\n').collect();
    let mut new_source = String::new(); 
    let mut ind = 0;
    for line in lines {
        if line.trim().chars().next() == Some('}') {
            for _ in 1..ind {new_source += "\t";}
        } else {
            for _ in 0..ind {new_source += "\t";}
        }
        new_source += line;
        new_source += "\n";
        ind += line.matches('{').count();
        ind -= line.matches('}').count()
    }
    new_source
}