use syn::{Expr, Pat, Stmt, Type};

const SOURCE: &str = "
fn sq(y: f32, argument: &mut [u32; 3]) -> f32 {
    let x: [f32; 3] = [y, 1.0, argument[0]];
    let t = change(&mut x);
    return x[0] * t;
}";

const SOURCE2: &str = "
fn len_sq_i32(v: [i32; 3]) -> f32 {
    let mut x = 0;
    for d in 0..v[2] {
        x += (x + 1);
    }
}";

fn main() {
    println!("Converting Rust function to C function:\n");
    let c = convert_function(SOURCE);
}

fn convert_function(source: &str) -> String {
    let c_sig = convert_signature(get_fn_signature(source));
    
    let expr = syn::parse_str::<Expr>(&get_fn_block(source)).unwrap();
    println!("{:#?}", expr);
    let c_block = format!("{}", convert_expr(expr));
    println!("{} {}", c_sig, c_block);

    
    String::new()
}

fn get_fn_block(source: &str) -> String {
    let block = "{".to_string() + source.split_once("{").expect("msg").1;
    block
}

fn get_fn_signature(source: &str) -> String {
    let signature = source.split("{").nth(0).expect("String should contain signature").trim();
    signature.to_string()
}

fn convert_signature(sig: String) -> String {
    let c_return_type = match sig.split("->").nth(1) {
        Some(rs_return_type) => convert_to_c_type(rs_return_type).unwrap(),
        None => "void".to_string(),
    };
    let fn_name: String = sig.split('(').nth(0).expect("msg").replace("fn", "").replace([' ', '\t', '\n'], "");
    let args: Vec<&str> = sig.split('(').nth(1).expect("msg").split(')').nth(0).expect("msg").split(',').collect();
    let mut c_args = String::new();
    for (i, arg) in args.iter().enumerate() {
        let name = arg.split(':').nth(0).expect("msg").trim().to_string();
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
fn convert_to_c_type(rs_type: &str) -> Result<String, String> {
    //return Ok("");
    let san = rs_type.replace([' ', '\t'], "");
    if san.contains("Vec<") { // Type is a Vector, use C pointer
        let rs_buftype  = san.split_once("Vec<").expect("msg").1.split(">").nth(0).expect("msg").to_string();
        match convert_to_c_type(&rs_buftype) {
            Ok(c_type) => return Ok(format!("{}*", c_type)),
            Err(_) => return Err(format!("Error converting Rust vector \"{}\" to C pointer", rs_type)),
        }
    }
    if san.chars().nth(0) == Some('&') { // Is reference, use pointer
        if san.contains('[') { // Type is fixed lenght array ([u32; 3]), use C pointer
            let rs_buftype  = san.split('[').nth(1).expect("").split(';').nth(0).expect("Should contain value").to_string();
            match convert_primitive_type(rs_buftype) {
                Ok(c_type) => return Ok(format!("{}*", c_type)),
                Err(_) => return Err(format!("Error converting Rust vector \"{}\" to C pointer", rs_type)),
            }
        }
    } else { // Moves value
        if san.contains('[') { // Type is fixed lenght array ([u32; 3]), use C pointer
            let rs_arr  = san.split('[').nth(1).expect("");
            let rs_arrtype = rs_arr.split(';').nth(0).expect("Should contain value").to_string();
            let rs_arrlen = rs_arr.split(';').nth(1).expect("msg").split(']').nth(0).expect("msg").to_string();
            match convert_primitive_type(rs_arrtype) {
                Ok(c_type) => return Ok(format!("{} arrname[{}]", c_type, rs_arrlen)),
                Err(_) => return Err(format!("Error converting Rust vector \"{}\" to C pointer", rs_type)),
            }
        }
    }
    if san.contains('[') { // Type is fixed lenght array ([u32; 3]), use C pointer
        let rs_buftype  = san.replace("[", "").split(';').nth(0).expect("Should contain value").to_string();
        match convert_primitive_type(rs_buftype) {
            Ok(c_type) => return Ok(format!("{}*", c_type)),
            Err(_) => return Err(format!("Error converting Rust vector \"{}\" to C pointer", rs_type)),
        }
    }
    convert_primitive_type(san)
}

fn convert_primitive_type(rs_type: String) -> Result<String, String> {
    match rs_type.as_ref() {
        "i8"  => Ok("char".to_string()),
        "i16" => Ok("short".to_string()),
        "i32" => Ok("int".to_string()),
        "i64" => Ok("long".to_string()),
        "u8"  => Ok("unsigned char".to_string()),
        "u16" => Ok("ushort".to_string()),
        "u32" => Ok("uint".to_string()),
        "u64" => Ok("ulong".to_string()),
        "f16" => Ok("half".to_string()),
        "f32" => Ok("float".to_string()),
        "f64" => Ok("double".to_string()),
        _ => Err(format!("Error converting Rust primitive type \"{}\" to C type", rs_type))
    }
}

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
        Expr::Assign(_) => todo!(),
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
                syn::BinOp::BitXor(_) => todo!(),
                syn::BinOp::BitAnd(_) => todo!(),
                syn::BinOp::BitOr(_) => todo!(),
                syn::BinOp::Shl(_) => todo!(),
                syn::BinOp::Shr(_) => todo!(),
                syn::BinOp::Eq(_) => "==",
                syn::BinOp::Lt(_) => "<",
                syn::BinOp::Le(_) => "<=",
                syn::BinOp::Ne(_) => "!=",
                syn::BinOp::Ge(_) => ">=",
                syn::BinOp::Gt(_) => ">",
                syn::BinOp::AddAssign(_) => "+=",
                syn::BinOp::SubAssign(_) => todo!(),
                syn::BinOp::MulAssign(_) => todo!(),
                syn::BinOp::DivAssign(_) => todo!(),
                syn::BinOp::RemAssign(_) => todo!(),
                syn::BinOp::BitXorAssign(_) => todo!(),
                syn::BinOp::BitAndAssign(_) => todo!(),
                syn::BinOp::BitOrAssign(_) => todo!(),
                syn::BinOp::ShlAssign(_) => todo!(),
                syn::BinOp::ShrAssign(_) => todo!(),
                _ => todo!(),
            };
            
            format!("{} {} {}", convert_expr(*binexp.left), op, convert_expr(*binexp.right))
        },
        Expr::Block(block) => {
            let mut stmts = String::new();
            for stmt in block.block.stmts {
                stmts += &convert_stmt(stmt);
            }
            return format!("{{\n{}}}", stmts);
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
        Expr::If(_) => todo!(),
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
                syn::Lit::Char(_) => todo!(),
                syn::Lit::Int(il) => {
                    il.to_string().split(char::is_alphabetic).nth(0).expect("msg").to_string()
                },
                syn::Lit::Float(fl) => {
                    let mut f = fl.to_string().split(char::is_alphabetic).nth(0).expect("msg").to_string();
                    f += "f";
                    f
                },
                syn::Lit::Bool(_) => todo!(),
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
            format!("&{}", convert_expr(*refexp.expr))
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
        Expr::Unary(_) => todo!(),
        Expr::Unsafe(_) => todo!(),
        Expr::Verbatim(_) => todo!(),
        Expr::While(_) => todo!(),
        Expr::Yield(_) => todo!(),
        _ => todo!(),
    }
    //String::new()
}

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
            let ident = convert_pat(*ptype.pat);
            if ctype.contains("arrname") {
                ctype.replace("arrname", &ident)
            } else {
                format!("{} {}", ctype, ident)
            }
        },
        Pat::Verbatim(_) => todo!(),
        Pat::Wild(_) => todo!(),
        _ => todo!(),
    }
}

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
