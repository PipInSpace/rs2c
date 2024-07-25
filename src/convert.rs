use syn::{Expr, Pat, Stmt, Type};

#[rustfmt::skip]
/// Converts Rust data types into C data types. Compatible with Vectors, Arrays and primitive data types.
pub fn convert_to_c_type(rs_type: &str) -> Result<String, String> {
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
pub fn convert_to_c_primitive_type(rs_type: String) -> Result<String, String> {
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
        "isize" => Ok("long".to_string()),
        "usize" => Ok("unsigned long".to_string()),
        _ => Err(format!("Error converting Rust primitive type \"{}\" to C type", rs_type))
    }
}

// -- Recursively traverse syntax tree and build C source code --
// The syn crate is used to build a Rust syntax tree from a Rust function's source code block. This syntax tree
// is traversed and a C source code is build by recursively interpreting Rust syntax.
//
/// Recursively transpiles Expressions from Rust to C. Returns a source string.
pub fn convert_expr(expr: Expr) -> String {
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
                None => format!("if ({}) {{\n{}}}", convert_expr(*ifexp.cond), stmts),
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
                    let val = fl.to_string().split(char::is_alphabetic).next().expect("msg").split('_').next().expect("msg").to_string();
                    if fl.to_string().contains("f32") { // C float
                        format!("{}f", val)
                    } else if fl.to_string().contains("f128") { // C long double
                        format!("{}l", val)
                    } else {
                        format!("{}", val)
                    }
                    
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
        Expr::MethodCall(methexp) => { // Method calls are handled by passing the receiver as the first C function arg
            let mut args = String::new();
            args += &convert_expr(*methexp.receiver);
            for arg in methexp.args {
                args += ", ";
                args += &convert_expr(arg.clone());
            }
            format!("{}({})", methexp.method.to_string(), args)
        },
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
        Expr::Repeat(repexp) => {
            format!("{{{}}}", convert_expr(*repexp.expr))
        },
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
        Expr::While(whileexp) => {
            let cond: String = convert_expr(*whileexp.cond);
            let mut stmts = String::new();
            for stmt in whileexp.body.stmts {
                stmts += &convert_stmt(stmt);
            }
            format!("while ({}) {{\n{}}}", cond, stmts)
        },
        Expr::Yield(_) => todo!(),
        _ => todo!(),
    }
    //String::new()
}

/// Recursively transpiles Statements from Rust to C
pub fn convert_stmt(stmt: Stmt) -> String {
    match stmt {
        syn::Stmt::Local(local) => {
            match local.init {
                Some(linit) => format!("{} = {};\n", convert_pat(local.pat), convert_expr(*linit.expr)),
                None => format!("{};\n", convert_pat(local.pat)),
            }
        },
        syn::Stmt::Item(_) => todo!(),
        syn::Stmt::Expr(expr, semi) => {
            match semi {
                Some(_) => format!("{};\n", convert_expr(expr)),
                None => format!("{}\n", convert_expr(expr)),
            }
        },
        syn::Stmt::Macro(_) => todo!(),
    }
}

/// Recursively transpiles Patterns from Rust to C
pub fn convert_pat(pat: Pat) -> String {
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
pub fn convert_type(ty: Type) -> String {
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
