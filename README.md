#### [Documentation](https://docs.rs/rs2c) | [Change Log](https://github.com/pipinspace/rs2c/blob/main/CHANGELOG.md)

[![](https://img.shields.io/crates/v/rs2c.svg)](https://crates.io/crates/rs2c) [![](https://docs.rs/rs2c/badge.svg)](https://docs.rs/rs2c)

# rs2c

A very WIP Rust to C transpiler. Far from all Rust functionality is implemented currently (see todo!()s).
This transpiler uses the language similarities between Rust and C to build C source code from a Rust
syntax tree. Basic language features will become available over time, but complex features like generics
or macros will likely not be possible without actual code analysis.

rs2c includes an optional feature `opencl` to transpile Rust functions into OpenCL kernels.

A simple Rust function translated into C: 

```rust
fn foo(y: f32, argument: &mut [u32; 3]) -> f32 {
    if y>=10.0 {
        return y.clamp(12.0_f32, 11.0_f32);
    } else if y == 2.0_f64 as f32 { 
        barfoo(&mut argument);
    } else {
        foobar();
    }
    let z: [u32; 4] = [2; 4];
    let mut x: [f32; 3] = [y, 1.0, argument[0] as f32];
    let t: f32 = bar(&mut x);
    return x[0] * t * z[2];
}
```

```C
float foo(const float y, unsigned int* argument) {
    if (y >= 10.0) {
        return clamp(y, 12.0f, 11.0f);
    } else if (y == (float)2.0) {
        barfoo(argument);
    } else {
        foobar();
    }
    const unsigned int z[4] = {2};
    float x[3] = {y, 1.0, (float)argument[0]};
    const float t = bar(x);
    return x[0] * t * z[2];
}
```

## Features

- Function signature transpiling (without generics)
- Variable and Array initialisation (Type annotation required)
- Smart Rust-to-C Type translation
- Reference translation
- `for`- and `while`-loops with simple Rust ranges
- `if-else` branching
- Function and method calls
- OpenCL kernel generation

## Goals

This project is under development to facilitate a crate that allows annotated Rust code to be compiled and executed via OpenCL.