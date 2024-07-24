# R2C

A very WIP Rust to C transpiler. Currently supports transpiling limited rust functions into C.

A simple Rust function translated into C: 

```rust
fn foo(y: f32, argument: &mut [u32; 3]) -> f32 {
    let x: [f32; 3] = [y, 1.0, argument[0] as f32];
    let t: f32 = bar(&mut x);
    return x[0] * t;
}
```

```C
float foo(const float y, uint* argument) {
    float x[3] = {y, 1.0f, (float)argument[0]};
    float t = bar(x);
    return x[0] * t;
}
```