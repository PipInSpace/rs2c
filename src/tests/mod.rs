#![allow(dead_code)]

use crate::*;

/// Simple Rust/C Code for verification
const SIMPLE_RS: &str = "fn foo(y: f32, argument: &mut [u32; 3]) -> f32 {
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
}";
const SIMPLE_C: &str = "float foo(const float y, unsigned int* argument) {
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
}";

/// Complex Rust code/OpenCL kernel for `opencl` feature verification.
/// Taken from https://github.com/PipInSpace/IonSolver/blob/main/src/kernels/sim_kernels.cl
const CL_KERNEL_RS: &str = "fn update_fields(fi: &Vec<f32>, rho: &mut Vec<f32>, u: &mut Vec<f32>, flags: &Vec<u8>, t: u64, fx: f32, fy: f32, fz: f32) {
    let n: u32 = get_global_id(0); // n = x+(y+z*Ny)*Nx
    if n>=def_N as u32 || is_halo(n) { return; } // don't execute update_fields() on halo
    let flagsn: u8 = flags[n as usize];
    let flagsn_bo: u8=flagsn&TYPE_BO;
    let flagsn_su: u8=flagsn&TYPE_SU; // extract boundary and surface flags
    if flagsn_bo==TYPE_S || flagsn_su==TYPE_G { return; } // don't update fields for boundary or gas lattice points

    let mut j: [u32; def_velocity_set] = [0; def_velocity_set]; // neighbor indices
    neighbors(n, &mut j); // calculate neighbor indices
    let mut fhn: [f32; def_velocity_set] = [0.0; def_velocity_set]; // local DDFs
    load_f(n, &mut fhn, &fi, &j, t); // perform streaming (part 2)

    // calculate local density and velocity for collision
    let mut rhon: f32 = 0.0;
    let mut uxn: f32 = 0.0;
    let mut uyn: f32 = 0.0;
    let mut uzn: f32 = 0.0;
    calculate_rho_u(&fhn, &mut rhon, &mut uxn, &mut uyn, &mut uzn); // calculate density and velocity fields from fi
    {
        uxn = uxn.clamp(-def_c, def_c); // limit velocity (for stability purposes)
        uyn = uyn.clamp(-def_c, def_c); // force term: F*dt/(2*rho)
        uzn = uzn.clamp(-def_c, def_c);
    }

    rho[      n as usize] = rhon; // update density field
    u[        n as usize] = uxn; // update velocity field
    u[  def_N+n as usize] = uyn;
    u[2*def_N+n as usize] = uzn;
} // update_fields()";
const CL_KERNEL_CL: &str = "__kernel void update_fields(const global float* fi, global float* rho, global float* u, const global unsigned char* flags, const unsigned long t, const float fx, const float fy, const float fz) {
    const unsigned int n = get_global_id(0);
    if (n >= (unsigned int)def_N || is_halo(n)) {
        return;
    }
    const unsigned char flagsn = flags[(unsigned long)n];
    const unsigned char flagsn_bo = flagsn & TYPE_BO;
    const unsigned char flagsn_su = flagsn & TYPE_SU;
    if (flagsn_bo == TYPE_S || flagsn_su == TYPE_G) {
        return;
    }
    unsigned int j[def_velocity_set] = {0};
    neighbors(n, j);
    float fhn[def_velocity_set] = {0.0};
    load_f(n, fhn, fi, j, t);
    float rhon = 0.0;
    float uxn = 0.0;
    float uyn = 0.0;
    float uzn = 0.0;
    calculate_rho_u(fhn, rhon, uxn, uyn, uzn);
    {
        uxn = clamp(uxn, -def_c, def_c);
        uyn = clamp(uyn, -def_c, def_c);
        uzn = clamp(uzn, -def_c, def_c);
    }
    rho[(unsigned long)n] = rhon;
    u[(unsigned long)n] = uxn;
    u[def_N + (unsigned long)n] = uyn;
    u[2 * def_N + (unsigned long)n] = uzn;
}";

#[test]
fn c_test() {
    let c = indent_c(convert_to_c_function(SIMPLE_RS));
    println!("{}", c);
    assert_eq!(c, SIMPLE_C);
}

#[test]
#[cfg(feature = "opencl")]
fn cl_test() {
    let cl = indent_c(opencl::convert_to_cl_kernel(CL_KERNEL_RS));
    println!("{}", cl);
    assert_eq!(cl, CL_KERNEL_CL);
}