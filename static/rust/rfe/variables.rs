use std::any::type_name_of_val;
use std::mem::size_of_val;
use std::ptr::addr_of;

fn main() {
    let a = 5;
    let a5_addr = (&a) as *const i32 as usize;
    println!(
        "a{}  {}  {:p}  {}",
        a,
        type_name_of_val(&a),
        &a,
        size_of_val(&a)
    );

    let a = 6_i64;
    let a6_addr = (&a) as *const i64 as usize;
    println!(
        "a{}  {}  {:p}  {}",
        a,
        type_name_of_val(&a),
        &a,
        size_of_val(&a)
    );

    let a = 7.0_f32;
    let a7_addr = (&a) as *const f32 as usize;
    println!(
        "a{}  {}  {:p}  {}",
        a,
        type_name_of_val(&a),
        &a,
        size_of_val(&a)
    );

    let a = 8.0;
    let a8_addr = (&a) as *const f64 as usize;
    println!(
        "a{}  {}  {:p}  {}",
        a,
        type_name_of_val(&a),
        &a,
        size_of_val(&a)
    );

    // show the value of addresses again
    println!("{:?} {:#x} {}", addr_of!(a5_addr), a5_addr, unsafe {
        *(a5_addr as *const i32)
    });
    println!("{:?} {:#x} {}", addr_of!(a6_addr), a6_addr, unsafe {
        *(a6_addr as *const i32)
    });
    println!("{:?} {:#x} {}", addr_of!(a7_addr), a7_addr, unsafe {
        *(a7_addr as *const f32)
    });
    println!("{:?} {:#x} {}", addr_of!(a8_addr), a8_addr, unsafe {
        *(a8_addr as *const f64)
    });
}
