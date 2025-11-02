fn main() {
    let a: f64 = 2.0;
    let b: f64 = 4.0;
    let angle: f64 = std::f64::consts::PI / 16.0;
    let c_squared = a.powi(2) + b.powi(2) - 2.0 * a * b * angle.cos();
    let c = c_squared.sqrt();
    println!("第三边长度 c = {}", c);
}
