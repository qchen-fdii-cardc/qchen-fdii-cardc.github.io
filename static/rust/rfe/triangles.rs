fn main() {
    let deg2rad = std::f64::consts::PI / 180.0;

    println!("{:>10}{:>10}{:>10}{:>10}", "a", "b", "α", "c");

    let a: f64 = 3.0;
    let b: f64 = 4.0;
    let angle: f64 = deg2rad * 30.0;
    let c_squared = a.powi(2) + b.powi(2) - 2.0 * a * b * angle.cos();
    let c = c_squared.sqrt();
    println!("{:>10.4}{:>10.4}{:>10.4}{:>10.4}", a, b, angle / deg2rad, c);

    let a: f64 = 3.0;
    let b: f64 = 4.0;
    let angle: f64 = deg2rad * 60.0;
    let c_squared = a.powi(2) + b.powi(2) - 2.0 * a * b * angle.cos();
    let c = c_squared.sqrt();
    println!("{:>10.4}{:>10.4}{:>10.4}{:>10.4}", a, b, angle / deg2rad, c);

    let a: f64 = 3.0;
    let b: f64 = 4.0;
    let angle: f64 = deg2rad * 90.0;
    let c_squared = a.powi(2) + b.powi(2) - 2.0 * a * b * angle.cos();
    let c = c_squared.sqrt();
    println!("{:>10.4}{:>10.4}{:>10.4}{:>10.4}", a, b, angle / deg2rad, c);

    let a: f64 = 3.0;
    let b: f64 = 4.0;
    let angle: f64 = deg2rad * 120.0;
    let c_squared = a.powi(2) + b.powi(2) - 2.0 * a * b * angle.cos();
    let c = c_squared.sqrt();
    println!("{:>10.4}{:>10.4}{:>10.4}{:>10.4}", a, b, angle / deg2rad, c);

    let a: f64 = 3.0;
    let b: f64 = 4.0;
    let angle: f64 = deg2rad * 150.0;
    let c_squared = a.powi(2) + b.powi(2) - 2.0 * a * b * angle.cos();
    let c = c_squared.sqrt();
    println!("{:>10.4}{:>10.4}{:>10.4}{:>10.4}", a, b, angle / deg2rad, c);
}
