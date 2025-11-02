fn main() {
    println!(
        "∠pi/16-(2.0,4.0) => {}",
        (2.0_f64.powi(2) + 4.0_f64.powi(2)
            - 2.0 * 2.0_f64 * 4.0_f64 * (std::f64::consts::PI / 16.0).cos())
        .sqrt()
    );
}
