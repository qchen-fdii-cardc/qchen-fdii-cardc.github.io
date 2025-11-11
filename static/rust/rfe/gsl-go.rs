#[link(name = "gsl")]
#[link(name = "gslcblas")]
unsafe extern "C" {
    fn gsl_sf_bessel_J0(x: f64) -> f64;
}

fn main() {
    let x = 5.0;

    let result = unsafe { gsl_sf_bessel_J0(x) };

    println!("GSL Bessel J0({}) = {}", x, result);
}
