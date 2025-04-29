use rust_for_stupid_engineers::ode::{rk6, OdeFunc};

struct SimpleOde {
    dim: usize,
}

impl OdeFunc for SimpleOde {
    fn func(&self, _t: f64, x: &Vec<f64>, dx: &mut Vec<f64>) {
        dx[0] = x[1];
        dx[1] = -x[0];
    }

    fn dimension(&self) -> usize {
        self.dim
    }
}

fn main() {
    let ode = SimpleOde { dim: 2 };
    let t0 = 0.0;
    let t1 = 10.0;
    let x0 = vec![1.0, 0.0];
    let h = 0.1;

    let result = rk6(&ode, t0, t1, x0, h);

    println!("#{}", "ODE45 Result");
    println!("#{:<18}{:<18}{:<18}", "Time", "X[0]", "X[1]");
    for (t, x) in result {
        println!("{:<18.4}{:<18.4}{:<18.4}", t, x[0], x[1]);
    }
}