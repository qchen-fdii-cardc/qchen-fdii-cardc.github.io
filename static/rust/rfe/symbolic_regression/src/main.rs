
use symbolic_regression::{ga, print_expr_as_tree, Expr};

fn main() {
    // Target function: y = x^2 + x + 1
    let target_fn = |x: f64| x.powi(2) + x + 1.0 + x * x.cos();
    let data: Vec<(f64, f64)> = (0..100)
        .map(|i| {
            let x = i as f64 / 10.0;
            let noise = (rand::random::<f64>() - 0.5) * 0.1; // Add a little noise
            (x, target_fn(x) + noise) })
        .collect();

    // Data -> csv
    let _r = std::fs::write(
        "data.csv",
        data.iter()
            .map(|(x, y)| format!("{},{}", x, y))
            .collect::<Vec<String>>()
            .join("\n"),
    );

    let config = ga::Config {
        population_size: 50,
        generations: 10000,
        elite_size: 20,
        mutation_rate: 0.5,
        simplify_rate: 0.8,
        stall_threshold: 1000,
    };

    let population = ga::fit(&config, &data);

    let mut final_scores: Vec<(f64, &Expr)> = population
        .iter()
        .map(|expr| (ga::fitness(expr, &data), expr))
        .collect();
    final_scores.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap());

    println!(
        "\nFinal best expression: {}",
        final_scores[0].1.simplify()
    );
    println!("Final fitness: {:.4}", final_scores[0].0);
    print_expr_as_tree(&final_scores[0].1.simplify(), 0);
}


