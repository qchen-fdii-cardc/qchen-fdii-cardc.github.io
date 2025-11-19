use rand::{Rng, seq::SliceRandom};
use std::fmt;

// --- Core Expression Representation ---

#[derive(Clone, Copy)]
pub struct UnaryOp {
    pub func: fn(f64) -> f64,
    pub name: &'static str,
}

impl fmt::Debug for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("UnaryOp").field("name", &self.name).finish()
    }
}

#[derive(Clone, Copy)]
pub struct BinaryOp {
    pub func: fn(f64, f64) -> f64,
    pub name: &'static str,
}

impl fmt::Debug for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("BinaryOp")
            .field("name", &self.name)
            .finish()
    }
}

/// Represents a mathematical expression as a tree.
///
/// This enum is the core data structure for the symbolic regression. It can represent
/// constants, variables, and both unary and binary operations, allowing for the
/// construction of complex mathematical formulas.
#[derive(Debug, Clone)]
pub enum Expr {
    Constant(f64),
    Variable(char),
    Unary(UnaryOp, Box<Expr>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
}

const UNARY_OPS: &[UnaryOp] = &[
    UnaryOp {
        func: f64::sin,
        name: "sin",
    },
    UnaryOp {
        func: f64::cos,
        name: "cos",
    },
];

const BINARY_OPS: &[BinaryOp] = &[
    BinaryOp {
        func: |a, b| a + b,
        name: "+",
    },
    BinaryOp {
        func: |a, b| a - b,
        name: "-",
    },
    BinaryOp {
        func: |a, b| a * b,
        name: "*",
    },
    BinaryOp {
        func: |a, b| if b.abs() < 1e-6 { 1.0 } else { a / b },
        name: "/",
    },
];

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Constant(c) => write!(f, "{:.2}", c),
            Expr::Variable(v) => write!(f, "{}", v),
            Expr::Unary(op, a) => write!(f, "{}({})", op.name, a),
            Expr::Binary(op, a, b) => write!(f, "({} {} {})", a, op.name, b),
        }
    }
}

impl Expr {
    pub fn deepth(&self) -> u32 {
        match self {
            Expr::Constant(_) | Expr::Variable(_) => 1,
            Expr::Unary(_, a) => 1 + a.deepth(),
            Expr::Binary(_, a, b) => 1 + u32::max(a.deepth(), b.deepth()),
        }
    }

    pub fn equal(&self, other: &Expr) -> bool {
        match (self, other) {
            (Expr::Constant(c1), Expr::Constant(c2)) => (c1 - c2).abs() < 1e-9,
            (Expr::Variable(v1), Expr::Variable(v2)) => v1 == v2,
            (Expr::Unary(op1, a1), Expr::Unary(op2, a2)) => op1.name == op2.name && a1.equal(a2),
            (Expr::Binary(op1, l1, r1), Expr::Binary(op2, l2, r2)) => {
                op1.name == op2.name && l1.equal(l2) && r1.equal(r2)
            }
            _ => false,
        }
    }
    pub fn simplify(&self) -> Expr {
        let simplified = match self {
            Expr::Unary(op, a) => Expr::Unary(*op, Box::new(a.simplify())),
            Expr::Binary(op, a, b) => {
                Expr::Binary(*op, Box::new(a.simplify()), Box::new(b.simplify()))
            }
            _ => self.clone(),
        };

        match &simplified {
            Expr::Binary(op, a, b) => {
                if let (Expr::Constant(c1), Expr::Constant(c2)) = (&**a, &**b) {
                    return Expr::Constant((op.func)(*c1, *c2));
                }
                match (op.name, &**a, &**b) {
                    ("+", _, Expr::Constant(c)) if c.abs() < 1e-9 => *a.clone(),
                    ("+", Expr::Constant(c), _) if c.abs() < 1e-9 => *b.clone(),
                    ("-", _, Expr::Constant(c)) if c.abs() < 1e-9 => *a.clone(),
                    ("*", _, Expr::Constant(c)) if (c - 1.0).abs() < 1e-9 => *a.clone(),
                    ("*", Expr::Constant(c), _) if (c - 1.0).abs() < 1e-9 => *b.clone(),
                    ("*", _, Expr::Constant(c)) if c.abs() < 1e-9 => Expr::Constant(0.0),
                    ("*", Expr::Constant(c), _) if c.abs() < 1e-9 => Expr::Constant(0.0),
                    ("/", _, Expr::Constant(c)) if (c - 1.0).abs() < 1e-9 => *a.clone(),
                    ("/", Expr::Constant(c), _) if c.abs() < 1e-9 => Expr::Constant(0.0),
                    ("/", Expr::Variable(_), Expr::Variable(_))
                        if a.to_string() == b.to_string() =>
                    {
                        Expr::Constant(1.0)
                    }
                    ("-", Expr::Variable(_), Expr::Variable(_))
                        if a.to_string() == b.to_string() =>
                    {
                        Expr::Constant(0.0)
                    }
                    ("/", _, _) if a.to_string() == b.to_string() => Expr::Constant(1.0),
                    ("-", _, Expr::Constant(c)) if *c < 0.0_f64 => Expr::Binary(
                        BINARY_OPS[0], // +
                        Box::new(*a.clone()),
                        Box::new(Expr::Constant(-c)),
                    ),
                    ("+", _, Expr::Constant(c)) if *c < 0.0_f64 => Expr::Binary(
                        BINARY_OPS[1], // -
                        Box::new(*a.clone()),
                        Box::new(Expr::Constant(-c)),
                    ),
                    _ => simplified,
                }
            }
            Expr::Unary(op, a) => {
                if let Expr::Constant(c) = **a {
                    return Expr::Constant((op.func)(c));
                }
                match (op.name, &**a) {
                    ("sin", Expr::Constant(c)) if c.abs() < 1e-9 => Expr::Constant(0.0),
                    ("cos", Expr::Constant(c)) if c.abs() < 1e-9 => Expr::Constant(1.0),
                    _ => simplified,
                }
            }
            _ => simplified,
        }
    }

    pub fn evaluate(&self, x: f64) -> f64 {
        match self {
            Expr::Constant(c) => *c,
            Expr::Variable(_) => x,
            Expr::Unary(op, a) => (op.func)(a.evaluate(x)),
            Expr::Binary(op, a, b) => (op.func)(a.evaluate(x), b.evaluate(x)),
        }
    }
}

// --- Utility Functions ---

pub fn print_expr_as_tree(expr: &Expr, indent: usize) {
    print!("|");
    for _ in 0..indent {
        print!("-");
    }
    match expr {
        Expr::Constant(c) => println!("Constant({:.2})", c),
        Expr::Variable(v) => println!("Variable({})", v),
        Expr::Unary(op, a) => {
            println!("Unary({})", op.name);
            print_expr_as_tree(a, indent + 1);
        }
        Expr::Binary(op, a, b) => {
            println!("Binary({})", op.name);
            print_expr_as_tree(a, indent + 4);
            print_expr_as_tree(b, indent + 4);
        }
    }
}

// --- Genetic Algorithm Module ---
pub mod ga {
    use super::*;

    #[derive(Clone, Copy)]
    pub struct Config {
        pub population_size: usize,
        pub generations: usize,
        pub elite_size: usize,
        pub mutation_rate: f64,
        pub simplify_rate: f64,
        pub stall_threshold: i32,
    }

    pub fn generate_random_expr(depth: u32) -> Expr {
        let mut rng = rand::thread_rng();
        if depth == 0 || rng.gen_bool(0.3) {
            if rng.gen_bool(0.5) {
                Expr::Constant(rng.gen_range(-5.0..5.0))
            } else {
                Expr::Variable('x')
            }
        } else {
            let op_type: u8 = rng.gen_range(0..=2);
            match op_type {
                0 => {
                    // Unary
                    let op = UNARY_OPS.choose(&mut rng).unwrap();
                    let operand = Box::new(generate_random_expr(depth - 1));
                    Expr::Unary(*op, operand)
                }
                _ => {
                    // Binary
                    let op = BINARY_OPS.choose(&mut rng).unwrap();
                    let left = Box::new(generate_random_expr(depth - 1));
                    let right = Box::new(generate_random_expr(depth - 1));
                    Expr::Binary(*op, left, right)
                }
            }
        }
    }

    pub fn fitness(expr: &Expr, data: &[(f64, f64)]) -> f64 {
        let mut mse = 0.0;
        for (x, y_true) in data {
            let y_pred = expr.evaluate(*x);
            mse += (y_true - y_pred).powi(2);
        }
        mse / data.len() as f64
    }

    pub fn fitness_max_abs(expr: &Expr, data: &[(f64, f64)]) -> f64 {
        let mut max_abs_error = 0.0;
        for (x, y_true) in data {
            let y_pred = expr.evaluate(*x);
            let abs_error = (y_true - y_pred).abs();
            if abs_error > max_abs_error {
                max_abs_error = abs_error;
            }
        }
        max_abs_error
    }

    pub fn crossover(parent1: &Expr, parent2: &Expr) -> Expr {
        let mut rng = rand::thread_rng();
        let mut p1 = parent1.clone();
        let p2 = parent2.clone();

        // For simplicity, we'll just swap a random subtree.
        // A more robust implementation would traverse the tree and pick a node.
        if rng.gen_bool(0.5) {
            if let (Expr::Binary(_, l, _), Expr::Binary(_, _, r2)) = (&mut p1, &p2) {
                *l = r2.clone();
            }
        } else if let (Expr::Binary(_, _, r), Expr::Binary(_, l2, _)) = (&mut p1, &p2) {
            *r = l2.clone();
        }
        p1
    }

    pub fn mutate(expr: &Expr) -> Expr {
        let mut rng = rand::thread_rng();
        let mut new_expr = expr.clone();

        if rng.gen_bool(0.2) {
            // 20% chance to mutate a node
            // Replace a random node with a new random expression
            let depth = rng.gen_range(1..3);
            return generate_random_expr(depth);
        }

        match &mut new_expr {
            Expr::Unary(_, a) => {
                **a = mutate(a);
            }
            Expr::Binary(_, l, r) => {
                if rng.gen_bool(0.5) {
                    **l = mutate(l);
                } else {
                    **r = mutate(r);
                }
            }
            _ => {}
        }
        new_expr
    }

    /// Runs the genetic algorithm to find an expression that fits the given data.
    ///
    /// # Arguments
    ///
    /// * `config` - Configuration for the genetic algorithm (population size, generations, etc.).
    /// * `data` - A slice of (x, y) tuples that the expression should try to fit.
    ///
    /// # Returns
    ///
    /// The final population of expressions after all generations have been run.
    pub fn fit(config: &Config, data: &[(f64, f64)]) -> Vec<Expr> {
        let mut population: Vec<Expr> = (0..config.population_size)
            .map(|_| generate_random_expr(3))
            .collect();

        let mut best_fitness = f64::MAX;
        let mut stall_counter = 0;
        let mut fit_func: fn(&Expr, &[(f64, f64)]) -> f64 = fitness_max_abs;

        for generation in 0..config.generations {
            if stall_counter > config.stall_threshold && fit_func as usize != fitness as usize {
                println!("Stall detected, switching fitness function to RSS error.");
                fit_func = fitness;
            }

            let mut fitness_scores: Vec<(f64, &Expr)> = population
                .iter()
                .map(|expr| (fit_func(expr, data), expr))
                .collect();

            fitness_scores.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap());

            let best_expr = fitness_scores[0].1;
            println!(
                "{:<6} {:<12.4} {}",
                generation, fitness_scores[0].0, best_expr
            );

            if fitness_scores[0].0 < best_fitness {
                best_fitness = fitness_scores[0].0;
                stall_counter = 0;
            } else {
                stall_counter += 1;
            }

            if fitness_scores[0].0 < 1e-4 {
                println!("\nFound a good solution!");
                break;
            }

            let mut new_population = Vec::new();

            // Elitism
            for (_, expr) in fitness_scores.iter().take(config.elite_size) {
                new_population.push((*expr).clone());
            }

            // Crossover and mutation
            while new_population.len() < config.population_size {
                let mut rng = rand::thread_rng();
                let parents: Vec<&Expr> = fitness_scores
                    .iter()
                    .take(config.population_size / 2) // Select from top 50%
                    .map(|(_, expr)| *expr)
                    .collect();

                let parent1 = parents.choose(&mut rng).unwrap();
                let parent2 = parents.choose(&mut rng).unwrap();

                let mut offspring = crossover(parent1, parent2);
                if rng.gen_bool(config.mutation_rate) {
                    offspring = mutate(&offspring);
                }
                if rng.gen_bool(config.simplify_rate) {
                    offspring = offspring.simplify();
                }

                new_population.push(offspring);
            }
            population = new_population;
        }
        population
    }
}
