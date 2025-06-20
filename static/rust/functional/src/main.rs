use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("Hello, world!");

    let a = (0..10)
        .map(|x| x * 3)
        .inspect(|x| println!("x: {}", x))
        .collect::<Vec<i32>>();
    // [0, 3, 6, 9, 12, 15, 18, 21, 24, 27]

    let b = a
        .iter()
        .map(|x| x * 2)
        .filter(|x| x % 2 == 0)
        .inspect(|x| println!("x: {}", x))
        .collect::<Vec<i32>>();
    // [0, 6, 12, 18, 24]

    let c = a
        .iter()
        .map(|x| x * 2)
        .filter(|x| x % 3 == 0)
        .inspect(|x| println!("x: {}", x))
        .collect::<Vec<i32>>();
    // [0, 6, 12, 18]

    // do stuff that might fail
    // for example, read a file
    let file = File::open("Cargo.toml")?;
    let mut reader = BufReader::new(file);
    let mut line = String::new();
    loop {
        let len = reader.read_line(&mut line)?;
        if len == 0 {
            break;
        }
        println!("line: {}", line);
    }
    // return a default OK at last
    Ok(())
}

fn add(a: i32, b: i32) -> i32 {
    a + b
}

fn add_func(step: i32) -> impl Fn(i32) -> i32 {
    move |x| x + step
}

#[test]
fn test_add() {
    let x = 10;
    let y = 20;

    let z = add(x, y);

    assert_eq!(z, 30);
}

#[test]
fn test_add_func() {
    let add_1 = add_func(1);
    let add_2 = add_func(2);

    assert_eq!(add_1(10), 11);
    assert_eq!(add_2(10), 12);
}
