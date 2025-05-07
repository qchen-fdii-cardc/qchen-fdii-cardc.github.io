use std::process::Command;

fn main() -> std::io::Result<()> {
    let output = Command::new("ls")
        .arg("-l")
        .output()
        .expect("failed to run ls");
    println!(
        "Child process output: {}",
        String::from_utf8_lossy(&output.stdout)
    );
    Ok(())
}
