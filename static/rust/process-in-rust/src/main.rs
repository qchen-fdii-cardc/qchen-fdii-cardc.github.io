use nix::unistd::getppid;
use std::io::Read;
use std::os::unix::process::CommandExt;
use std::process::{Command, Stdio};

fn main() -> std::io::Result<()> {
    println!("Parent process starting, PID: {}", std::process::id());

    // 创建子进程
    let mut command = Command::new("ls");
    command.arg("-l");

    unsafe {
        command.pre_exec(|| {
            println!(
                "Child process starting, PID: {}, PPID: {}",
                std::process::id(),
                getppid().as_raw()
            );
            Ok(())
        });
    }

    let mut child = command.spawn()?;

    // 等待子进程完成并获取输出
    let mut output = String::new();
    if let Some(mut stdout) = child.stdout.take() {
        stdout.read_to_string(&mut output)?;
    }

    // 等待子进程结束并获取状态
    let status = child.wait()?;

    println!("Child process output:\n{}", output);
    println!("Child process exited with status: {}", status);

    Ok(())
}
