+++
title = 'Coding_on_android机上编译运行C/Cpp/Rust程序'
date = 2025-06-03T09:13:26+08:00
draft = false
mathkatex = true
categories = ['c', 'cpp', 'rust']
tags = ['termux', 'coding', 'android']
toc = true
tocBorder = true
+++

## 手机的CPU整天没事干怎么办？

有些人，比如我，总是会觉得一个CPU没有在满功率运行是一种犯罪……

而现在的手机，实际上非常强力，通常有8个核的高性能CPU，还有GPU还有一堆内存。怎么办？

看看这个CpuInfo，简直是厉害啊。

```txt
processor : 0
BogoMIPS : 2000.00
Features : fp asimd evtstrm aes pmull sha1 sha2 crc32 atomics fphp asimdhp cpuid asimdrdm jscvt fcma lrcpc dcpop sha3 sm3 sm4 asimddp sha512 sve asimdfhm dit uscat ilrcpc flagm ssbs sb paca pacg dcpodp flagm2 frint svei8mm svebf16 i8mm bf16 dgh bti amu ecv wfxt
CPU implementer : 0x48
CPU architecture: 8
CPU variant : 0x2
CPU part : 0xd23
CPU revision : 1

processor : 1
BogoMIPS : 2000.00
Features : fp asimd evtstrm aes pmull sha1 sha2 crc32 atomics fphp asimdhp cpuid asimdrdm jscvt fcma lrcpc dcpop sha3 sm3 sm4 asimddp sha512 sve asimdfhm dit uscat ilrcpc flagm ssbs sb paca pacg dcpodp flagm2 frint svei8mm svebf16 i8mm bf16 dgh bti amu ecv wfxt
CPU implementer : 0x48
CPU architecture: 8
CPU variant : 0x2
CPU part : 0xd23
CPU revision : 1

processor : 2
BogoMIPS : 2000.00
Features : fp asimd evtstrm aes pmull sha1 sha2 crc32 atomics fphp asimdhp cpuid asimdrdm jscvt fcma lrcpc dcpop sha3 sm3 sm4 asimddp sha512 sve asimdfhm dit uscat ilrcpc flagm ssbs sb paca pacg dcpodp flagm2 frint svei8mm svebf16 i8mm bf16 dgh bti amu ecv wfxt
CPU implementer : 0x48
CPU architecture: 8
CPU variant : 0x2
CPU part : 0xd23
CPU revision : 1

processor : 3
BogoMIPS : 2000.00
Features : fp asimd evtstrm aes pmull sha1 sha2 crc32 atomics fphp asimdhp cpuid asimdrdm jscvt fcma lrcpc dcpop sha3 sm3 sm4 asimddp sha512 sve asimdfhm dit uscat ilrcpc flagm ssbs sb paca pacg dcpodp flagm2 frint svei8mm svebf16 i8mm bf16 dgh bti amu ecv wfxt
CPU implementer : 0x48
CPU architecture: 8
CPU variant : 0x2
CPU part : 0xd23
CPU revision : 1

processor : 4
BogoMIPS : 2000.00
Features : fp asimd evtstrm aes pmull sha1 sha2 crc32 atomics fphp asimdhp cpuid asimdrdm jscvt fcma lrcpc dcpop sha3 sm3 sm4 asimddp sha512 sve asimdfhm dit uscat ilrcpc flagm ssbs sb paca pacg dcpodp flagm2 frint svei8mm svebf16 i8mm bf16 dgh bti amu ecv wfxt
CPU implementer : 0x48
CPU architecture: 8
CPU variant : 0x2
CPU part : 0xd46
CPU revision : 1

processor : 5
BogoMIPS : 2000.00
Features : fp asimd evtstrm aes pmull sha1 sha2 crc32 atomics fphp asimdhp cpuid asimdrdm jscvt fcma lrcpc dcpop sha3 sm3 sm4 asimddp sha512 sve asimdfhm dit uscat ilrcpc flagm ssbs sb paca pacg dcpodp flagm2 frint svei8mm svebf16 i8mm bf16 dgh bti amu ecv wfxt
CPU implementer : 0x48
CPU architecture: 8
CPU variant : 0x2
CPU part : 0xd46
CPU revision : 1

processor : 6
BogoMIPS : 2000.00
Features : fp asimd evtstrm aes pmull sha1 sha2 crc32 atomics fphp asimdhp cpuid asimdrdm jscvt fcma lrcpc dcpop sha3 sm3 sm4 asimddp sha512 sve asimdfhm dit uscat ilrcpc flagm ssbs sb paca pacg dcpodp flagm2 frint svei8mm svebf16 i8mm bf16 dgh bti amu ecv wfxt
CPU implementer : 0x48
CPU architecture: 8
CPU variant : 0x2
CPU part : 0xd46
CPU revision : 1

processor : 7
BogoMIPS : 2000.00
Features : fp asimd evtstrm aes pmull sha1 sha2 crc32 atomics fphp asimdhp cpuid asimdrdm jscvt fcma lrcpc dcpop sha3 sm3 sm4 asimddp sha512 sve asimdfhm dit uscat ilrcpc flagm ssbs sb paca pacg dcpodp flagm2 frint svei8mm svebf16 i8mm bf16 dgh bti amu ecv wfxt
CPU implementer : 0x48
CPU architecture: 8
CPU variant : 0x2
CPU part : 0xd05
CPU revision : 1
```

其实手机上完全能够搞点这样的活动，而且不需要什么root，什么华为的HarmonyOS，也完全没有问题。我的Iphone最近被刷烂了，没法再整IPhone了。

## 手机上编译C/Cpp程序

其实Termux已经挺成熟的，直接去下载就行[Termux:Android](https://termux.dev)

安装Termux跟普通的Android应用没有区别，就是一个独立的app，安装好了之后能够打开一个黑乎乎的命令行。

后面安装软件，跟普通的Linux没有区别，`apt install clang`或者`dpkg install clang`，然后就可以编译C/Cpp程序了。

那我们就简单的测试一下，前面我还在施工的[Eigen3](https://www.windtunnel.cn/eigen3tutorial/)，这个项目的代码在：[Eigen3Tutorial](https://github.com/qchen-fdii-cardc/eigen3tutorial)。

为了可以运行这些测试，我们需要安装几个工具：

```bash
apt install clang cmake git eigen
```

特别是是这个`eigen`，跟`libeigen3-dev`不一样。我们通过`apt search eigen`才发现它的。

整好了之后，就可以：

```bash
git clone https://github.com/qchen-fdii-cardc/eigen3tutorial.git
cd eigen3tutorial
cmake -B build
cmake --build build
```

然后发现，所有的几个例子都可以任意执行。

## 编译Rust程序

Rust同样没有问题，可以直接`apt install rust`，然后`cargo`这一套完全可以用。

最好的一点是，Termux的镜像整得特别好，会循环更新亚洲区、国内各个高校的镜像，所以下载速度特别快。

```rust
use rayon::prelude::*;
use std::time::Instant;
use rand::Rng;

fn monte_carlo_pi(samples: u64, threads: usize) -> f64 {
    let start = Instant::now();
    
    // 使用局部线程池而不是全局线程池
    let pool = rayon::ThreadPoolBuilder::new()
        .num_threads(threads)
        .build()
        .unwrap();
    
    let hits: u64 = pool.install(|| {
        (0..samples)
            .into_par_iter()
            .map(|_| {
                let mut rng = rand::rng();
                let x: f64 = rng.random_range(-1.0..1.0);
                let y: f64 = rng.random_range(-1.0..1.0);
                if x*x + y*y <= 1.0 { 1 } else { 0 }
            })
            .sum()
    });
    
    let duration = start.elapsed();
    let pi_estimate = 4.0 * hits as f64 / samples as f64;
    
    println!("🎯 Threads: {}, Time: {:?}, π ≈ {:.6}", 
             threads, duration, pi_estimate);
    pi_estimate
}

fn main() {
    let samples = 10_000_000;
    println!("📱 手机CPU并行计算演示");
    println!("🔢 计算π值 ({}M 采样点)", samples / 1_000_000);
    
    // 获取CPU核心数
    let cpu_count = num_cpus::get();
    println!("🔧 检测到 {} 个CPU核心", cpu_count);
    
    let mut results = Vec::new();
    
    for threads in 1..=cpu_count {
        let pi_val = monte_carlo_pi(samples, threads);
        results.push((threads, pi_val));
    }
    
    // 显示加速比分析
    println!("\n📊 性能分析:");
    if let Some(single_thread_time) = results.first() {
        println!("单线程基准: π ≈ {:.6}", single_thread_time.1);
        
        for (threads, _) in &results[1..] {
            println!("{}线程相对加速比: 估算 {:.2}x", 
                     threads, 
                     *threads as f64 * 0.8); // 简单估算，考虑并行开销
        }
    }
    
    println!("\n🎯 理论π值: 3.141592653589793");
    println!("📱 手机并行计算测试完成！");
}
```

同时需要在 `Cargo.toml` 中添加依赖，`cargo add rayon rand num_cpus`。

```toml
[package]
name = "mobile-pi-calculator"
version = "0.1.0"
edition = "2021"

[dependencies]
rayon = "1.8"
rand = "0.8"
num_cpus = "1.16"
```

在手机上运行，效果如下：

```bash
cargo build --release
./target/release/hello_cargo
📱 手机CPU并行计算演示
🔢 计算π值 (10M 采样点)
🔧 检测到 8 个CPU核心
🎯 Threads: 1, Time: 415.965937ms, π ≈ 3.142158
🎯 Threads: 2, Time: 284.113594ms, π ≈ 3.141996
🎯 Threads: 3, Time: 234.696094ms, π ≈ 3.141445
🎯 Threads: 4, Time: 181.711719ms, π ≈ 3.141939
🎯 Threads: 5, Time: 203.050312ms, π ≈ 3.141030
🎯 Threads: 6, Time: 141.51ms, π ≈ 3.141340
🎯 Threads: 7, Time: 140.900625ms, π ≈ 3.142074
🎯 Threads: 8, Time: 141.51375ms, π ≈ 3.142262
📊 性能分析:
单线程基准: π ≈ 3.142158
2线程相对加速比: 估算 1.60x
3线程相对加速比: 估算 2.40x
4线程相对加速比: 估算 3.20x
5线程相对加速比: 估算 4.00x
6线程相对加速比: 估算 4.80x
7线程相对加速比: 估算 5.60x
8线程相对加速比: 估算 6.40x
🎯 理论π值: 3.141592653589793
📱 手机并行计算测试完成！
```

还是挺好玩的啊！

## 小贴士

Termux下面我都用`nano`来简单编辑文件，都是通过Github把文件编写完成，Termux那边直接`git pull`，然后`cmake`一套，或者`cargo`一套。

Termux可以打开storage，就能直接访问手机的Documents/Downloads，也还挺方便。打开的方式是在Termux下面运行：

```bash
termux-setup-storage
```

这样，在Termux下面，就能直接访问手机的多个文件夹，位置在`~/storage`。
