+++
title = 'Rust for Stupid Engineers愚蠢工程师用Rust'
date = 2025-04-29T08:52:55+08:00
draft = false
mathkatex = true
categories = ['rust']
tags = ['rust', 'array', 'vector', 'numeric']
toc = true
tocBorder = true
+++

## 愚蠢工程师用Rust

在考虑用Rust做工程计算——这是一个愚蠢的选择——时，常常需要的一个场景，就是算一大堆数据，然后输出到一个文件中。基本上，我们工程师能够处理的极限就是二维数组，如果是更高维的，我们一个二维数组一个二维数组地处理，毕竟，我们是愚蠢地工程师。

翻开Rust，很好嘛，二维数组：

```rust
let array = [[0; 10]; 10];
```

这就是一个简单的二维数组，10行10列，每个元素都是0。

简单干净，但是我们只能看不能改。翻翻Rust book（`rustup doc --book`）。而且，我们是工程师，我们只能处理浮点数。

```rust
let mut array = [[0.0; 10]; 10];
for i in 0..10 {
    array[i][i] = 1.0;
}
println!("{:?}", array);
```

打印了我们想要的10维单位矩阵。那么我们工程师是天生懂得复用的。

```rust
fn eye10() -> [[f64; 10]; 10] {
    let mut array = [[0.0; 10]; 10];
    for i in 0..10 {
        array[i][i] = 1.0;
    }
    array
}
```

现在可以调用这个函数，得到一个10维单位矩阵。

```rust
let array = eye10();
println!("{:?}", array);
```

或者，我们要修改这个矩阵，也没问题。当然是很好很强大。

```rust
let mut arr10 = eye10();
println!("{:?}", arr10);

// arr10[0][0] = 100.00;
arr10[1][1] = 100.0;
println!("{:?}", arr10);
```

总之，不改动的数据，我们用`let`，要改动的数据，我们用`let mut`。

### 数组的使用

当我们把这个处理过程也放在一个函数里面，那要怎么整呢？方案一：输出一个新的二维数组。

```rust
pub fn setij(arr: [[f64; 10]; 10], val: f64, i: usize, j: usize) -> [[f64; 10]; 10] {
    let mut new_arr = arr;
    new_arr[i][j] = val;
    new_arr
}
```

用起来也很简单，可以看到，array就是一个值，随便的传递，随便的使用，就跟我们使用普通的1，2，3一样。唯一需要注意的是，我们的数组，大小和类型都是固定的，这个固定的含义是在编译的时候是固定的。这对于工程师来说都不是事情。随便改改Magic number，编译运行，看结果，工程师的快乐就是这么简单。

```rust
#[test]
fn test_set11(){
    let arr1 = eye10();
    
    let (i, j, new_val) = (1, 1, 100.0);
    let arr2 = setij(arr1, new_val, i, j);
    assert_eq!(arr1[i][j], new_val);
    println!("{:?}", arr1);
    println!("{:?}", arr2);
}
```

哎哎哎，聪（yu）明（chun）的工程师发现了不对，不是说了，arr1被调用了其所有权就被移动了吗？为什么这个也行？那我们还要引用干什么？

```rust
pub fn setij_ref(arr: &[[f64; 10]; 10], val: f64, i: usize, j: usize) -> [[f64; 10]; 10] {
    let mut new_arr = *arr;
    new_arr[i][j] = val;
    new_arr
}
```

按照我们的理解，不是应该是这样的吗？完美规避了arr1被移动的问题（我认为！）

但是，array和tuple，以及平凡的简单数据类型：i32, f64, bool, char, 等等，都是Copy的。根本不存在什么所有权被移动的问题。

```rust
let x = 10;
let y = x;
println!("x: {}, y: {}", x, y);
```

这个毫无问题，那谁有问题呢？

```rust
let x = String::from("hello");
let y = x;
println!("x: {}, y: {}", x, y);
```

这个代码就会报错，`y=x`的时候，字符串的ownership被移动了，只有`y`能用，`x`不能用了。

这整个问题就很清楚了，Rust的基本类型（i32, f64, bool, char, 数组，tuple）都是值类型，没有什么所有权，就随便用。而所有权是针对谁的呢？如果学过C语言就知道，是针对堆上的数据。这些值类型，都是放在栈上的，它们的主要特征就是大小在编译的时候就是已知的。那么代价呢，古尔丹？

### 值数据的代价

对于我们工程师，我们很满意数组可以直接这样用，我们也不在乎需要重新编译程序才能改变数组的大小。在这个层次，我们的Rust程序跟任何其它有GC或者不穿内裤的C语言程序是一样的。

> 堆栈则无痛。

那么代价呢？我们直接用堆栈就ok，完全不用处理所有权。

代价就是爆栈啊，愚蠢的年轻人。

```rust
#[test]
fn fuck_stack_with_big_array() {
    const N : usize = 100000;
    let array = [[0.0; N]; N];
    
}
```

```bash
cargo test fuck_stack_with_big_array -- --nocapture
```

程序会输出：`thread 'tests::fuck_stack_with_big_array' has overflowed its stack`。

实际上，我们可以用`stacker`这个工具，来查看栈的大小。

```bash
cargo add stacker
```

```rust
use stacker;

fn main() {
    println!("Stupidity, and Engineering");
    let size = stacker::remaining_stack().unwrap();
    println!("reamaining stack:{}", size);
    println!("Stack: {} kb", size / 1024 );
}

```

```bash
cargo run
# Stupidity, and Engineering
# reamaining stack:1018016
# Stack: 994 kb
```

可以看到，栈的大小是还不到1MB。我看有本书上说有8MB，也还是很容易爆的。

## 愚蠢的尝试和解药

### 愚蠢的尝试

首先，我们知道，堆栈好，堆坏。但是，堆栈那么小，我们工程师的那么大，怎么办？我们从Rust the language book看到，Box就相当于是指针，放在堆上的。马上就动手：

```rust
#[test]
fn test_big_array_usage() {
    const N: usize = 1024;
    let mut rng = rand::rng();
    let mut array = Box::new([[0.0; N]; N]);
    for i in 0..N {
        for j in 0..N {
            array[i][j] = rng.random_range(1.0..100.0);
        }
    }

    assert_eq!(array.len(), N);
    assert_eq!(array[0].len(), N);
}
```

```bash
cargo test test_big_array_usage -- --nocapture
```

结果就是：`thread 'tests::test_big_array_usage' has overflowed its stack`。原来，Rust会先在堆栈上创建一个array，然后Copy这个array到堆上，谁让array是值类型呢……

### 更加愚蠢的`unsafe`领域

那怎么办？我们怎么在堆上面创建一个数组？

```rust
use std::alloc::alloc;
use core::{alloc::Layout, ptr::NonNull};

fn heap_array<T, const N: usize>() -> Box<[T; N]> {
    unsafe {
        let layout = Layout::new::<[T; N]>();
        let pointer = alloc(layout);

        if pointer.is_null() {
            panic!("allocation failed");
        } else{
            let ptr = NonNull::new_unchecked(pointer as *mut [T; N]);
            Box::from_raw(ptr.as_ptr())
        }
    }
}
```

大概就是这样一坨东西，没人希望看懂这个，也没必要看懂这个，工程师懂个屁的编程。

那怎么办？

### `vec!`和`Vec<T>`

Rust提供了很好用的动态数组，`Vec<T>`，还有一个宏实现的语法糖`vec!`。

```rust
let array = vec![[0.0; N]; N];
```

对这个这个玩意，我们就需要小心翼翼地处理其所有权转移。

但是工程师搞不了那么复杂地，我们就这样，看看这里，这里的`N`都不用编译期固定，是一个普通的变量。

```rust
fn rand_array(array: &mut Vec<Vec<f64>>, rng: &mut impl Rng) {
    for i in 0..array.len() {
        for j in 0..array[i].len() {
            array[i][j] = rng.random_range(1.0..100.0);
        }
    }
}
#[test]
fn test_very_big_vec() {
    let N = 8 * 1024 * 1024; // 8 MB
    let mut rng = rand::rng();

    let mut array = vec![vec![0.0; N]; 2];

    rand_array(&mut array, &mut rng);

    assert_eq!(array.len(), 2);
    assert_eq!(array[0].len(), N);
    assert!(array[0].iter().all(|&x| x >= 1.0 && x < 100.0));
}
```

我们其实不用管那么多，我们只是按照以下原则：

1. 参数不修改，用`&`
2. 参数需要修改，用`&mut`。

所以我们先不管，申明函数写`array: &Vec<Vec<f64>>`，调用函数写`& array`，就跟C语言一样，前面是形参，后面是实参。

然后我们`cargo check`，看看哪里出错，然后我们改成`array: &mut Vec<Vec<f64>>`和`&mut array`。

只要从不转移所有权，那么所有权就跟我没有任何关系！

## 求解简谐振动的ODE

![简谐振动](/rust/rust-for-stupid-engineers/harmonic_oscillator.gif)

### 二阶常系数线性微分方程

考虑二阶常系数线性微分方程 $x'' = -x$，初始条件为$x(0) = 1$，$x'(0) = 0$。

1. **特征方程求解**：
   - 设解的形式为 $x(t) = e^{rt}$
   - 代入方程得到特征方程：$r^2 + 1 = 0$
   - 解得特征根：$r = \pm i$

2. **通解形式**：
   - 对于复根 $r = \pm i$，通解为：
     $$x(t) = C_1 \cos(t) + C_2 \sin(t)$$

3. **确定特解**：
   - 根据初始条件 $x(0) = 1$ 和 $x'(0) = 0$：
     - $x(0) = C_1 = 1$
     - $x'(0) = C_2 = 0$
   - 因此特解为：$x(t) = \cos(t)$
   - 对应的导数为：$x'(t) = -\sin(t)$

4. **解的物理意义**：
   - 描述了一个简谐振动
   - 振幅为1，周期为 $2\pi$
   - 在相空间中形成单位圆：$x^2 + (x')^2 = 1$

5. **验证**：
   - 二阶导数：$x''(t) = -\cos(t) = -x(t)$
   - 初始条件：$x(0) = 1$，$x'(0) = 0$

### Rust实现

这个方程，可以写成对应的ODE：

$$
\begin{cases}
x' = y \\
y' = -x
\end{cases}
$$

我本来可以实现一个ODE45，但是我就不，我要实现一个Fehlberg的6阶方法。这里定义了一个`OdeFunc`，注意看，我们的`func`函数，需要传入`t`，`x`，`dx`。`t`是时间，`x`是状态，`dx`是状态的变化量，并且按照我前面的原则，`dx`需要是可变的。

```rust

pub trait OdeFunc {
    fn func(&self, t: f64, x: &Vec<f64>, dx: &mut Vec<f64>);
    fn dimension(&self) -> usize;
}

```

针对这个`trait`，我们实现一个求解的算法。

```rust
pub fn rk6<F: OdeFunc>(
    ode_func: &F,
    t0: f64,
    t1: f64,
    x0: Vec<f64>,
    h: f64,
) -> Vec<(f64, Vec<f64>)> {
    let mut t = t0;
    let mut x = x0.clone();
    let mut trajectory = vec![(t, x.clone())];
    let n = ode_func.dimension();
    let mut k1 = vec![0.0; n];
    let mut k2 = vec![0.0; n];
    let mut k3 = vec![0.0; n];
    let mut k4 = vec![0.0; n];
    let mut k5 = vec![0.0; n];
    let mut k6 = vec![0.0; n];
    let mut dx = vec![0.0; n];

    while t < t1 {
        let h_actual = if t + h > t1 { t1 - t } else { h };

        // 计算 Runge-Kutta 的系数
        ode_func.func(t, &x, &mut k1);
        for i in 0..n {
            dx[i] = x[i] + h_actual * k1[i] / 4.0;
        }
        ode_func.func(t + h_actual / 4.0, &dx, &mut k2);
        for i in 0..n {
            dx[i] = x[i] + h_actual * (3.0 * k1[i] + 9.0 * k2[i]) / 32.0;
        }
        ode_func.func(t + 3.0 * h_actual / 8.0, &dx, &mut k3);
        for i in 0..n {
            dx[i] = x[i] + h_actual * (1932.0 * k1[i] - 7200.0 * k2[i] + 7296.0 * k3[i]) / 2197.0;
        }
        ode_func.func(t + 12.0 * h_actual / 13.0, &dx, &mut k4);
        for i in 0..n {
            dx[i] = x[i] + h_actual * (439.0 * k1[i] / 216.0 - 8.0 * k2[i] + 3680.0 * k3[i] / 513.0 - 845.0 * k4[i] / 4104.0);
        }
        ode_func.func(t + h_actual, &dx, &mut k5);
        for i in 0..n {
            dx[i] = x[i] + h_actual * (-8.0 * k1[i] / 27.0 + 2.0 * k2[i] - 3544.0 * k3[i] / 2565.0 + 1859.0 * k4[i] / 4104.0 - 11.0 * k5[i] / 40.0);
        }
        ode_func.func(t + h_actual / 2.0, &dx, &mut k6);

        // 更新状态
        for i in 0..n {
            x[i] += h_actual
                * (16.0 * k1[i] / 135.0
                    + 6656.0 * k3[i] / 12825.0
                    + 28561.0 * k4[i] / 56430.0
                    - 9.0 * k5[i] / 50.0
                    + 2.0 * k6[i] / 55.0);
        }
        t += h_actual;
        trajectory.push((t, x.clone()));
    }

    trajectory
}
```

然后在`main`函数中，我们就可以这样使用：

```rust
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
```

最后整个脚本把数据画出来：

```python
{{% codeseg "/static/rust/rust-for-stupid-engineers/scripts/visualize_results.py" %}}
```

![](/rust/rust-for-stupid-engineers/results_plot.png)

还有那个动画脚本：

```python
{{% codeseg "/static/rust/rust-for-stupid-engineers/scripts/animate_results.py" %}}
```

## 总结

其实Rust拿来做计算还是挺方便的。我还没有开始用更加好用的`ndarray`和相关的线性代数包。

工程师搞什么优雅，能在堆栈上解决，咱们就堆栈解决；实在不行，用`Vec<T>`解决，然后全部搞不可变引用`&Vec<T>`，在编译器的指导下把部分地方改成可变引用`&mut Vec<T>`。
