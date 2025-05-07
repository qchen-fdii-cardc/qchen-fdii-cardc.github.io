+++
title = 'Rust for Matlab里面调用Rust函数之为什么要这么做'
date = 2025-05-06T17:24:25+08:00
draft = true
mathkatex = true
categories = ['rust', 'matlab']
tags = ['rust', 'matlab', 'c', 'abi', 'pointer']
toc = true
tocBorder = true
+++

## 因为能

前面写[WASM](/posts/rust/maze-wasm-in-rust/)的时候，发现名为WASM实则是一个C语言的动态链接库。

既然如此，为什么不能用Rust来写一个动态链接库，然后Matlab来调用呢？

虽然，不知道为什么要这么干。实际上，我连用C/C++写一个动态链接库然后Matlab来调用都嫌没必要，最多是写一个可执行文件，然后处理输入到输出，然后matlab调用这个可执行文件。

就当做是没苦硬吃。

## 那就开始

首先，建立一个Rust的库。

```bash
cargo new --lib rs2m
```

然后在`Cargo.toml`中添加

```toml
[lib]
crate-type = ["cdylib"]
```

变成：

```toml
{{% codesnap "static/rust/rust4matlab/Cargo.toml" %}}
```

然后，写一个简单的函数，然后导出。

```rust
{{% codeseg "static/rust/rust4matlab/src/lib.rs" 1 30 %}}
```

注意到这里，我们每定义一个函数，都要在前面加上`#[no_mangle]`，这个是告诉编译器，这个函数不要进行名字修饰，否则在Matlab中调用的时候会找不到。

然后，在函数的返回值前面加上`pub extern "C" fn`，这个是告诉编译器，这个函数是导出的，然后`"C"`表示这个函数是按照C语言的规则来导出的。

普通的值的传递乏善可陈，就是简单的对应关系，可以在[Comparing C/C++ types to Rust](<https://locka99.gitbooks.io/a-guide-to-porting-c-to-rust/content/features_of_rust/types.html>)中找到；然后C/C++和Matlab的对应可以不太在意，因为Matlab是一个工程师的懒人语言，通常不纠结数据类型，除非算不出来。

唯一好玩一点点的就是，浮点数组的传递，这里需要用`*mut f64`来表示，然后Matlab中需要用`libpointer('doublePtr', result)`来表示。这个`*mut f64`是Rust的原始指针类型。

```rust
{{% codeseg "static/rust/rust4matlab/src/lib.rs" 11 30 %}}
```

这个代码中就有对原始指针的两个操作，`is_null()`和`add()`。

`is_null()`是判断指针是否为空，`add()`是获取指针指向的值。我们使用了`unsafe`来操作原始指针指向的值。

```bash
cargo build --release
```

Rust这一边的事情就全部完成。最终我们我们也弄了点测试，假装测试一下。

```rust
{{% codeseg "static/rust/rust4matlab/src/lib.rs" 31 %}}
```

当然，这里测试中的原始指针就通过`Vec<f64>::as_mut_ptr()`来获取。

```bash
cargo test
```

## Matlab与C语言的接口

基本上C语言的接口，ABI，是计算机中非常非常通用的。Matlab也不例外，提供了一系列函数来调用。

比较重要的函数有：

- `loadlibrary`：加载动态链接库
- `calllib`：调用动态链接库中的函数
- `libfunctions`：查看动态链接库中的函数
- `libisloaded`：查看动态链接库是否加载
- `unloadlibrary`：卸载动态链接库
- `libstruct`：创建一个结构体
- `libgetptr`：获取一个结构体的指针

大概，也就是这么多，可以用Matlab的帮助去查看如何使用。

我们上面的库，定义的函数非常简单，所以头文件也可以非常简单。

```C
{{% codeseg "static/rust/rust4matlab/matlab/rs2m.h" 12 18 %}}
```

整个文件：

- [rs2m.h](/rust/rust4matlab/matlab/rs2m.h)

主要是引用`stdint.h`，来访问`uint64_t`、`int32_t`这些更加具有一致性的类型定义。

然后，我们就可以在Matlab中调用这个库了。

```matlab
[~, ~] = loadlibrary('rs2m.dll', 'rs2m.h'); 
calllib('rs2m', 'add', uint64(2), uint64(3));
arr = libpointer('doublePtr', zeros(1, 11));
calllib('rs2m', 'linspace', 0, 1, 11, arr);
calllib('rs2m', 'square', 2);
% 卸载库
unloadlibrary('rs2m');
```

我们通常会包装一层，来方便使用。

```matlab
{{% codeseg "static/rust/rust4matlab/matlab/rs2m.m" %}}
```

代码块11中的13行，我们在调用`loadlibrary`函数时，还提供了一组参数让matlab输出一个m文件，这个m文件可以获得所有方法的接口信息。

测试代码：

```matlab
{{% codeseg "static/rust/rust4matlab/matlab/test.m"  %}}
```

正常运行。C语言的使用方式简单可靠，唯一就是那个`libpointer`，有点烦人，我一直没找到如何设置数组尺寸，只能通过构造一个初始值的方式，如果这个数组很大，那就太坑了。

接着往下看，Matlab还提供了一套跟C++的接口，叫做`clib`。稍微看了一下就感觉，这个更有搞头。

## Matlab与C++的接口

这个方式来调用编译好的库，貌似需要三个文件：

- `rs2m.h`：头文件
- `rs2m.dll`：库文件
- `rs2m.lib`：库文件的桩

如果是cpp源文件，那就直接使用头文件和源文件即可。当然，可能需要安装个什么c++的编译器。在windows上，安装一个Visual C++社区版，然后就可以使用。我们这里的库文件，`cargo build --release`，就会生成。头文件跟刚才相同。

```matlab
clibgen.generateLibraryDefinition("rs2m.h", Libraries="rs2m.dll", OutputFolder="rs2mlib-cpp");
```

这个命令会生成一个`rs2mlib-cpp`的文件夹，里面包含一个`definers2m.m`文件和另外一个`xml`文件，这个文件就是我们要的包装文件。

这里唯一一个坑，就是`definers2m.m`文件中，有些函数的定义需要补充。对于我们这几个函数，就是`linspace`函数，需要我们自己补充参数的维度。

```matlab
{{% codeseg "static/rust/rust4matlab/matlab/rs2mlib-cpp/definers2m.m" 31 43 %}}
```

我么需要把这个注释去掉，然后把`<SHAPE>`补充完整。

```matlab
{{% codeseg "static/rust/rust4matlab/matlab/rs2mlib/definers2m.m" 41 41 %}}
```

注意这里，用的`"n"`，也就是原来方法的第三个参数，实际上，定义了这个尺寸之后，`clib.rs2m.linspace`的第三个参数`n`就被省略了，直接包含在第四个参数中。

然后就可以使用`build`命令来生成dll文件了。

```matlab
build(definers2m);      
```

这个命令会生成一个`dll`文件：`rs2mInterface.dll`，有了这两个dll文件，就可以在Matlab中调用这个库了。当然调用之前，建议先做一个设置：

```matlab
clibConfiguration('rs2m', 'ExecutionMode', 'outofprocess');
```

如果运行模式为`inprocess`，则需要重新启动Matlab才能卸载该库，当然我们使用了`outofprocess`，所以不需要重新启动Matlab就可以卸载该库。

```matlab
clibConfiguration('rs2m').unload();
```

然后就可以在Matlab中调用这个库了。

```matlab
{{% codeseg "static/rust/rust4matlab/matlab/test_cpp.m" %}}
```

正常运行。

## 总结

忙活挺长时间才搞定这个，Matlab真是无所不能的。
