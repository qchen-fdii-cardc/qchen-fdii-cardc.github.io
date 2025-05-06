+++
title = 'Maze Wasm in Rust实现WASM边缘计算'
date = 2025-05-04T11:02:28+08:00
draft = false
mathkatex = true
categories = ['rust', 'wasm', 'javascript']
tags = ['rust', 'wasm', 'javascript', 'game', 'maze', 'DFS algorithm', '迷宫生成']
toc = true
tocBorder = true
+++

## 迷宫生成

突然想做一个生成迷宫的游戏给小朋友玩，于是就开始整。就大概是下面这样的，唯一需要考虑的就是必须有一个从入口到出口的路径。

![迷宫游戏](/rust/maze-wasm-in-rust/mazes/maze-41x41.png)

一开始的想法是把迷宫用字符数出来，墙就采用黑色方块字符，路径就用空个字符。几下子小朋友就觉得黑窗口里面的字符太丑。然后就考虑是不是用JavaScript在浏览器中绘制，会不会好看一些。、

```html
{{% codesnap "static/rust/maze-wasm-in-rust/game-js.html" %}}
```

JavaScript代码来实现一个DFS深度优先搜索算法的迷宫生成。

```javascript
{{% codesnap "static/rust/maze-wasm-in-rust/maze-js.js" %}}
```

- [Maze App by JavaScript](/rust/maze-wasm-in-rust/game-js.html)

程序运行非常完美，提供了生成迷宫、下载迷宫为PNG，在小朋友的建议下，增加了改变颜色的功能。

不得不感叹，Javascript的效率真高，编起来很快，运行起来更快。生成好几百个格子的迷宫，只需要一眨眼的时间。完全没有性能问题。

~全文完。~

## 怎么才能做到没有苦硬吃？

前面有人跟我说，我是探索Rust来做ADT表达式没有苦硬吃。当然Rust来搞什么树状数据机构是有点烦人的。我还给自己辩解，在工程上，当然要考虑更好的性能、更好的开发体验，但是学习和研究就应该投入足够的精力在极限场景、边缘场景。不这样，就不能更深的理解，`是故君子无所不用其极`。

那么，看，我又开始没有苦硬吃，把这个迷宫生成的算法用Rust来实现。并且，还要把Rust实现的算法嫁接到JavaScript中。

```html
{{% codesnap "static/rust/maze-wasm-in-rust/maze-app.html" %}}
```

- [Maze App by Rust](/rust/maze-wasm-in-rust/game.html)

感觉上，貌似没有任何区别。

引用Rust所编制的库，在JavaScript代码中：

```javascript
{{% codesnap "static/rust/maze-wasm-in-rust/maze.js" %}}
```

首先是：

```javascript
import init, { Cell, Maze } from "./pkg/hello_wasm.js";
```

从`pkg/hello_wasm.js`中导入`init`、`Cell`、`Maze`。然后就是调用`init`函数，初始化Rust的代码。

```javascript
init().then(() => {
    // 初始化完成
});
```

`Cell`、`Maze`是Rust中定义的类型，用于实现具体的迷宫生成算法。

```javascript
const maze = new Maze(41, 41);
var size = maze.get_dimension()[0];
var start = maze.get_start();
var end = maze.get_end();
var cell = maze.get_cell(x, y);
```

大概，接口就是如此简单。Javascript中调用起来也没有多大区别。

## Rust怎么生成WASM？

### 手工生成

Rust生成WASM的步骤，网上有很多教程。

首先，用`cargo`建立一个库工程，然后在`Cargo.toml`中把`crate-type`从默认的`lib`改为`cdylib`。

```toml
[lib]
crate-type = ["cdylib"]
```

这里的`cdylib`是表示这是一个C语言的动态链接库；另外还有`lib`,`dylib`，分别表示这是一个静态链接库和动态链接库。因为我们需要在JavaScript中调用，所以需要生成一个C语言的动态链接库。

```bash
cargo build --target wasm32-unknown-unknown --release
```

这里，`--target`指定目标平台为`wasm32-unknown-unknown`，`--release`指定编译优化等级为`release`。

编译完成后，在`target/wasm32-unknown-unknown/release`目录下，会生成一个`project_name.wasm`文件。

然后，我们需要为`project_name.wasm`生成一个`project_name.js`文件。Javascript调用WASM的代码还是挺烦人的。

```javascript
async function init() {
    const {instance } = await WebAssembly.instantiateStreaming(
        fetch("project_name.wasm")
    );
     const some_function = instance.exports.some_function;
     const result = some_function();
     console.log(result);
}
```

大概就是这类东西。当然，为了导出`cdylib`，所有需要导出的函数还需要标注成：

```rust
// 保持函数名不变，不进行混淆，否则在JavaScript中调用会找不到
#[unsafe(no_mangle)]   // Only after Rust 1.82
pub fn some_function() -> i32 {
    1
}
```

大概，就是这类东西。还挺麻烦的。但是Rust的整个工具链都是开源的，所以有一个更加方便的工具，叫做`wasm-pack`。

### wasm-pack

`wasm-pack`是一个用于将Rust编译为WASM的工具。它可以帮助我们更方便地生成WASM文件，并提供一些额外的功能。

```bash
cargo install wasm-pack
```

安装`wasm-pack`。这样就能把Rust的代码编译为WASM，并且自动生成JavaScript的绑定文件。

在Rust代码中，要导出的函数、结构体、枚举等，需要标注成：

```rust
#[wasm_bindgen]
pub struct SomeStruct {
    // ...
}

#[wasm_bindgen]
pub fn some_function() -> i32 {
    1
}

#[wasm_bindgen]
pub enum SomeEnum {
    // ...
}
```

例如，我们的迷宫生成算法代码[src/lib.rs](/rust/maze-wasm-in-rust/src/lib.rs)，需要导出`Maze`结构体和`generate_maze`函数。

```rust
{{% codesnap "static/rust/maze-wasm-in-rust/src/lib.rs" %}}
```

最终运行：

```bash
wasm-pack build --target web
```

这里，`--target web`指定目标平台为Web。编译完成后，在`pkg`目录下，会生成一个`project_name_bg.wasm`文件和`project_name.js`文件。我们只需要把`project_name_bg.wasm`文件和`project_name.js`文件拷贝到JavaScript项目中，就可以直接在JavaScript中调用Rust的代码了。

就在[maze.js](/rust/maze-wasm-in-rust/maze.js)中，我们就可以看到：

```javascript
import init, { Cell, Maze } from "./pkg/project_name.js";
```

整个过程非常丝滑。当然，在本地直接打开html看不到效果，必须是某种方式的部署，最简单的就是用`http-server`。

```bash
python -m http.server --bind localhost 8080
```

然后，在浏览器中打开`http://localhost:8080/maze.html`，就可以看到效果了。

当然，每次我们更改了代码，重新编译wasm并生成js文件之后，就需要重新加载页面（硬重载，大概是Ctrl+F5），浏览器才会重新加载wasm文件。

## 结论

Rust生成WASM，使用`wasm-pack`是最方便的。参考[wasm-pack](https://github.com/rustwasm/wasm-pack)。虽然我还是没有太看到是否用Rust来生成WASM的必要性……
