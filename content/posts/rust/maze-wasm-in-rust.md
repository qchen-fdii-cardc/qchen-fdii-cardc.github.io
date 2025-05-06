+++
title = 'Maze Wasm in Rust'
date = 2025-05-04T11:02:28+08:00
draft = true
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

JavaScript代码来实现一个迷宫生成。

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

- [Maze App](/rust/maze-wasm-in-rust/game.html)

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

## 怎么在HTML中使用WASM？

## 结论
