+++
title = 'Egui First Try 使用Rust开发一个简单的UI'
date = 2025-04-27T14:15:26+08:00
draft = true
mathkatex = true
categories = ['rust']
tags = ['rust', 'egui', 'checklist', 'github pages', 'rust-lang', '清单革命', 'git branch', 'python', 'icon', 'font', 'font-cutter', '字体裁剪']
toc = true
tocBorder = true
+++

## Why

我用Git有一些时间，不过一直是单线git flow，所有的库都是从master/main上拉取，直接在master/main上开发，然后push到远程仓库。因为我的[Github Pages](<https://www.windtunnel.cn>)是基于Github Actions自动部署的，每次main分支有push，就会自动部署。这个功能一直运行得很好，在前面我写文章的过程中，我一直有一个很不好的感觉，就是commit之后，对推送（附带的Github Actions）有很强的抗拒，总觉得也没给钱而且用了计算时间就相当于排放了二氧化碳。

直到今天，我突然意识到，我可以随便push，只要我checkout到其他分支！这也是为什么Git的分支为什么那么轻，因为Git鼓励多用分支，把所有的工作都正交化。作为一个老年人，我为自己骄傲……我居然还天天都有进步！总之，进步的空间还有很大。

当然我也可能不承认我居然这么笨……我就说我就喜欢用master/main……绝不承认是我没有意识到，用分支工作是git的设计意图。

![心安理得](/rust/egui-first-try/logo.png)

## 《The Checklist Manifesto》

这是一本我很喜欢的书，作者是[Atul Gawande](<https://atulgawande.com/book/the-checklist-manifesto/>)，他是一位外科医生，也是一位作家。这本书是关于医疗领域的，但是我觉得在软件开发领域，这本书同样适用。

![The Checklist Manifesto](/rust/egui-first-try/thebook.jpg)

这本书的副标题是：How to Get Things Done That Matter，中文翻译为[《清单革命》微信读书](<https://weread.qq.com/web/bookDetail/250321505e0be425019aa06>)。

我马上（大概4个小时）就制定了一个清单，GitHub Pages文章发布清单！

![GitHub Pages文章发布清单](/rust/egui-first-try/checklist.png)

一定每次都更新本地文件，新建分支，……

最重要的是，要把文章的draft改为false，然后再合并分支，推送到远程仓库。

## 又是不做领域专家的一天

今天根本没有评审、也不需要开会，那当然就是R神启动的一天。

既然需求都那么清楚，当然是编个小程序，顺便看看Rust写界面怎么样。

- [Rust神启动：Githhub Pages Checklist](<https://github.com/qchen-fdii-cardc/hugo-post-checklist>)
- [Windows-x64 app](<https://github.com/qchen-fdii-cardc/hugo-post-checklist/releases/download/windows-11-x64/hugo-post-checklist.exe>)

我倒是在wsl上编译了一个，感觉不太靠谱的样子，回家在Ubuntu上试一下。

总体感觉：

1. 单文件发布感觉非常爽；
2. 程序的尺寸在合理的范围，就这个没啥功能的玩意，只有5MB多；
3. 编写过程非常流畅；
4. windows和wsl都能很容易编译，感觉非常棒；
5. R神启动，感觉非常爽！

### 使用Egui

[egui](<https://github.com/emilk/egui>)是一个将近有2.5万star的Rust> UI库，我也没仔细看，就随便tab，tab，tab一路下来。

有空把代码好好看看，下次一定……下次一定……

### UI设计

因为这个需求和内容过于简单，我把主要精力放在了小清新上。

- 使用了[小清新开源字体：霞鹜文楷](<https://github.com/lxgw/LxgwWenKai>)
- 下载[字体文件](<https://github.com/qchen-fdii-cardc/hugo-post-checklist/raw/refs/heads/main/fonts/%E9%9C%9E%E9%B9%9C%E6%96%87%E6%A5%B7.ttf>)
- 使用了自行设计的清新ICon
- 界面的风格很清新

### 字体裁剪

因为我们想要的是单一的exe，所以需要裁剪字体。我们用的是[pyftsubset](<https://fonttools.readthedocs.io/en/latest/subset/index.html#>)，这是一个字体工具，可以用来裁剪字体。编了一个脚本，把我们所需要的字符裁剪出来。

```python
{{% codeseg "/static/rust/egui-first-try/generate_font.py" %}}
```

为了裁剪方便，我们把程序中的所有文字都放在一个mod中定义：

```rust
{{% codeseg "/static/rust/egui-first-try/text.rs" %}}
```

本来我还想整理一下，就是被注释掉的那点代码，后来感觉没有太大收益就算了。

这个Python基本产生一个文件`custom_font.ttf`，然后就可以用这个字体了。

在程序中，我们用了一个宏`include_bytes!`，直接把字体文件嵌入到exe中。

```rust
{{% codeseg "/static/rust/egui-first-try/main.rs" 119 122 %}}
```

### Icon生成

虽然可以在网上下载一个Icon但是，这样不够小清新，我决定用Python生成一个。

```python
{{% codeseg "/static/rust/egui-first-try/generate_icon.py" %}}
```

![app.icon](/rust/egui-first-try/app.ico)

真的还挺好看的。

### 主程序

整个程序简直是乏善可陈，

```rust
{{% codeseg "/static/rust/egui-first-try/main.rs" %}}
```

### Windows打包

```rust
{{% codeseg "/static/rust/egui-first-try/build.rs" %}}
```

## 总结

很好很强大！
