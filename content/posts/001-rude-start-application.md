+++
title = '001 粗鲁先生Lisp再出发'
date = 2024-07-23T10:21:34+08:00
draft = false
mathjax = false
categories = ['lisp', 'programming-language']
tags = ['lisp', 'programming', 'alive-lsp', 'vscode']
toc = true
tocBorder = true
+++


## 目标和梦想

目标和梦想是最近考虑的一个问题。什么是目标？什么是梦想？梦想可以激励改变，目标才能实现改变。

开始这个部分的时候，我的梦想是什么？我的目标是什么？我想要什么？我要做什么？

- 逃避手头的项目
- 逃避手头的工作

学习Lisp可以回溯到大学时期，为什么要学这么奇怪的东西呢？也看过《画家和黑客》这本书，时断时续地看过一些比较外围的Lisp博客，甚至书籍。但是Lisp从来没有成为生产力工具，也没有形成任何成果。

那么，到底行不行呢？

所以，这次被手头的工作压垮的时候，我又退回到曾经的路径，逃避到学习一个新的、或者旧的编程语言上。好多编程语言就是这样学会的……这个策略就跟失眠就去学微积分一样，要么学了一点人类最高级的智慧，要么困了就睡着了。

那么这一次Lisp的学习，又会怎么样呢？能不能让我在被死线压垮的时候，摆一个更加优雅的姿势呢？希望能够吧……Lisp如此优雅，这次我一定会死得很优雅吧。

## 实用的目标

跟以前学习Lisp不同的是，这次我想选择一个更加实用的目标。第一时间想到的是，我要不要把我经常写给别人的小工具，用Lisp来实现呢？通常，我用Python来写这些，但是给别人的时候总是有这样那样的问题。

比如说，真的很多人根本不可能装Python，所以我会感染每一个我接触的电脑，我用过之后，这些电脑上总是被被装上Python。

我会用pyinstaller打包，但是Pyinstaller打包的过程和结果，总是让人有一点点不放心。每次成功了我总是会觉得自己很幸运。

那么，Lisp是不是一个很好的选择呢？

根据我那些远古时代关于Lisp的记忆，加上一点点搜索，结论是：相当行。只要你能跟Lisp在一定的程度上不相互抵触……

这次重新开始Lisp，我准备戴上一顶粗鲁先生的帽子，简单粗暴地对待Lisp，毫不理会Lisp那些优雅、美好的特性，只要能用就行。
## 开始的过程

稍微看一下，现在开发Lisp大概有两个主要的流派（单纯是个人观点）：

1. Emacs Lisp
2. Common Lisp

开发环境同样有两个主要的流派：

1. Emacs
2. VSCode

这两个并不是完全对应的，因为Emacs啥都行，VSCode搞Emacs Lisp估计比较抽象。

我选择的是Common Lisp和VSCode。至于Common Lisp的实现，直接说结论：SBCL。

1. 安装SBCL
2. 安装VSCode
3. 安装alive插件
   1. 安装quicklisp
   2. 安装alive-lsp
   3. 安装alive插件
4. 开始写代码
5. 产生一个可执行文件

### SBCL安装

[SBCL](http://www.sbcl.org/)是Steel Bank Common Lisp的缩写，是一个Common Lisp的实现。在[这里](http://www.sbcl.org/platform-table.html)可以找到各种平台的安装方法。

或者，如果不是windows，可以直接用包管理器安装：

```shell
sudo apt-get install sbcl
```

安装好之后，可以考虑把SBCL的路径加到环境变量里面。之后就能在任意地方打开终端，输入`sbcl`，就能进入SBCL的REPL了。

```shell
This is SBCL 2.4.6, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
*
```

这就是成功安装的标志。在这个星号后面，就可以输入Lisp代码了。比如输入`(print "Hello, World!")`，然后回车，就会输出`"Hello, World!"`。输入`(quit)`或者`(exit)`，就会退出SBCL的REPL。后面还有一堆需要在这个REPL里面输入的命令。每次看到REPL，那就是说的这个东西。

SBCL软件的配置文件位置为`~/.sbclrc`，可以在这个文件中配置SBCL的一些参数。下面安装过程中会涉及这个文件。

### VSCode安装

VSCode是微软出品的一个编辑器，可以在[这里](https://code.visualstudio.com/)下载。这个就不需要我多说了，安装方法也很简单。

### alive插件安装

Alive插件本身是VSCode的一个插件，可以在插件市场直接搜索安装。但是Alive的正常运行就不是一个那么简单的事情了。

Alive的功能依赖于LSP，并不是lao se pi，是Language Server Protocol，是实现通用编辑软件中不同类型编程语言的语法分析和自动补全的协议。

所以，我们需要安装一个LSP的实现，这个实现就是alive-lsp。那么Common Lisp的软件包怎么管理呢？优雅的Common Lisp的软件包管理工具就是quicklisp。

#### quicklisp安装

1. 下载`quicklisp.lisp`文件，可以在[这里](https://www.quicklisp.org/beta/)找到，具体的下载链接是[这里](https://beta.quicklisp.org/quicklisp.lisp)，另存在一个目录下，比如`~/Downloads`。
2. 在这个目录中打开终端，或者打开终端，切换到第一步的目录。
3. 输入`sbcl --load quicklisp.lisp`，然后就会进入SBCL的REPL。
4. 输入`(quicklisp-quickstart:install)`，然后就会开始安装quicklisp。
5. 安装完成之后，输入`(ql:add-to-init-file)`，然后就会把quicklisp的初始化代码加到`~/.sbclrc`文件中，这里有一个确认的过程，不要以为不动了，按一下回车就行。
6. 输入`(quit)`，退出SBCL的REPL。

这个过程简单粗暴。貌似这几个网站访问也很正常，不想大型交友网站github那样，需要上科学技术。

唯一值得一提的是，`quicklisp.lisp`文件只有1757行，还真是一个小巧的东西，等我带上知识先生的帽子，也许我要好好读一读这个文件。


在SBCL的配置文件中，`quicklisp`加了几行：
    
```lisp
;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
```

`quicklisp`会给你的主文件夹下面生成一个`~/quicklisp`文件夹。从SBCL的配置文件可以看到，这个文件夹里面有一个`setup.lisp`文件，这个文件是`quicklisp`的初始化文件，可以用来加载`quicklisp`的软件包。

#### alive-lsp安装

这个插件是目前VSCode上可以用的Common Lisp实现，但是这个玩意有两个问题：
1. 不会自动安装SBCL
2. 不会自动安装alive-lsp
3. 自己不在`quicklisp`的软件库里

除此之外，这个软件目前看起来还是挺完美的。

所以，我们需要手动安装`alive-lsp`。不然的话，我们会一次一次看到报错“MISSING DEPENDENCY USOCKET”之类的。大概一共有四五个缺失的包，每次都要手动安装，然后重启VSCode，再次报错另外一个包，再次手动安装，再次重启VSCode……

鉴于我戴了粗鲁先生的帽子，智商的缺陷简直是肉眼可见。在我把上面的过程全部搞了一遍之后，我才想到可能直接安装alive-lsp可能是我所需要的，然后就发现`quicklisp`里面根本没有`alive-lsp`这个包。

```lisp
(ql:quickload :alive-lsp)      ;; 安装alive-lsp
(ql:system-apropos "alive")    ;; 查找alive包
```

最后，大型交友网站上看到[alive-lsp](https://github.com/nobody-famous/alive-lsp)，跑去看了Alive-LSP的文章。好的，这个玩意还需要自己去装。

倒是也不亏，学会了一个把自己的包弄到`quicklisp`里面的方法。

这几回到前面提到的文件夹`~/quicklisp`，这个文件夹里面有一个`local-projects`文件夹，把`alive-lsp`的文件夹放到这个文件夹里面，然后再次进入SBCL的REPL，输入`(ql:quickload :alive-lsp)`，就会自动安装`alive-lsp`了。

网络状态稳定的话，就在这个目录下使用下面的命令：

```shell
C:\Users\<<username>>\quicklisp\local-projects> git clone https://github.com/nobody-famous/alive-lsp.git
```

要把`<<username>>`替换成你的用户名。

如果网络不稳定，那么下面的链接是`alive-lsp`的压缩包，这个是我用`git archive`命令生成的：

```shell
git archive --format=zip --output=alive-lsp.zip HEAD
```

[alive-lisp.zip](./alive-lsp.zip)


#### alive插件使用

Alive插件的安装就很简单了，直接在VSCode的插件市场搜索安装就行。

安装好了之后，打开一个Lisp文件，然后就会自动启动`alive-lsp`，然后就可以使用LSP的功能了。

比如下面的命令面板里面增加了一个`REPL`的标签，可以直接在这个标签里面输入Lisp代码，然后执行。

### 一个小工具的实现

这部分部分就不用深入到`uiop`或者语法，或者什么SBCL了，只是展示可能性。

这里，实现一个统计软件行数的工具，整好写软件著作权也需要这个东西。

```lisp
;; lc.lisp
;; 从命令行获得一个表达式，然后统计这个表达式的文件的行数，列出每个文件的行数，最后列出总行数

(require :uiop)

(defun get-expr ()
  "Get the expression to find files, default is *.lisp"
  (let ((expr (uiop:command-line-arguments)))
    (if (null expr)
        "*.lisp"
        (car expr))))


(defun get-files (expr)
  (uiop/filesystem:directory-files
    #P"./"
    expr))

;; count lines in a file
(defun count-lines (file)
  (with-open-file (stream file)
    (loop for line = (read-line stream nil)
          while line
            count line)))

;; catch count-lines io errors
(defun count-lines-with-error (file)
  (handler-case
      (count-lines file)
    (error (c)
      (format t "=====~%Error: ~A~%======~%" c)
      0)))

;; count lines in files print each number and sum them
(defun main ()
  (let ((expr (get-expr)))
    (let ((sum 0))
      (dolist (file (get-files expr))
        (let ((lines (count-lines-with-error file)))
          (format t "~A: ~A~%" file lines)
          (incf sum lines)))
      (format t "Total: ~A~%" sum))))
```

好了这个软件已经写完了。接下来就是如何产生一个可执行文件。

```lisp
;; compile-lc.lisp
;; 产生一个可执行文件
(load "lc.lisp")
(sb-ext:save-lisp-and-die
  #p"lc.exe"
  :toplevel #'main
  :executable t)
```

有了这两个文件，就可以在这两个文件所在的目录下启动SBCL，然后输入`(load "compile-lc.lisp")`，就会在这个目录下生成一个`lc.exe`文件。

或者直接执行`sbcl --load compile-lc.lisp`，也会生成一个`lc.exe`文件。

最后就是这个软件的使用：

```shell
lc.exe
```

```shell
(base) PS C:\lisp-revisit> lc
C:/lisp-revisit/compile-helloworld.lisp: 5
C:/lisp-revisit/compile-lc.lisp: 5
C:/lisp-revisit/helloworld.lisp: 17
C:/lisp-revisit/lc.lisp: 39
Total: 66
```

这也挺不错的不是吗？这个跟`lc *.lisp`效果是一样的。

或者，还能统计子目录：

```shell
lc **/*.lisp
```

```shell
(base) PS C:\lisp-revisit> lc **/*.lisp
C:/lisp-revisit/.vscode/alive/fasl/tmp.lisp: 5
C:/lisp-revisit/compile-helloworld.lisp: 5
C:/lisp-revisit/compile-lc.lisp: 5
C:/lisp-revisit/helloworld.lisp: 17
C:/lisp-revisit/lc.lisp: 39
Total: 71
```

这个效果在windows的PowerShell和cmd上都可以使用。

## 总结

Common Lisp其实挺好用的。

1. 推荐使用SBCL作为Common Lisp的实现
2. Lisp可以使用VSCode作为编辑器
3. Lisp使用quicklisp作为软件包管理工具
4. 使用alive-lsp作为LSP的实现，配合alive插件
5. SBCL可以直接产生可执行文件，无依赖，单文件