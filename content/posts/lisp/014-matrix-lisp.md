+++
title = '013 恶作剧先生的黑客帝国特效'
date = 2024-11-18T12:43:58+08:00
draft = false
mathjax = false
categories = ['lisp']
tags = ['lisp', 'tutorial', 'ansi', 'terminal', 'matrix']
toc = true
tocBorder = true
+++


## 黑客帝国特效

### 好好先生被整蛊

好好先生天天把WSL2的Ubuntu启动起来Alt+Enter，在终端里认真学习Lisp编程。这天，他的终端突然变成这样，耳朵旁边还传来了一句话：


> You ever have that feeling where you're not sure if you're awake or still dreaming?
>
> 你有没有那种感觉，你不确定自己是醒着还是在做梦？

{{<rawhtml>}}

<video width=100% controls autoplay="autoplay" loop="loop">
    <source src="/video/matrix.mp4" type="video/mp4">
    Your browser does not support the video tag.
</video>

{{</rawhtml>}}


……不用说，肯定是恶作剧先生干的！好好先生知道自己本来就是一个虚拟的漫画人物！他怎么会有这种无聊的感觉！不过，效果还真是可以，熟练按下`Ctrl+C`，终端恢复了正常。好好先生决定居然发现命令行里运行的上一个命令是：

```shell
sbcl --load "matrix4.lisp" --eval "(run-matrix-effect `tput cols` `tput lines`)"
```

啊哈，居然使用Lisp写了一个黑客帝国特效！好好先生决定学习一下这个特效的实现方法。

### 特效代码

- [源代码下载链接](/lisp-code/matrix4.lisp)

代码很短，不到100行，但是实现了一个非常炫酷的黑客帝国特效。这个特效会在终端中显示绿色的字符，字符会从屏幕上方掉落，就像黑客帝国电影中的那个特效一样。这个特效会一直运行，直到程序被中断，还会把终端的颜色恢复到默认值。


## 原理解析

好好先生看了一下代码，发现这个黑客帝国特效的原理其实很简单：

1. 使用ANSI转义码清空终端屏幕，设置文本颜色为绿色。
2. 生成一个二维数组，每个位置存储一个字符，字符是随机生成的，有一定概率是空格。
3. 每次更新终端屏幕时，只更新有变化的字符。
4. 每次更新时，随机生成新的字符，然后将字符向下移动一行。
5. 循环执行步骤3和4，直到程序被中断，然后重置文本颜色。

### ANSI转义码

[ANSI转义码](https://en.wikipedia.org/wiki/ANSI_escape_code)是一种用于控制文本终端的特殊字符序列。通过在终端输出这些特殊字符，可以实现一些高级的文本显示效果，比如清空屏幕、移动光标、设置文本颜色、设置背景颜色等。通常，这个字符串序列描述为类似于`033[xxxx;`这样的奇怪东西，其中`033`是转义字符`ESC`，`xxxx`是一系列以分号分隔的参数。大部分终端都支持ANSI转义码，包括Linux终端、macOS终端、Windows终端等。

我们在Linux下面用`man ascii`可以看到下面的信息，对应与ASCII码表中的`ESC`字符，其十进制值为27，八进制值为033，十六进制值为0x1B。

![](/lisp-img/ascii.png)

### Lisp中输出ANSI转义码
在Lisp中，可以使用`format`函数输出ANSI转义码，实现一些高级的文本显示效果。这里首先是使用`~c`格式输出一个字符，然后用字面量`#\Escape`表示转义字符`ESC`。

一些常用的ANSI转义码输出方式在Lisp中大概是这样的：

```lisp
(format t "~c[2J~c[H" #\Escape #\Escape) ; Clear screen
(format t "~c[32m" #\Escape) ; Set text color to green
(format t "~c[0m" #\Escape) ; Reset text color to default
(format t "~c[10;20H" #\Escape) ; Move cursor to row 10, column 20
```

### 随机字符生成

在Lisp中，字符是一种数据类型，可以使用`#\`前缀来表示一个字符。比如`#\Space`表示空格字符，`#\A`表示大写字毸A，这个描述对应与`man ascii`中的名称（Char对应的列）。同时，对照ASCII码表，可以使用`code-char`函数将一个整数转换为对应的字符，也可以把一个字符用`char-code`转换为对应的整数。

```lisp
(code-char 65) ; => #\A
(char-code #\A) ; => 65
```

那么随机产生字符的函数，选择从33到126之间的字符，这个范围包含了大部分可打印的ASCII字符。此外，为了让屏幕上的空白更多一些，用`(zerop (random n))`来以1/n的概率直接返回一个空格字符。

```lisp
(defun random-char ()
  "Generate a random character with a higher probability of being a space."
  (if (zerop (random 5)) ; 1/20 chance to return a space
      #\Space
      (code-char (+ 33 (random 94))))) ; Characters from '!' to '~'
```

### 二维数组操作

因为在终端中显示的字符有行和列的概念，所以用二维数组来存储屏幕上的字符是比较自然的。Lisp的二维数组，可以用`make-array`函数创建，用`aref`函数访问。这里的`buffer`和`prev-buffer`都是二维数组，用来存储屏幕上的字符。`columns`是一个一维数组，用来存储每一列对应会被更新的行数。

这里恶作剧先生使用基本的数组操作运算和循环来实现二维数组的复制和更新。

```lisp
(defun copy-2d-array (src target w h)
  "Copy the contents of a 2D array to another 2D array."
  (loop for row from 0 below h do
          (loop for col from 0 below w do
                  (setf (aref target row col) (aref src row col)))))
```

### 实现字符往下掉

实现字符往下掉的关键是一个`random-2d-array-with-columns`函数，这个函数会在每一列的某个位置（`columns`变量记录的值，初始为0）生成一个新的字符；然后将这个位置向下移动一行；如果这个位置已经到达最底部，那么将这个位置重置到最顶部。当然，为了增加随机性，每个更新周期，也有一定的概率重新产生一个列的随机行作为起点。这个随机的行为同样采用`(zerop (random n))`的方式来控制。

```lisp
(defun random-2d-array-with-columns (buffer columns width height)
  (loop for col from 0 below width do
          (when (zerop (random 10)) ; 1/10 chance to start a new character
                (setf (aref columns col) (random height)))
          (setf (aref buffer (aref columns col) col) (random-char))
          (incf (aref columns col))
          (when (>= (aref columns col) height)
                (setf (aref columns col) 0))))
```

最后就是，比较两个二维数组，只更新有变化的字符，这样可以减少输出的字符数量，提高效率。

```lisp
(defun update-with-changed-chars (buffer prev-buffer width height)
  "Update the terminal with only the changed characters."
  (loop for row from 0 below height do
          (loop for col from 0 below width do
                  (unless (char= (aref buffer row col) (aref prev-buffer row col))
                    (move-cursor row col)
                    (format t "~c " (aref buffer row col))))))
```

理论上，这里还可以进一步优化，比如只更新有变化的行，而不是每个字符都比较。不过，这个简单的实现已经足够了。

### 最终的程序框架

有了上述工具，最终的实现就很简单。

```lisp
(defun matrix-effect (width height delay-time)
  "Simulate the Matrix effect with green characters falling down the terminal."
  (clear-screen)
  (set-green-color)
  (loop
 with columns = (make-array width :initial-element 0)
 with buffer = (make-array (list height width) :initial-element #\Space)
 with prev-buffer = (make-array (list height width) :initial-element #\Space)
 for i from 0
 do (progn
     ; Update buffer with new characters
     (random-2d-array-with-columns buffer columns width height)
     ; Draw only the changed parts of the buffer
     (update-with-changed-chars buffer prev-buffer width height)
     ; Copy buffer to prev-buffer
     (copy-2d-array buffer prev-buffer width height)
     ; Sleep for a short delay
     (sleep delay-time)
     ; Force the output to be displayed
     (finish-output))))
```

这里的`matrix-effect`函数，接受三个参数：终端的宽度、高度和更新的延迟时间，就是简单的无限循环上面的算法。


## 与终端交互

最后，为了让黑客帝国特效能够自适应终端的大小，恶作剧先生试图使用`uiop:run-program`函数调用`"tput cols"`和`"tput lines"`命令来获取终端的宽度和高度。这样，黑客帝国特效就可以在任何终端上运行，并且自动适应终端的大小。

```lisp
;;; call `tput cols` and `tput lines` to get the terminal size
(defun get-width ()
  "Get the width of the terminal."
  (parse-integer (uiop:run-program '("tput" "cols") :output :string)))

(defun get-height ()
  "Get the height of the terminal."
  (parse-integer (uiop:run-program '("tput" "lines") :output :string)))

;; Run the Matrix effect with the terminal size and a delay time of 0.05 seconds
(defun run-matrix-effect (&optional (w nil) (h nil) (delay-time 0.05))
  ; dealing with errors and Ctrl+C
  (let ((w (or w (get-width)))
        (h (or h (get-height))))
    (unwind-protect
        (matrix-effect w h delay-time)
      (reset-color))))
```

可惜并不成功，因为`run-program`每次都会重新启动一个新进程，这个进程默认的行数和列数分别为24和80，而不是当前终端的大小。所以，这个方法并不适用于获取终端的大小。

所以，最终恶作剧先生采取了命令行中传入终端的大小的方式，这样就可以在命令行中直接运行黑客帝国特效了。

```shell
sbcl --load "matrix4.lisp" --eval "(run-matrix-effect `tput cols` `tput lines`)"
```

这里面的反引号在终端命令中会替换为运行该命令的结果，这样就可以直接获取终端的大小了。

## 总结

恶作剧先生只有在进行恶作剧的时候才会动脑筋，他可真是个傻瓜啊……