+++
title = '001 Try It with JavaScript初见情'
date = 2024-11-23T14:20:15+08:00
draft = true
mathjax = false
categories = ['javascript']
tags = ['javascript', 'html', 'css', 'tutorial']
toc = true
tocBorder = true
+++

## 我如何开始整JavaScript

最近看了一个很扯的视频，就是说要想高效，就要缩短工作时间。不管你们信不信，反正我是信了。我缩减工作时间干什么呢？当然是摸鱼。
不钓鱼的退休干部就只能搞点人畜无害的东西，比如学一门新的程序设计语言……

前面知乎有个知友给大家安利JavaScript，我很多年前看了会那本

- [JavaScript: The Good Parts](https://www.oreilly.com/library/view/javascript-the-good/9780596517748/)

感觉还行，又有点怪怪的感觉，因为对HTML、CSS和Web技术先的不感兴趣，就先放在一边。

后面还是前面不记得，在用Anasys Workbench的时候，被迫编几个DesignModeler的脚本用的JavaScript，感觉很不爽……

这次跟知友论来论去，跑去把Lisp捡起来玩玩才发现，SCIP已经改用JavaScript啦！

- [SICP-Lisp](https://mitp-content-server.mit.edu/books/content/sectbyfn/books_pres_0/6515/sicp.zip/index.html)
- [MIT-SICP-Lisp](https://web.mit.edu/6.001/6.037/sicp.pdf)
- [SICP-JavaScript](https://mitpress.mit.edu/9780262543231/structure-and-interpretation-of-computer-programs/)

大受震撼……

我正好Lisp感觉没啥好整的，太大的现在有2个deadline，太小的整的没劲。那就JavaScript呗……

### JavaScript的例子

下面先看一个例子。

{{% htmlfile "/static/javascript/fact.html" %}}

上面的“应用”是用JavaScript写的，可以计算阶乘。你可以输入一个数字，然后点击“计算”按钮，就可以看到这个数字的阶乘。

其实，别的不说，就JavaScript这个运行速度，简直是盖了……这还不是尾递归，速度快的一塌糊涂，也


对应的HTML代码为：

```html    
{{% codesnap "/static/javascript/fact.html" %}}
```

下面是JavaScript的代码。

```javascript
{{% codesnap "/static/javascript/fact.js" %}}
```


```css
{{% codesnap "/static/javascript/fact-style.css" %}}
```

## JavaScript中危险的`eval`

下面我们设置一个对话框，输入JavaScript语句，将运行并将结果显示下面。

{{% htmlfile "/static/javascript/eval.html" %}}

对应的代码为：


```html    
{{% codesnap "/static/javascript/eval.html" %}}
```