+++
title = '001 Try It with JavaScript in Hugo中增加一点JavaScript'
date = 2024-11-23T14:20:15+08:00
draft = false
mathjax = false
categories = ['javascript']
tags = ['javascript', 'html', 'css', 'tutorial', 'hugo', 'include-file']
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

其实，别的不说，就JavaScript这个运行速度，简直是盖了……这还不是尾递归，速度快的一塌糊涂。
好吧，技术总是发展得如此之快，麻瓜们只能瞠目结舌不知所措。


对应的[HTML代码](/javascript/fact.html)为：

```html    
{{% codesnap "/static/javascript/fact.html" %}}
```

这里我引用了jQuery，我唯一知道的JavaScript基础设施……不知道是不是有替代的更好的，在我找到之前，就用这个来操纵我的HTML吧。


对应的JavaScript代码放在[单独的文件](/javascript.fact.js)中，下面是JavaScript的代码。

```javascript
{{% codesnap "/static/javascript/fact.js" %}}
```

首先是定义阶乘函数，最无聊的一种定义方式，递归调用，终止条件`0!=1`。然后是更新文字，利用jQuery来操纵，设置文本，并添加历史记录。

通过在历史记录中按照`id`设置为对应数字来保持单一性。最后是给输入框的回车和失去焦点添加回调函数。jQuery是那么性感。


不要忘了还有一个[CSS文件](/javascript/fact-style.css)，内容如下，不要嘲笑领域专家的CSS技能……我们基本上刷成灰的就会说好看，刷成绿的就说难看。

```css
{{% codesnap "/static/javascript/fact-style.css" %}}
```

真看起来真的还行，在Hugo里面也能做一个JavaScript的App……下面再测试一下另外一个东西。

## JavaScript中危险的`eval`

下面我们设置一个对话框，输入JavaScript语句，将运行并将结果显示下面。

{{% htmlfile "/static/javascript/eval.html" %}}

对应的代码为：

```html    
{{% codesnap "/static/javascript/eval.html" %}}
```

看起来这个也没有问题。

## Hugo中导入HTML、JS和CSS的美好之处

实际上，这里面最好的就是，所有的代码，可以直接写成HTML文件、JS文件和CSS文件，在这个帖子里中，我可以直接引用文件内容。

如果是插入HTML文件内容本身，我定义了一个Hugo宏，`htmlfile.html`，放在`/laytouts/shortcodes`目录下，

```hugo
{{ .Get 0 | readFile | safeHTML }}
```

只要在Github Repository的`hugo.toml`中增加一个选项：

```
[markup]
  [markup.goldmark]
    [markup.goldmark.renderer]
      unsafe = true
```

在Markdown文件中增加HTML文件的原始内容，就是记得这里要写文件相对于Hugo站点根目录的全路径：

![](/javascript/include-raw-html.png)

这个部分就直接成为博客的一部分，这也太完美了吧。

然后，要展示源代码，就只需要：

![](/javascript/include-code-snap.png)

这两个宏命令，其实都是一样的，我专门做成两个，就是为了区别功能，当然，`codesnap`还可以引用别的语言的代码。


还是有点完美的……我简直是个天才。

## 下一步

这样的话，可以玩的就太多了……