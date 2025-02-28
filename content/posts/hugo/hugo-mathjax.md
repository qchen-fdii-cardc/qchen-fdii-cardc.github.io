+++
title = 'Hugo+Mathjax的正确姿势'
date = 2025-02-22T12:48:39+08:00
draft = false
mathkatex = true
categories = ['hugo']
tags = ['hugo', 'mathjax', 'markdown', 'hugo数学公式']
toc = true
tocBorder = true
+++

## 游戏名称：hugo

把手头的无聊域名用起来之后，就用Hugo搭了一个博客。

- [写作练习](https://www.windtunnel.cn)

感觉整个的使用体验还是很不错很不错的。

### 安装

安装Hugo的时候，一定要记得是安装hugo-extended版本，好多功能需要这个版本。

这个版本的安装地址：

```bash
export HUGO_VERSION=0.144.2
wget -o hugo_extended_${HUGO_VERSION}_Windows-amd64.zip https://github.com/gohugoio/hugo/releases/download/v${HUGO_VERSION}/hugo_extended_${HUGO_VERSION}_Windows-amd64.zip
```
如果是wsl（最好的Linux发行版），可以这样安装：

```bash
export HUGO_VERSION=0.144.2
wget -o hugo_extended_${HUGO_VERSION}_Linux-amd64.deb https://github.com/gohugoio/hugo/releases/download/v${HUGO_VERSION}/hugo_extended_${HUGO_VERSION}_Linux-amd64.deb
sudo dpkg -i hugo_extended_${HUGO_VERSION}_Linux-64bit.deb
```


### 建立站点

wsl下面安装好就可以访问`hugo`命令了。在windows下面，需要把解压后的`hugo.exe`文件移动到某个通过`PATH`环境变量可以访问的目录下。

之后就可以通过`hugo`命令，测试是否安装成功，

```bash
hugo version
```

如果成功安装，会显示hugo的版本号。

然后就可以用`hugo new site`命令，创建一个新站点。

```bash
hugo new site MyBlog
```

套用一个模板，入门的一般是`ananke`，

```bash
hugo new site MyBlog --template=ananke
```

或者

```bash
git init
git submodule add https://github.com/theNewDynamic/gohugo-theme-ananke.git themes/ananke
```

### 写文章

更新文章的方式

```bash
hugo new posts/hugo/hugo-mathjax.md
```

### 本地运行

```bash
hugo server -D
```

这里的`-D`表示draft，表示草稿。在新建的文章默认是draft的，所以需要加这个参数。


### 发布

```bash
hugo build
```

此时完整的站点就生成在`public`目录下。可以采用各种方式部署。

当然我们采用GitHub Pages的时候，可以直接只提交`public`目录到`gh-pages`分支。也可以采用Github-Actions自动部署。

### 几个特殊的文件

- `hugo.toml`：站点配置文件
- `CNAME`：用于设置自定义域名
- `.github/workflows/hugo.yml`：Github-Actions自动部署文件

## Hugo支持数学的正确方式

### 测试驱动
下面用Hugo写数学公式为例，看看搞点自己的东西是什么样子的。

我们首先新建一个测试文件：

```bash
hugo new posts/hugo/hugo-mathjax.md
```

这个文件的内容包括：

```markdown
+++
date = '2025-02-22T13:20:35+08:00'
draft = true
title = 'Mathjax'
+++

# Block Math

$$
\int_0^\infty \frac{x^3}{e^x} \, dx = \frac{\pi^4}{15}
$$


\[
\int_0^\infty \frac{x^3}{e^x} \, dx = \frac{\pi^4}{15}
\]

# Inline Math

$\int_0^\infty \frac{x^3}{e^x} \, dx = \frac{\pi^4}{15}$ 是一个行内公式。\(
    \int_0^\infty \frac{x^3}{e^x} \, dx = \frac{\pi^4}{15}
\) 也是一个行内公式。
```

当然，这个相当于是我们用一个TDD的开发模式，先写一个测试案例，然后逐步开发，达到我们想要的效果。这个时候，测试肯定是通不过的。`hugo server -D`会显示下面的页面：

![hugo-mathjax](/hugo/mathjax/test-origin.png)

### 第一步

第一步，整一个部分的Html文件增加到Hugo产生的Html文件中。这个文件我们放在`themes/ananke/layouts/partials/extend-head.html`文件中。

```html
{{ if or .Params.math .Site.Params.math }}
<script>
  MathJax = {
    tex: {
      inlineMath: [['\\(', '\\)']],
      displayMath: [['$$','$$']],
      processEscapes: true,
      processEnvironments: true
    },
    options: {
      skipHtmlTags: ['script', 'noscript', 'style', 'textarea', 'pre']
    }
  };
</script>
<script src="https://polyfill.io/v3/polyfill.min.js?features=es6"></script>
<script id="MathJax-script" async src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"></script>
{{ end }} 
```

还需要在`\themes\ananke\layouts\_default\baseof.html`文件中，增加一个`extend-head.html`文件的引入。

```html
{{ partial "extend-head.html" . }}
```

这个应该是增加在`<head>`标签中。


然后，我们要修改`hugo.toml`文件，增加`math`参数，并开启`mathjax`。
当然，这个math = true/false，可以全站设定，也可以在单个页面中的头部设置。


```toml
# Site parameters
[params]
  #  other params
  math = true  # Enable math support


[markup]
  [markup.goldmark]
    [markup.goldmark.renderer]
      unsafe = true  # This allows HTML in markdown
```
这里有两个部分，一个部分就是要打开`math`参数，另一个部分是打开`unsafe`参数。

有第二个参数，才会允许MathJax处理HTML标签。

完成这一步，我们可以看到，有一个公式被处理了，但是其他的公式没有被处理。


![hugo-mathjax](/hugo/mathjax/test-step1.png)


### 第二步

依然没有效果，我们在仔细看看我们的代码中，是不是没有搞对。

首先是，我们注册的公式格式中，只包含了`$$`和`\()`，修改`extend-head.html`文件，增加`$`和`\`的注册。

```html
      inlineMath: [['\\(', '\\)'], ['$', '$']],
      displayMath: [['$$', '$$'], ['\\[', '\\]']],
```

现在，我们再运行`hugo server -D`，可以看到，用`$`和`$$`包围的公式已经可以正常显示了。

![hugo-mathjax](/hugo/mathjax/test-step2-1.png)


### 第三步

我们仔细观察，发现得到的Html文件中，`()`和`[]`包围的公式没有被处理。

这里还会有一个非常精巧的bug，就是，用`$`和`$$`包围的公式中，某些`_xxxx_`会被处理成`<em>xxxx</em>`，从而造成公式不能正常显示。Hugo的处理流程中，首先会进行Markdown的解析，然后进行HTML的解析。

所以，这里我们应该引入一个条件，来让Hugo解析Markdown的时候，不处理相应的公式。

这一次，我们应该修改`hugo.toml`文件，增加`extensions`参数。

这里首先启用markdown的扩展，然后增加一个passthrough的扩展，来处理我们想要的公式。当碰到区块的分隔符和行内分隔符（在下面定义）时，Hugo跳过不进行处理，从而能够把行间公式和行内公式留给后续的Mathjax，产生正确的公式渲染。

```toml
[markup]
  [markup.tableOfContents]
    endLevel = 2
    startLevel = 2
    ordered = true
  [markup.goldmark]
    [markup.goldmark.renderer]
      unsafe = true
    [markup.goldmark.extensions]
      [markup.goldmark.extensions.passthrough]
        enable = true
        [markup.goldmark.extensions.passthrough.delimiters]
          block = [['\[', '\]'], ['$$', '$$']]
          inline = [['\(', '\)'], ['$', '$']]
```

大概就是这样，经过这一步，我们就可以看到，所有的公式都可以正常显示了。

![hugo-mathjax](/hugo/mathjax/test-step3.png)

这里有一个要记得，定义行间公式时，`$$`、`\[`和`\]`的前后要留一个空行，不然hugo会忽略任何跟行间公式相连（只有一个换行）的文字。

## 总结

1. Hugo的Markdown解析流程中，首先会进行Markdown的解析，然后进行HTML的解析。
2. Hugo程序命令行提供了站点、文章的创建、运行、发布等操作。
3. 这里我们提供了一个没有bug的Mathjax的配置，可以让大家在Hugo中愉快地写数学公式，这个问题有一大堆人在用英语问，但是解决方案却很少。








