+++
title = 'Katex vs. Mathjax in Hugo中数学公式的显示'
date = 2025-02-26T10:20:38+08:00
draft = false
mathkatex = true
categories = ['hugo']
tags = ['hugo', 'katex', 'mathjax']
toc = false
tocBorder = true
+++

在Hugo中的数学公式可以用两种方式显示：Katex和Mathjax。

Katex的速度比Mathjax快，但是Mathjax的公式显示效果比Katex好。

在Hugo中，可以很容易的设置数学公式的显示方式。

[mathjax+Hugo的配置](/posts/hugo/hugo-mathjax)中有详细的配置方法。

只需要再建一个`mathkatex.html`在同样的位置。

```html
<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.16.21/dist/katex.min.css"
    integrity="sha384-zh0CIslj+VczCZtlzBcjt5ppRcsAmDnRem7ESsYwWwg3m/OaJ2l4x7YBZl9Kxxib" crossorigin="anonymous">
<script defer src="https://cdn.jsdelivr.net/npm/katex@0.16.21/dist/katex.min.js"
    integrity="sha384-Rma6DA2IPUwhNxmrB/7S3Tno0YY7sFu9WSYMCuulLhIqYSGZ2gKCJWIqhBWqMQfh" crossorigin="anonymous">
    </script>
<script defer src="https://cdn.jsdelivr.net/npm/katex@0.16.21/dist/contrib/auto-render.min.js"
    integrity="sha384-hCXGrW6PitJEwbkoStFjeJxv+fSOOQKOPbJxSfM6G5sWZjAyWhXiTIIAmQqnlLlh" crossorigin="anonymous"
    onload="renderMathInElement(document.body);">
    </script>
<script>
    document.addEventListener("DOMContentLoaded", function () {
        renderMathInElement(document.body, {
            delimiters: [
                { left: '\\[', right: '\\]', display: true },   // block
                { left: '$$', right: '$$', display: true },     // block
                { left: '\\(', right: '\\)', display: false },  // inline
                { left: '$', right: '$', display: false },     // inline
            ],
            throwOnError: false
        });
    });
</script>
```

然后在Hugo页面的`head`标签中增加：

```html
  {{ if .Params.mathjax }}
  {{ partial "mathjax.html" . }}
  {{ else if or .Params.mathkatex .Site.Params.mathkatex }}
  {{ partial "mathkatex.html" . }}
  {{ end }}
```
这段话的逻辑是：如果在当前页面中设置`mathkatex = true`，则使用Mathjax，否则如果当前页面中设置`mathkatex = true`，或者Hugo的配置中设置`mathkatex = true`，则使用Katex。

当然，我们可以提前在`hugo.toml`中增加如下参数，对应`.Site.Params.mathkatex`。

```toml
[params]
  mathkatex = true
```

然后，除非页面中明确指定`mathkatex = true`，否则默认使用Katex。







