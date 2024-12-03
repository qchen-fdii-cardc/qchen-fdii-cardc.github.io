+++
title = '002 Coolprop小试牛刀：流体属性查询示例'
date = 2024-11-23T16:06:55+08:00
draft = false
mathjax = false
categories = ['javascript']
tags = ['javascript', 'CoolProp', 'example']
toc = false
tocBorder = true
+++


{{% htmlfile "/static/javascript/coolprop-first-example.html" %}}


## Coolprop工具箱

这是一个物质属性的热力学工具箱。
本身提供了很多种高级语言的绑定，当然，JavaScript也是提供的。

这里实现了一个简单的单纯流体的一般参数查询。还没有准备更加复杂的东西。总的来说，这只是一个用官方的JavaScript绑定代码改造的Smoke Test。

- [Coolprop.js](/javascript/coolprop.js)
- [Coolprop.wasm](/javascript/coolprop.wasm)
- [Coolprop JavaScipt wrapper](http://www.coolprop.org/coolprop/wrappers/Javascript/index.html)

## HTML源程序

- [HTML源文件](/javascript/coolprop-first-example.html)

这个就非常简单，几个选择框，两个输入框，一个计算按钮。

```html
{{% codesnap "/static/javascript/coolprop-first-example.html" %}}
```
## JavaScript文件


- [JavaScript源文件](/javascript/coolprop-first.js)

计算的部分也很简单，唯一个就是CoolProp的JavaScript绑定通过WASM实现，载入需要时间。
所以，这里在`document.ready`事件里面还设置了一个`setTimeout`，1s之后在设置默认值，载入流体列表。
手快的小朋友也必须等等。

```javascript
{{% codesnap "/static/javascript/coolprop-first.js" %}}
```

