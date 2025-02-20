+++
title = '003 Canvas Drawing一个小小的白板'
date = 2025-02-20T16:30:52+08:00
draft = true
mathjax = false
categories = ['javascript']
tags = ['javascript', 'canvas', 'drawing', 'whiteboard']
toc = false
tocBorder = true
+++


{{% htmlfile "/static/javascript/canvas-drawing/draw.html" %}}
## 原因

这几天我需要给别人发一个签名的图片，自己在画图里挣了半天，不好看……

突然记起（不记得什么时候）看到有人发了一个他的白板小工具，做得非常不错，非常好用。

那就不纠结，趁着死线还有若干小时，抽出半小时，自己写一个。

## 使用指南

就只提供了几个简单的功能：

- 清屏
- 颜色
- 画刷尺寸
- 导出

这几个功能完全满足了我签个名字，导出一个图片，发给别人的需求。

## 代码

三个文件下载：

- [HTML](/javascript/canvas-drawing/draw.html)
- [CSS](/javascript/canvas-drawing/style.css)
- [JavaScript](/javascript/canvas-drawing/draw.js)

文件内容如下所示。

### draw.html

```html
{{% codesnap "/static/javascript/canvas-drawing/draw.html" %}}
```

### style.css

```css
{{% codesnap "/static/javascript/canvas-drawing/style.css" %}}
```

### draw.js

```javascript
{{% codesnap "/static/javascript/canvas-drawing/draw.js" %}}
```
