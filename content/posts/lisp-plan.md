+++
title = '000 中年人与Lisp'
date = 2024-07-23T09:47:44+08:00
draft = false
categories = ['lisp', 'plan']
tags = ['lisp', 'plan', 'life']
mathjax = false
+++


## 中年人与Lisp
Lisp实际上已经是我的老朋友。

大学的时候就开始看Lisp，很有意思也很有力的7个构成部分形成整个LISP大厦，非常震撼。

断断续续看了很多Lisp的书和帖子。这次又准备再来一次，经过20年之后，我的能力和认知肯定已经发生巨大变化。说不定，这一次，有好玩的。

## 计划

1. 随意地写几篇找找感觉；
2. 感觉好的话，再写几篇；
3. 等写10篇再考虑是不是做一个规划。

## 文章列表

1. [001 粗鲁先生Lisp再出发](/posts/001-rude-start-application/)
2. [002 懒惰先生的Lisp开发流程](/posts/002-lazy-process/)
3. [003 颠倒先生的数学表达式](/posts/003-lazy-process/)
4. [004 完美先生的完美Lisp](/posts/004-perfect/)
5. [005 好奇先生用Lisp来探索Lisp](/posts/005-explore-lisp/)
6. [006 好奇先生在Lisp的花园里挖呀挖呀挖](/posts/006-sequence-in-lisp/)
7. [007 挑剔先生给出终止迭代的条件](/posts/007-recursive-eq/)
8. [008 挠痒痒先生建网站记](/posts/008-real-app/)
9. [009 小小先生学习Lisp表达式](/posts/009-expression/)
10. [010 聪明先生拒(ji)绝(xu)造轮子](/posts/010-smart-cl-classification/)
11. [010 Common Lisp Symbols分类参考](/posts/010-appendix-cl-symbols/)
12. [011 没人先生学习Lisp函数](/posts/011-functions.md)
13. [012 白日梦先生的白日梦](/posts/012-1am-tutor.md)

## 计划

可能是时候做一个计划。

目前已经完成的部分：

```mermaid
graph LR

A[001 粗鲁先生Lisp再出发]
B[002 懒惰先生的Lisp开发流程]
C[003 颠倒先生的数学表达式]
D[004 完美先生的完美Lisp]
E[005 好奇先生用Lisp来探索Lisp]
F[006 好奇先生在Lisp的花园里挖呀挖呀挖]
G[007 挑剔先生给出终止迭代的条件]
H[008 挠痒痒先生建网站记]
I[009 小小先生学习Lisp表达式]
J[010 聪明先生继续造轮子]
K[011 没人先生学习Lisp函数]


To(开发工具) --> A
To --> B
To --> C

Basic(基础) --> D
Basic --> G
Basic --> I
Basic --> K

CL(常用库) --> E
CL --> F
CL --> H
CL --> J

CommonLispTutorial(教程) --> To
CommonLispTutorial --> Basic
CommonLispTutorial --> CL

```

还应该再写什么呢？写的内容是聚焦到实际应用？还是聚焦到理念、理论和PL？

还需要再想一想。

是不是照着一本书来Copy结构？还是围绕一个想要做出来的应用展开？

### 目标是什么？

要不要再一次来思考，我的目标是什么？

最近刚看了[Lisp Curse](https://www.winestockwebdesign.com/Essays/Lisp_Curse.html)，感觉很符合这两个月对Lisp的学习。

- 工具零碎，全部都不完整，能够用但是又刚好不够用；
- 如果很不舒服，就自己写一个，也不是太难的样子；
- 用别人的，也要挑挑拣拣，而且，文档是几乎没有的，就比如Common Lisp自己的工具，都没有一个非常好用、完备的文档，跟[Racket](https://docs.racket-lang.org/)比起来，差距太大了；


就比如说画图，[vgplot](https://github.com/volkers/vgplot)，[文档](https://volkers.github.io/vgplot/vgplot.html)， 其实说起来还算是不错，精心制作了`(vgplot:demo)`，照葫芦画瓢也简单。当然，如果有GNUPlot的基础就更是毫无压力。如果仔细考虑，这个包的功能也符合上面的特点，勉强够用。


就比比如说[quicklisp](https://www.quicklisp.org/beta/releases.html)，下载量排名第一的[Alexandria](https://alexandria.common-lisp.dev/)，有文档，但是不多，没有中文，实际上，他的文档就是跟[explore-lisp](https://github.com/qchen-fdii-cardc/explore-lisp.git)差不多，用脚本生成的，但是这已经算是很好的了。

至于中文的，那就更可怜。

所以，我的目标是什么呢？

向工程需要，或者实际解决问题需要的`Common Lisp`入门指南。我本人没有能力，也不准备探讨那些非常复杂、难以理解的PL理论，也不准备仔细辨别`Common Lisp`中微妙的语法构造。我只想给一个快速上手、能够使用的梯子，通过这个梯子，Lisp可以成为工具箱的一把扳手，而不是叶公好龙、充作谈资的花瓶。

所以，我要以我自己的需要为出发点，**Make Lisp Great Again**。

> Make Lisp Great Again, for Engineers.

### 目标读者

