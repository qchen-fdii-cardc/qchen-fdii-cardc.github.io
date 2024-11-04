+++
title = '前进的方向，Lisp接下来写什么？'
date = 2024-11-03T20:46:20+08:00
draft = false
mathjax = false
categories = ['lisp']
tags = ['lisp']
toc = false
tocBorder = false
+++


有一阵子没有写任何关于Lisp的东西。

为什么？

感觉没有什么值得写的。

反而是把前面写的一个[explore-lisp](https://sun-wukong.cn)折腾好几次。增加了`lookfor`函数，是不是在增加一个`help`函数呢？感觉已经有点离不开了……所以我都准备把这个加到我的`~/.sbclrc`里面去了。没事就`lookfor`一下，`describe`一下，跟我在Matlab里面的操作差不多。

在看Common Lisp the Language，也感觉恨不得劲，简单的东西没看出来什么，复杂的东西其实也很简单。Lisp简直是太透明，只要一层一层剥开，从头到脚都是清楚的。

这样就回到了这个系列到底在写什么？为什么写？为谁写？

最初当然是为我自己。记录一下探索的过程，也做一个颅外记忆。

也在知乎跟人讨论这个事情，知乎有兄弟推荐给我很多问题，都是学术性比较强的，我兴趣不是太大，对PL的理论研究目前也不太在我的射程之内，毕竟，我也多少算个甲方老爷、领域专家之类。

其实大家都认可，在中文社区，几乎没有多少有价值的Lisp资料。虽然我本身看英文没有多大障碍，但是看到居然CSDN的Lisp博文看的人、收藏的人那么多，我也有点感触，也可能慢慢会有一点点别的想法。

但毕竟这个想法不是那么强烈，也不太像是我内心真正的冲动，再说我这个年龄，还有什么冲动啊，可能、大概都是为了炫耀……跟钓鱼佬钓到大鱼就容易迷路是一个道理。

那么下一步，往哪里？实际上，我的小工具语言有点过于充足，给别人写小工具都是看别人方便；给自己写小工具都是看梯子在哪里。

对Lisp我有一种很强的感觉，就是对于`Common Lisp`实际上的掌握还有很远。

- 是否有必要好好掌握`Common Lisp`提供的工具？
- 还是应该专注于掌握从头构建一门方言（more or less）的能力？
- 或者应该对PL的某些有趣概念进行探索？并考察在实际CL中的实现？

从领域、甲方的角度看，我认为一个PL要在工程上有所应用，其本身提供的工具应该是非常非常非常（此处重复几百次）重要的。

但是对于Lisp，也可能像是Racket一样，应该放弃工程、应用这些俗气的东西，致力于虚无缥缈、也更加有趣的东西？



最近看过的Blogs：
- [Planet Lisp](https://planet.lisp.org/)
- [Steve Losh](https://stevelosh.com/blog/)
- [Dan's Musings](https://blog.djhaskin.com/)
- [Fernando Borretti](https://borretti.me/)
- [Lisp Journey](https://lisp-journey.gitlab.io/)