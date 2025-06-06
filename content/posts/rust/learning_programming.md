+++
title = 'Learning_programming学习编程的难点和解决方法'
date = 2025-06-04T07:37:54+08:00
draft = false
mathkatex = true
categories = ['rust', 'programming']
tags = ['rust', 'programming', 'learning', 'complexity', 'software development']
toc = true
tocBorder = true
+++

## 学习编程为什么难？

### 软件构建的复杂性

软件的构建在上个世纪逐步干掉硬件成为主要的驱动引擎，这个世纪更是成为了显学中的显学，但是最近AI的兴起，似乎又让现在入软件的好像是49年入国军。

编程，就作为软件构建的一般称呼，成为毕业就要到来的同学们的心痛和纠结。入，还是不入，这是一个问题。

开发计算机软件其实是一个非常复杂的活动，虽然最聪明的科学家和工程师努力构造一层一层的抽象，试图让软件开发（编程）的过程变得更加可行。我这里都不敢说简单，只是说可行，因为还是很复杂，还是很麻烦，只是勉强能够进行。哪怕是在AI的帮助下，阅读代码也不见得符合人类的本性，因为代码的实际执行过程，除了人类最喜欢的线性结构外，还包括分支、循环，还包括最为恶心的并发、异步，还包括各种异常、错误处理。

过去的五十年，关于软件开发的体系架构已经积累了大量的知识，就比如《Code Complete 2》（代码大全2）这样的大部头，中文有一百万字，快1000页。而且，编程是一个实践活动，学了知识还不够。看书的时候小霸王碾压严白虎；上手的时候哪怕就写个Hello world也会愕然发现自己原来本体是个弟弟，孙十万大战张文远……

编程的过程，从定义问题到解决问题，需要经历：

1. 理解问题
2. 设计解决方案
3. 编写代码
4. 测试和调试
5. 优化和维护

这里面的难度其实主要在于理解问题和设计解决方案，而编写代码的过程其实大学本科生稍微培训一下就能入职的。但是理解问题是最难的，这还没有考虑发现问题、提出问题。通常软件开发的甲方会提供一个含糊而笼统的需求，然后就等着乙方给出解决问题的方案。而软件构建的工作，就从帮助甲方理解问题开始……这个过程的价格，通常比编写代码要高很多很多。

这就是为什么看到Rust重写这个、重写哪个，为什么？这一下子就干掉了最难的部分：定义问题、理解问题。那还不是随便写？（并不是）在问题之后就是设计。

而设计一个软件解决方案，其实是包含了编写代码、测试和调试、优化和维护的闭环。评价一份解决方案的优劣，主要就是对问题的解决程度，其次就是编写代码、测试和调试、优化和维护等工作的代价。而且，软件系统的投资，有很多就是后一个部分：

- 能否在给定的时间完成编写代码？能否以很便宜的价格找到人来完成编写代码？
- 测试和调试的过程是否可控？
- 优化和维护的代价是否可以接受？

虽然，有大量的软件因为这三个部分而遭到失败，但是大部分软件工作还是到1、2、3就交付结束了。如果说学习编程，也有1$\sim$5的学习方法，和1$\sim$3的学习方法。

全部闭环的学习方法，更加麻烦，一开始就要折腾一个很复杂的框架，学了很久都学不到编写代码本身。这样的方法就是《Code Complete 2》/《代码大全2》中描述的。另外一种，很典型的学习编程的语境，就是那本薄薄的《C程序设计语言》，非常简单，上来就是Hello world，然后就是各种语法构造（其实也没多少）。

### 难度三层次

总而言之，学习编程的三个层次对应了学习编程的三个难度。

- 具体程序设计语言的语法
- 解决具体而良好定义问题的数据结构和算法
- 解决复杂软件系统构造的体系架构和质量控制方法

第一层难度，就是学习编程语言的语法。这个过程，其实和学习英语的语法差不多，就是熟悉、输入、输出。这里的难度比较小，但是需要大量的练习。没有练习，跟我们以前一样搞纸上编程，是要反复补课的。这个部分的学习能够控制在很短的时间中完成。

第二层难度，就是解决具体而良好定义问题的数据结构和算法。这个过程，根据从事工作的不同，差异很大。面向计算机的数据结构和算法开发，和面向任务的数据结构和算法应用，差异很大。第一个部分那是最聪明的人的领域，我就不瞎说了。而现代的编程语言通常会包括第二部分（面向任务的数据结构和算法），这个部分通常会让同一个编程语言在两种人的口里有两种不同的评价。就比如，C语言，有些人说C语言好简单：没错，C语言的内置数据结构和算法，的确很简单；但是C语言无所不能，学C语言学了很久还是这也不会那也不会，挫折感倍增，很多C语言老鸟，还是会被数据结构、算法要解决的安全性问题折磨……。与此相对应的就是Rust，Rust的语法比C语言复杂太多，但是Rust的实际应用，有时候又出奇的简单。这是因为，Rust内置了大量的数据结构、算法（例如生命周期），这些数据结构和算法实际上解决了变成中的大量通用问题（内存安全、并发安全、错误处理），所以，Rust的编程和C语言有很大的不同。说一个很Lisp的话，C语言需要自己写一个简易版的生命周期管理系统，然后再实现实际的解决方案。而C++
所实现的面向对象的构造和析构也是一种处理同一问题的数据结构和算法。

第三层难度，就是解决复杂软件系统构造的体系架构和质量控制方法。目前，我看也不需要仔细描述。因为一般的编程问题，集中在第一层难度和第二层难度。特别是现代化的程序设计语言中，第一层和第二层其实都是集中在一起的，例如Python的内置数据结构和GC算法，就解决了大量的C来完成软件编制所需解决的额外基础问题。

## 解决的方向

### 知其所止，可以人而不如鸟乎

诗云：缗蛮黄鸟，止于丘隅。编程学习的过程，其实最重要的是长时间的坚持，这是人类最大的挑战。跟AI不同，人类通常不能只根据理智来行动，长时间的坚持需要定期的情感激励。态度决定学习的效果。

所以这个问题一定要想清楚，为什么学习编程？

为了找工作、为了大房子、为了女朋友、为了改变世界？必须是真正认可的，必须跟自己的兴趣、爱好、特长相结合的。我个人C++真正有点进步是因为死期将至，课题要验收，所以才逼着自己硬写了若干勉强通过C++编译器代码，并完成了课题。

真实才有力量；虚假的目标不能长久。这一点我看好多年轻人不太理解：嘴里说重要，嘴里说要学，但是你心里其实并不认可，所以学着学着就放弃了。

### 君子务本，本立道生

编程的本是什么？数据+算法=程序。我感觉这个等式还是没有过时。

就比如说，你要是把Rust中的生命周期标注这些理解为一种数据结构和算法，就很自然；就比如说所有权，其实也是一种数据结构和算法。

那么生命周期标注要解决什么问题呢？

生命周期标注要解决的问题是：

- 在数据的有效期内之外，数据不能被使用
- 在数据的有效期内，数据不能被释放

描述数据的有效期信息，就是生命周期标注；编译器根据生命周期标注，检查数据的有效期，并给出警告。学习Rust生命周期，就要把自己当做编译器的生命周期检查算法，这样就很自然。

### 学而时习之，不亦说乎

学习编程，其实是一个不断练习的过程。Rust中有一个很棒的练习，就是[Rustlings](https://github.com/rust-lang/rustlings)。这个练习，就是通过很小的练习，帮助你熟悉Rust的语法和语义。

此外，Rust提供了一个[Rust by Example](https://doc.rust-lang.org/stable/rust-by-example/)，这个网站，就是通过大量的例子，帮助进一步掌握Rust的语法和语义。

最好的就是，Rust社区的确在努力提供大量的材料。

最近看的一本[C++ Rust Rephrase](https://cel.cs.brown.edu/crp/)，就是通过C++的代码，帮助你理解Rust的语法和语义和习惯用法。

### 温故而知新，可以为师矣

这句话看起来重点是在前面，我却经常反过来理解。

我主要靠写帖子假装教人编程来自己学编程。

我建议你也常常写帖子，这样可以帮助你更好地理解问题、发现新问题。

## 期待

真希望AI能够更厉害，跟我们进行更加深入和有意义的交互，彻底解决编程的问题。就我个人来看，AI应该更加擅长代码大全2中所描述的机构和体系啊。
