+++
title = '002 懒惰先生的Lisp开发流程'
date = 2024-08-05T10:28:00+08:00
draft = flse
mathjax = false
categories = ['lisp', 'programming-language']
tags = ['lisp', 'quickproject', 'project', 'template']
toc = true
tocBorder = true
+++



## 简介
比较现代的语言，跟C/C++比起来通常有一个需要反复炫耀的特性，包管理工具，这个工具可以很方便地管理工程项目的依赖关系，通常会由几个部分组成：

1. 一个软件工程的管理工具，处理依赖关系，构建，测试，发布，现代的软件开发还可能包含了编译工具链的管理；
2. 一个记录软件包（生态）的registry，通常提供访问和下载依赖包的功能；
3. 实际存储软件包的地方，有些软件是直接从源代码构建的，有些是提供预编译的二进制包，还有一些是混合的。

对于Java，Maven是一个很好的例子，它提供了一个标准的项目结构，一个中央的registry，以及一个标准的构建工具，Gradle也是一个类似的工具。

对于Python，pip是一个很好的例子，它提供了一个标准的项目结构，一个中央的registry，以及一个标准的构建工具。

对于C#、F#，.NET Core提供了一个标准的项目结构，一个中央的registry，以及一个标准的构建工具。

良好的生态是现代语言成功的重要因素。跟这个比较，C/C++可以理解为，软件管理工具、软件包（库）、编译器，在Linux系统中跟系统紧密耦合，采用apt、yum等工具提供；在windows中，这些功能由IDE提供一部分，但是整个的库文件管理、包管理是比较混乱的状态。但是因为Linux对于C/C++的支持比较好，所以C/C++的生态实际上也是能够得到保障的（虽然诟病的很多）。工具包的不同版本的混乱，都是因为跟操作系统紧密耦合造成的。

基本上，比较流行的前几个语言，都有实际上的依赖管理工具。这个功能如果没有保障，必然会成为软件开发人员使用的阻碍。而如果一个语言得到程序员的爱，也必然会有程序员来维护相应的工具链。

那么老古董Lisp呢？这个完全不纯洁、极度自由的语言，有没有什么工具可以用来管理依赖呢？

Lisp标准的包管理工具是ASDF，它是一个构建工具，可以用来管理Lisp项目的依赖关系。在ASDF之上，Lisp还有一个叫Quicklisp的工具，提供实际的软件包下载和管理功能。而实际上，在ASDF上，还有一个叫`quickproject`的工具，用来更加方便的创建一个Lisp项目。

- ASDF：构建工具
- Quicklisp：软件包管理工具
- Quickproject：项目管理工具

这种分层的设计，是Lisp中的典型特征，每个工具都有自己的功能，但是又可以很好的配合使用。大牛们的说法是：Lisp软件（称为系统）是自然而然长出来的。也就是，Lisp软件是自下而上开发的。

那么我们分别介绍下几个工具中的核心概念。并用一个例子来展示如何使用这些工具（来开发一个系统）。

`Lisp`作为一个老古董，居然有这么好用的工具，这是懒惰先生的福音。懒惰先生很懒惰，所以他必须好好看各种工具的帮助文档，然后才能挪动他的胖手指。

- [ASDF](https://common-lisp.net/project/asdf/asdf.html)
- [Quicklisp](https://www.quicklisp.org/beta/)
- [Quickproject](https://www.xach.com/lisp/quickproject/)

## ASDF的关键核心概念
其实，在Linux系统中有一个asdf工具，用来切换不同的软件包版本，这个跟Lisp的ASDF没有关系。Lisp的ASDF是Another System Definition Facility的缩写，是一个构建工具，用来管理Lisp项目的依赖关系。ASDF的核心概念有：

- 系统：一个系统是一个逻辑上的单元，包含了一组文件，通常是一个库。系统的定义在一个.asd文件中，这个文件包含了系统的名字，依赖关系，编译选项等。
  - 系统的公开接口：系统的接口是系统的一部分，包含了系统的导出函数和变量，其他系统可以通过这个接口来访问系统的功能。
  - 系统的依赖关系：一个系统可以依赖于其他系统，这个依赖关系可以在.asd文件中定义。
  - 系统的实现：记录在.asd文件中，包含了系统的源代码文件，编译选项等。
    - 组件：子系统或者文件，一个系统可以包含多个组件，组件之间可以有依赖关系。
    - 组件的顺序和位置

对于用户而言，ASDF的核心操作是载入一个系统，这个操作会自动载入系统的依赖关系，编译系统的源代码，载入系统的接口。载入一个系统的操作是`(asdf:load-system :system-name)`。

对于开发Lisp应用软件的开发者而言，ASDF的核心操作是定义一个系统，这个操作是在一个.asd文件中定义一个系统，包含了系统的名字，依赖关系，编译选项等。

对于扩展ASDF的开发者而言，ASDF提供了扩展系统管理的接口，例如`quicklisp`就是一个扩展ASDF的工具，提供了软件包的下载和管理功能，每一个软件包都是一个`ASDF`系统。而`quickproject`是另一个扩展ASDF的工具，提供了更加方便的创建一个Lisp项目的功能。实际上，为了实现`ASDF`的功能，`ASDF`提供了一个通用的操作系统接口`UIOP`，这个接口提供了文件操作，路径操作，编译操作等功能。


## 另外两个工具人哦不库

Quicklisp是一个软件包管理工具，提供了软件包的下载和管理功能。通常，我们不需要直接安装ASDF，而是安装Quicklisp，Quicklisp会自动安装ASDF。针对于只是想使用别人的库的用户来说，我们一般就停留在Quicklisp这个层次。比如，[粗鲁先生Lisp再出发](https://blog.csdn.net/withstand/article/details/140528782)中，已经简单粗暴地讲解了如何安装Quicklisp。Quicklisp的核心命令包括：

- `(ql:quickload :system-name)`：安装一个软件包，这个操作会自动下载软件包，载入软件包的依赖关系，编译软件包的源代码，载入软件包的接口。
- `(ql:system-apropos "system-name")`：查找一个软件包，这个操作会列出所有包含system-name的软件包。
- `(ql:system-list)`：列出所有已经安装的软件包。
- `(require :system-name)`：这个命令虽然不是Quicklisp的命令，但是安装后，我们通常用这个命令来载入一个软件包。

而Quickproject是一个项目管理工具，他只对于库的开发者有意义。Quickproject提供了一个快速创建一个Lisp项目的功能，这个功能包括了创建一个标准的项目结构，创建一个.asd文件，创建一个README文件，创建必要的两个lisp文件等。对于懒惰先生，只要某个软件提供自动功能，那就必须撸起袖子来学一下。

对于QuickProject的安装，那就正好使用Quicklisp来安装。Quickproject的核心是提供一个`quickproject:make-project`函数，这个函数会创建一个标准的Lisp项目结构。


## 就决定是你了，Quickproject
懒惰先生懒洋洋的甩了甩鼠标，打开一个终端，从下面的代码片段中，拷贝三个命令，粘贴到终端中，然后按下回车键。


```lisp
(ql:quickload :quickproject)  ;; 安装Quickproject

(require 'quickproject)       ;; 载入Quickproject

(quickproject:make-project "helloworld" :depends-on '(:uiop :cl) :author "Dafu Chen <qchen2015@hotmail.com>" :license "MIT")
```

就这样，一个名为`helloworld`的Lisp项目就创建好了。这个项目的目录结构如下：

```shell
helloworld/
├── helloworld.asd
|── helloworld.lisp
|── package.lisp
├── README.md
```

其中，`helloworld.asd`是项目的定义文件，`helloworld.lisp`是项目的主文件，`package.lisp`是项目的包文件，`README.md`是项目的说明文件。

```lisp
;;;; helloworld.asd

(asdf:defsystem #:helloworld
  :description "Describe helloworld here"
  :author "Dafu Chen <qchen2015@hotmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:uiop #:cl)
  :components ((:file "package")
               (:file "helloworld")))
```

也i可很清楚的看到提供的参数` "helloworld" :depends-on '(:uiop :cl) :author "Dafu Chen <qchen2015@hotmail.com>" :license "MIT"`被用在哪里。

```lisp
;;;; helloworld.lisp

(in-package #:helloworld)

(defun hello (&optional (name "World"))
  (format t "Hello, ~A!~%" name))
```

这里，使用了`in-package`来**打开**了一个包，然后定义了一个函数`hello`，这个函数可以接受一个参数，然后输出一个字符串。

```lisp
;;;; package.lisp

(defpackage #:helloworld
  (:use #:cl)
  (:export #:hello))
```

这里，懒惰先生拷贝了一个`(:export #:hello)`，这个表示`hello`函数是公开的，其他系统可以通过这个函数来访问`helloworld`系统的功能。

```markdown
# helloworld
### _Dafu Chen <qchen2015@hotmail.com>_

This is a project to do ... something.

## License

MIT
```

这个是一个标准的README文件，包含了项目的名字，作者，许可证等信息。

虽然懒惰先生所有的代码都是拷贝的，但是他还是很高兴，因为他的项目已经创建好了。全文完！（并没有）


## 下面就是Helloworld工具包的使用！

粗鲁先生看到懒惰先生马上就要睡着了，粗鲁地踢了一下他的凳子。

这给了懒惰先生一点点力气，他奋起全力，点开vscode，新建`hello.lisp`文件，然后输入下面的代码：

```lisp
(ql:quickload :helloworld)  ;; 当然会报错，什么找不到系统！
                            ;;懒惰先生直接关机睡觉，全文完！（并没有）
```

到这里，找不到系统，这简直太打击人了……懒惰先生不知道怎么办，只好转向粗鲁先生，粗鲁先生说，这个问题很简单，双喜临门，有两个办法：

1. 办法一，把`helloworld`项目的目录拷贝到Quicklisp的本地目录下（粗鲁先生已经演示过`~/quicklisp/local-projects`），然后重新运行Quicklisp安装命令，这样Quicklisp就能找到`helloworld`项目了。
2. 办法二，把`helloworld`项目收录到Quicklisp的软件包中，这样Quicklisp就能找到`helloworld`项目了。这个也很简单，只需要三步：
    1. 把`helloworld`项目添加到大型交友网站github上；
    2. 给[Quicklisp](https://github.com/quicklisp/)写一个issue，请求收录`helloworld`项目；
    3. 等待Quicklisp的管理员收录`helloworld`项目，Quicklisp收录项目周期很长……反正懒惰先生也不在乎，就慢慢等吧……（全文完）
3. 办法三（两个办法当然会有第三，这是国际惯例众所周知）：把`helloworld`项目的目录添加到`asdf:*central-registry*`变量中，这样ASDF就能找到`helloworld`项目了。

当然，五杰的第六人是最强的。懒惰先生又拷贝了一些代码到`hello.lisp`文件中：

```lisp
(require 'uiop)

(push (uiop/pathname:merge-pathnames* "./helloworld/")
      asdf:*central-registry*)

(ql:quickload :helloworld)

(require 'helloworld)
```
果然显示安装成功，懒惰先生一高兴又拷贝了一些代码到`hello.lisp`文件中：

```lisp
(helloworld:hello)
;; => "Hello, World!"

(helloworld:hello "Dafu")
;; => "Hello, Dafu!"
```
好了好了，懒惰先生今天干的活实在是太多了，他决定去睡觉了，全文完！

## 结论

1. Lisp的包管理工具是ASDF，提供了系统的定义和载入功能。
2. Quicklisp是一个软件包管理工具，提供了软件包的下载和管理功能。
3. Quickproject是一个项目管理工具，提供了快速创建一个Lisp项目的功能。
4. 懒惰先生可以使用Quickproject来创建一个Lisp项目，然后使用Quicklisp来安装这个项目。
5. 懒惰先生可以使用ASDF的`*central-registry*`变量来添加一个项目的目录，这样ASDF就能找到这个项目。