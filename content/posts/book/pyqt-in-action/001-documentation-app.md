+++
title = '001 Documentation App'
date = 2024-08-22T18:01:40+08:00
draft = false
mathjax = false
categories = ['python', 'book', 'chapter', 'pyqt-in-action']
tags = ['python', 'book', 'pyqt-in-action', 'pyqt', 'qt.io', 'ux', 'ui', 'design']
toc = true
tocBorder = true
+++

## 界面设计和需求分析

界面设计（UI）或者更加新潮的概念体验设计（UX），实际上对于人类来说是一个很古老的概念。人类开始使用工具就开始不由自主地塑造工具的个人体验，改进工具的有用性、易用性。在这个过程中有一对很有张力的概念始终扮演着核心的元素。

- 个性化
- 标准化

标准化是一个很奢侈的概念，个性化是一个更奢侈的概念。或者反过来说，同样成立。

对于软件来说，标准化的实施是最为简单的，每一份拷贝都可以是一样的，带来同等的体验；对于基于网络服务的软件体系而言，体验更加标准，所有人都通过html+css的窗口体验同一组软件组件提供的计算和信息服务。但是软件设计过程的标准化，又是高度艰难的。软件的需求实际上是一个非常不确定的概念，人类的语言有多么不确定，软件需求就有多么不确定，在ChatGPT之前，计算机在人类自然语言的理解上，简直是不值一提。哪怕是ChatGPT，在实际的软件需求分析上，也只能提供很少的帮助（极度模板化的部分）。在自然语言的难度至上，软件需求还有一个更加难以标准化处理的玩意，那就是人本身，软件的用户有些时候是软件系统本身，有些时候必须面对人。只需要看看软件培训行业每年有多少的投入就知道这个问题了，更别提还有大量自学，大量开源的内容贡献者进行软件培训。

对于软件而言，体验个人化始终是一个很好的卖点，是字面意义上的卖点。Google、百度，各个电商平台的推荐算法，那都是试图提供更加个性化的软件使用体验。更不用提针对特定行业的那些软件，比如对该行业的软件使用人员提供更加个性化的界面，这个个性化不是针对一个人，而是针对一群人、一类人。那么，在通用的框架，例如Qt，基础上要进行什么样的定制呢？比如测量和控制的上位机软件，通常会定制一些模拟实际物理仪器的界面元素。

软件开发是在标准化和个性化中寻求一个平衡点。反复开展需求分析是一个必然的过程；反复折腾软件框架和库是另一个必然的过程。因此软件开发要想走得远，必须着力于三个领域：

- 需求分析：应用领域分析和软件需求
- 库的选择：了解各类库提供的标准化功能
- 库的定制：库提供的定制与二次开发能力

其中，需求分析的人才是最为稀缺的，但是能够在一定程度上对后面两点有意识的涉猎并跟第一点很好结合的人才，同样稀缺。至于那些开发库的高端人才，实际上我们国家的软件开发也逐步深度卷到这里面，我觉得迟早这帮人也能更好挣到钱。

使用PyQt5来实现用户界面，是一个很自然的选择，有大量的计算、建模、数据处理、网络爬虫的脚本和程序采用Python来开发。利用PyQt5可以很快速地搭建一个可用的用户界面，提高Python程序的可用性。这方面的中文书籍已经有很多。

## PyQt5中文书籍

学习PyQt5有一小段时间了，看了大概四五本书的样子。

- PyQt编程快速上手，人民邮电出版社，任路顺，2023-04
- Qt for Python PySide6 GUI界面开发详解与实例，清华大学出版社，李增刚，2022-08
- PyQt从入门到精通，清华大学出版社，明日科技，2021-06
- Python Qt GUI与数据可视化编程，人民邮电出版社，王维波，2019-09
- PyQt5快速开发与实战，电子工业出版社，王硕，2017-10

这几本书的结构简单整理如下，建议只看看目录或者电子版快速过一遍就行，实际的参考性不太强。

![PyQt编程快速上手](/pyqt5-img/book1.png)


![Qt for Python PySide6 GUI界面开发详解与实例](/pyqt5-img/book2.png)


![PyQt从入门到精通](/pyqt5-img/book3.png)


![Python Qt GUI与数据可视化编程](/pyqt5-img/book4.png)


![PyQt5快速开发与实战](/pyqt5-img/book5.png)


国内还有一本书，微信读书上没有电子版，必须在[文泉书局](https://wqbook.wqxuetang.com)购买电子版或者购买纸质书。

- Qt5/PyQt5实战指南：手把手教你掌握100个精彩案例

这本书的前14章针对Qt来讲，后面的章节才是PyQt5。这本书的例子比较多，C++的例子61个，PyQt的例子39个。这三十九个例子，多半还是着眼在比较细节的地方：

- designer的使用
- 信号与槽的使用
  - 自定义信号和槽
- 布局管理器
- 各种控件的使用
- 自定义绘制
- SDI和MDI
- 事件处理
- 多线程

书里的例子启发性比较强，具体细节交代得很清楚，分类也一目了然，是一个很好的参考书，值得购买。

国外的PyQt5的书比较多，扫射了若干本，感觉比国内的书写得还要差。有一本Cookbook，都出到第三版，其内容非常简易，干货特别少，举的例子也不是很恰当。反而是国内的那几本书，还能稍微看看。

要学习PyQt5，目的肯定是用PyQt5来做项目、做应用。其实最好的学习资料我感觉是[qt.io](https://doc.qt.io/qt-5)
，如果对C++有一定的基础，很容易把这里面的内容结合到PyQt5中的stub（也就是*.pyi文件中描述的类接口），对具体开发的支持帮助很大。我基本上都是要用什么就看什么，然后去找Qt5
C++的文档来看。所以我写的这一系列教程，非常少罗列方法、信号和槽，因为这些信息在qt.io上非常齐全，介绍也很清晰。

这个系列基本上是从应用侧来观察PyQt5，落脚点始终在功能和应用上。国内的这几本我看过的书，相对来说，信息量也都是比较小，罗列接口的比较多，但是又不能作为完整的参考手册来使用，非常尴尬。其中那本从《PyQt5快速开发与实战》，我还不幸买了纸质版，很厚一本，挺烦人的。

## PyQt5类结构和帮助速查应用

说起[qt.io](https://doc.qt.io/qt-5)，有个小毛病，按类名查询慢吞吞的，主要是别的东西太多。那就自己撸一个。这里通过一个实际有用的例子，来展示PyQt5的开发结果。

开发需求，两个报表：

- 显示PyQt5的类结构
- 显示对应类的帮助

交互设计：

- 显示一个类的树视图
- 点击类名，显示对应的帮助

## 实现与解释

文件头很简单，导入包，这里专门用了*的方式导入，把所有类名都放到当前的空间中，目的是为了变了反射。

```python
import re
import sys

from PyQt5.QtCore import pyqtSignal, pyqtSlot, QUrl, Qt
from PyQt5.QtWebEngineWidgets import QWebEngineView
from PyQt5.QtWidgets import QTreeWidgetItem, QWidget, QVBoxLayout, QLineEdit, QTreeWidget, QTreeWidgetItemIterator, \
    QApplication, QMainWindow, QDockWidget
import PyQt5
import glob
import importlib
```

这里就用sys.modules把类名转换成类本身。从这里也可以看到，Python是如何找到一个类的。

```python
def str_to_class(name, module="PyQt5"):
    package_name, module_name = name.split(".")
    return getattr(sys.modules[module + "." + package_name], module_name)
```

这个函数，就是找出pyi文件中的顶级类，也就是那些没有父类的类。在PyQt5的各个结构定义中，把父类设定为是`sip.wrapper`
或者`PyQt5.sipsimplewrapper`的哪些类。具体的路径，根据每个人PyQt5的安装位置不同。这里我们只针对"QtCore", "QtGui", "
QtWidgets"
三个包。打开pyi文件，对每行进行模式匹配。这里有一点点Python的字符串匹配的内容`"^class ([^\(\)]*)\(sip.wrapper\):$"`
，这里就是匹配那些行开头是class，行结尾是:，中间把类名抓出来。方法最后，调用上面的函数，把字符串转为类。

```python
def pyi_file_names(pyqt5_root):
    return glob.glob(f"{pyqt5_root}/*.pyi")

def root_object_names(pyqt5_root=PyQt5.__path__[0], class_fingerprint="^class ([^\(\)]*)\(sip.wrapper\):$"):
    files = pyi_file_names(pyqt5_root)
    names = []
    for f in files:
        module = f.split("\\")[-1].split(".")[0]
        if module.startswith("Qt"):
            importlib.import_module(f"PyQt5.{module}")
            with open(f) as fid:
                for line in fid:
                    ret = re.match(class_fingerprint, line)
                    if ret is None:
                        continue
                    captures = ret.groups()
                    if len(captures) > 0:
                        names.append(module + "." + captures[0])

    return [str_to_class(n) for n in sorted(names)]
```

此外还要定义一个方法，把类和子类构造成`QTreeWidget`中显示的形式。这个函数返回的是一个`QTreeWidgetItem`
，通过这个类的构造方法，自动把父子关系给建立起来了。这个函数的要点是两个。

- __subclasses__魔术方法，得到子类；
- 递归调用，构造父类-子类的完整的树

```python
def walk_to_QTreeWdigetItem(self: object, parent: QTreeWidgetItem = None):
    sc = self.__subclasses__()
    item = QTreeWidgetItem(parent)
    item.setText(0, self.__name__)
    item.setText(1, f"{len(self.__dict__)}")
    item.setExpanded(True)
    for c in sc:
        walk_to_QTreeWdigetItem(c, item)
    return item

```

实现这个玩意的痛点就是要求能够搜索得比较快，那么这里我们定义一个包含搜索框和QTreeWidget的QWidget。构造函数中第一部分是构造界面，安排布局，上面一个搜索框，下面一个树控件。

第二部分就是树控件里面填充进PyQt5的类。

第三部分就是处理两个信号。

- 树控件的点击
- 搜索框的文字变化

可以看到，这个类还定义了一个信号，选择了一个项，这个信号，由树控件的点击事件触发，信号的参数就是类名。

```python
class TreeWithSearch(QWidget):
    selectItem = pyqtSignal(str)

    def __init__(self, parent=None, classes=None):
        super(TreeWithSearch, self).__init__(parent=parent)
        self.layout = QVBoxLayout(self)
        self.searchBox = QLineEdit()
        self.classes = QTreeWidget()
        self.layout.addWidget(self.searchBox)
        self.layout.addWidget(self.classes)
        self.setLayout(self.layout)

        self.searchBox.setPlaceholderText("type class name")

        if classes is None:
            classes = root_object_names()
            classes.extend(root_object_names(class_fingerprint="^class ([^\(\)]*)\(PyQt5.sipsimplewrapper\):$"))


            classes.sort(key=lambda iClass: iClass.__name__)

        for i, c in enumerate(classes):
            root = walk_to_QTreeWdigetItem(c)
            self.classes.addTopLevelItem(root)

        self.classes.setHeaderLabels(["name", "funcs"])
        self.classes.setColumnHidden(1, True)
        self.classes.setHeaderHidden(True)

        self.classes.clicked.connect(
            lambda index:
            self.selectItem.emit(self.classes.itemFromIndex(index).text(0))
        )

        self.searchBox.textChanged.connect(self._textChanged)

```

接下来就是有意思的一个点，搜索框的文字变化。这段代码是一个参数为字符串的槽函数。槽函数中，如果搜索框为空白，那么就遍历树控件中有子节点的节点，把他们全部设为折叠。

第二部分代码处理搜索。针对这个搜索字符串，我们遍历所有的节点，如果节点对应的类名包含搜索字符串，我们把这个节点的所有夫节点设为可见、展开。不包含的节点，设为隐藏。

```python
    @pyqtSlot(str)
    def _textChanged(self, name: str):
        # collapse parent nodes
        if name.isspace():
            iterator = QTreeWidgetItemIterator(self.classes, QTreeWidgetItemIterator.HasChildren)
            while (item := iterator.value()) is not None:
                item: QTreeWidgetItem
                item.setExpanded(False)
                iterator += 1
            return

        iterator = QTreeWidgetItemIterator(self.classes, QTreeWidgetItemIterator.All)
        while (item := iterator.value()) is not None:
            item: QTreeWidgetItem
            class_name: str = item.text(0).lower()
            is_show = name.strip().lower() in class_name
            item.setHidden(not is_show)
            # toggle to show and expand all its ancestors
            if is_show:
                p = item
                while p := p.parent():
                    p.setHidden(False)
                    p.setExpanded(True)
            iterator += 1
```

这里的关键知识点就是QTreeWidgetItemIterator，这个遍历器的访问方法就是`it.value()`函数和`it += 1`。如果漏掉了这个加一，那这个循环就成了无限循环。

最后就是整一个浏览视图。

```python
class QtHelpView(QWebEngineView):

    def __init__(self, parent=None):
        super(QtHelpView, self).__init__(parent)
        self.base_url = "https://doc.qt.io/qt-5"
        self.load(QUrl(self.base_url))

    @pyqtSlot(str)
    def show_class(self, name):
        self.load(QUrl(f"{self.base_url}/{name.lower()}.html"))
```

主函数为一个要做的就是把界面搭起来，并且把TreeWithSearch的选择一项的信号与QtHelpView中的槽函数连起来。

```python
if __name__ == '__main__':
    app = QApplication([])

    main_window = QMainWindow()

    tree = TreeWithSearch(main_window)

    dock = QDockWidget()
    dock.setWidget(tree)

    main_window.addDockWidget(Qt.LeftDockWidgetArea, dock)

    # pip install PyQtWebEngine
    view = QtHelpView(main_window)
    tree.selectItem.connect(view.show_class)

    main_window.setCentralWidget(view)

    main_window.resize(1440, 900)
    main_window.show()
    sys.exit(app.exec_())

```

## 最终界面和完整源代码

### 界面

最终实现的界面如下。通过这个app，可以浏览PyQt5主要的类结构，比如QObject和QPaintDevice这两个最主要的基类。点击一个类，就可以在右边看到对应的帮助（前提得上网）。

![帮助查询App](/pyqt5-img/class-find.png)




### 完整的代码

[pyQt代码下载](/pyqt5-code/pyqt5help.py)

实际中，要求已经安装了PyQt5和PyQtWebEngine。这个代码是一个很好的参考，可以用来做其他的类似的应用。

```shell
pip install PyQt5 PyQtWebEngine
```

安装后运行：

```shell   
python pyqt5help.py
```

## 总结

1. [qt.io](https://doc.qt.io/qt-5)是最好的学习资源；
2. Python的良好反射特性（魔术方法）是探索实际代码和机制的很好工具；
3. 学一个东西拿来用好过学一百个东西。