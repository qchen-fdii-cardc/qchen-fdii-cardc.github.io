+++
title = '002 Multiprocessing与pyinstaller冻结程序'
date = 2025-01-04T16:11:10+08:00
draft = false
mathjax = false
categories = ['pyqt-in-action', 'python', 'pyqt']
tags = ['pyqt-in-action', 'frozen', 'multiprocessing', 'pyinstaller', 'upx', 'ui', 'design']
toc = true
tocBorder = true
+++


> 采用Pyinstaller冻结打包多进程程序时，必须非常小心。这个技术线在Windows上会有一个非常严重的Bug。直接运行打包后的程序会造成无限创建进程，直到系统崩溃。

## 问题描述


本文针对一个非常具体的场景，需求包括以下要素：

- 需要用PyQt5设计GUI程序
- 需要调用其他库完成后台计算，计算与GUI线程松耦合
- 通过PyInstaller打包成独立可执行程序

## 多进程的使用

在Python的标准库中，`multiprocessing`模块提供了多进程的支持。

如果我们采取这个技术线，则通常采用计算进程的方式来实现。

### 无限循环的计算进程

```python
def worker(iq, oq):
    while True:
        # get input from input queue
        data = iq.get()
        # process data
        result = f(data) 
        # put result to output queue
        oq.put(result)
```
这个是一个典型的单纯的数据处理的计算过程，`f(data)`是一个计算函数，`iq`是输入队列，`oq`是输出队列。这个函数的内部是一个死循环，不断地从输入队列中获取数据，然后处理数据，最后将结果放到输出队列中。函数的大部分时间预计在`get()`函数的阻塞（当队列中没有数据时）和`f(data)`的计算上。

### 主进程启动计算进程

```python
import multiprocessing

if __name__ == '__main__':


    input_q = multiprocessing.Queue()
    output_q = multiprocessing.Queue()

    multiprocessing.Process(target=worker, 
        args=(input_q, output_q), 
        daemon=True).start()    
```
这里的`daemon=True`表示这个进程是一个守护进程，当主进程结束时，这个进程也会结束，必须要等待这个进程结束后，才能结束主进程。如果不设置这里或者`daemon=False`，就需要在`worker`函数中设置一个退出条件。例如：

```python
def worker(iq, oq):
    while True:
        # get input from input queue
        data = iq.get()
        if data == 'EXIT':
            break
        # process data
        result = f(data) 
        # put result to output queue
        oq.put(result)
```
这样，当主进程发送一个`EXIT`的数据时，计算进程就会退出。

### 数据共享

在`multiprocessing`中，有两个机制可以实现进程中的数据共享：

- `Queue`：进程间通信的队列
- `Pipe`：进程间通信的管道

这两个机制中，`Queue`是比较高层次的，`Pipe`是比较底层的。所以后者的效率（也许）会更高。对于我们常规的应用，主要计算时间在上面的`f(data)`，所以直接使用`Queue`就可以了。

## PyInstaller和UPX

在交付Python应用时，通常的术语成为“冻结”（Frozen）。冻结的目的是将Python程序打包成一个独立的可执行文件，这个文件可以在没有Python解释器的环境中运行。

常见的冻结工具有：

- PyInstaller
- cx_Freeze

大概用得比较多的就是这两个，我使用前者更多一些。

这个工具的使用方法非常简单，只需要在命令行中输入：

```bash
pyinstaller your_script.py
```

这个命令会在当前目录下生成一个`dist`目录，里面包含了所有的依赖文件和可执行文件。

当然我们还可以设置一些选项，例如：

- `-D`：生成一个目录，而不是一个单独的文件
- `-F`：生成一个单独的文件

### UPX压缩

当然，在生成可执行文件后，我们还可以使用UPX进行压缩。UPX是一个开源的可执行文件压缩工具，可以将可执行文件压缩到更小的体积。

- [UPX官网](https://upx.github.io/)

通常在调用PyInstaller时，我们可以使用下面的命令：

```shell
pyinstaller -D your_script.py --upx-dir=upx-folder
```

这里，就设置了UPX的目录，当然，如果在当前的环境变量的PATH中有UPX，那么就不需要设置这个选项了。

可以用`--no-upx`来禁用UPX。

在常见（PyQt5）情况下，UPX还是能够提供超过50%的压缩率的。非常可观。

## 多进程与冻结的冲突

在Windows开发中，试图冻结一个上面的程序，不会有任何错误提示。

但是会带来一个非常严重的Bug。当客户运行冻结的`exe`时，程序会疯狂创建新的进程，直到系统崩溃。

> 请不要测试……必须直接关机。

这个问题的原因是，`multiprocessing`模块在Windows中使用`spawn`方法来创建新的进程。而在冻结程序中，没有`python`解释器，所以`multiprocessing`模块会调用我们冻结得到的`exe`，然后这个`exe`又会调用`multiprocessing`模块，然后……就会无限循环。

### 解决方案

在冻结多进程`multiprocessing`的程序时，我们需要在`if __name__ == '__main__':`中调用`freeze_support()`函数。

```python
if __name__ == '__main__':
    multiprocessing.freeze_support()
    input_q = multiprocessing.Queue()
    output_q = multiprocessing.Queue()

    multiprocessing.Process(target=worker, 
        args=(input_q, output_q), 
        daemon=True).start()    
```

这个调用必须在任何其他的`multiprocessing`调用之前进行。这里就在`if __name__ == '__main__':`中调用。

## 一个表现良好的例子

### 界面
下面是一个表现良好的例子.

这个例子是一个简单的加法计算器，用户输入两个数，然后计算它们的和。

![](/pyqt5-img/pyqt-in-action-003/ui.png)

后台的计算进程是一个无限循环的进程，不断地从输入队列中获取数据，然后计算，最后将结果放到输出队列中。这里的PyQt5部分采取了硬核布局（！）。

### 代码

```python
import multiprocessing
import sys

from PyQt5.QtWidgets import QApplication, QLineEdit, QListWidget, QPushButton
from PyQt5.QtWidgets import QMainWindow


# process to calculate, data by Queue across processes
def calculate(input_queue_a: multiprocessing.Queue, output_queue_a: multiprocessing.Queue):
    while True:
        try:
            x, y = input_queue_a.get()
            result = x + y
            output_queue_a.put({"x": x, "y": y, "result": result})
        except:
            continue

def tryParse(s, default_value=0.0):
    try:
        num = float(s)
    except ValueError:
        num = default_value
    return num


# Press the green button in the gutter to run the script.
if __name__ == '__main__':
    multiprocessing.freeze_support()

    input_queue = multiprocessing.Queue()
    output_queue = multiprocessing.Queue()

    multiprocessing.Process(target=calculate, daemon=True, args=(input_queue, output_queue)).start()

    # PyQt Window
    app = QApplication(sys.argv)
    window = QMainWindow()
    window.setWindowTitle('Multiprocessing')

    # add ui elements here
    num1_input = QLineEdit(window)
    num1_input.move(20, 20)
    num1_input.resize(200, 30)

    num2_input = QLineEdit(window)
    num2_input.move(20, 60)
    num2_input.resize(200, 30)

    calculate_button = QPushButton('Calculate', window)
    calculate_button.move(20, 100)
    calculate_button.resize(200, 30)

    output_list = QListWidget(window)
    output_list.move(20, 140)
    output_list.resize(200, 200)


    def calculate():
        x = tryParse(num1_input.text() or 0.0)
        y = tryParse(num2_input.text() or 0.0)
        input_queue.put((x, y))
        try:
            xy_result = output_queue.get()
            output_list.addItem(f"{xy_result['x']} + {xy_result['y']} = {xy_result['result']}")
        except Exception as e:
            output_list.addItem(f"Error{e}")

        output_list.scrollToBottom()


    calculate_button.clicked.connect(calculate)
    num2_input.returnPressed.connect(calculate)
    num1_input.returnPressed.connect(calculate)

    window.resize(240, 360)
    # fix window size, set it to non-resizable
    window.setFixedSize(240, 360)

    window.show()

    # exit app when close window
    sys.exit(app.exec_())
```
### 其他注意事项
程序充分考虑了用户输入的错误，当用户输入的不是数字时，会自动转换为0.0。当用户输入的是空字符串时，也会转换为0.0。

并且，按钮，输入框的回调函数都是一个函数。

值得注意的是，Python的`multiprocessing.Queue`是什么都能放，简直是头发安全的程序设计。这里，我们传出的数据是一个字典，包含了输入的两个数和计算的结果。通过重复输入的数据，我们更大程序的避免了计算进行和主进程的耦合。这又是一个典型面向头发安全的编程习惯。

### 打包

在打包之前，应该用`pip`安装`pyinstaller`。这个时候，我们可以使用下面的命令：

```shell
# 设置UPX目录
pyinstaller -D -w ./main.py -n addUpx -y --upx-dir=D:/Users/User/upx-4.2.4-win64

# 不使用UPX
pyinstaller -D -w ./main.py -n addWithoutUpx --noupx -y
```

在我们注意了所有的事项后，我们就可以放心地交付我们的程序。