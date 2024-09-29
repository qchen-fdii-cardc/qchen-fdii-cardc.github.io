+++
title = '012 白日梦先生的白日梦'
date = 2024-09-28T21:52:51+08:00
draft = false
mathjax = false
categories = ['lisp']
tags = ['教程', 'lisp', '入门', '1am']
toc = true
tocBorder = true
+++


## 白日梦先生的白日梦

白日梦先生已经跟着大家一起学Lisp长达两个月零五天！

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
11. [011 没人先生学习Lisp函数](/posts/011-functions.md)

他已经能够：

- 安装库文件、探索库函数、调用库函数
- 定义函数、调用函数
- 操纵列表
- 递归、迭代
- 表达式替换，表达式求值
- 判断相等
- 建立一个网站！！！

白日梦先生自觉天下无敌，接下来呢？他想得很简单，那就是编一个`quicklisp`排名第一的库给大家用！

> 梦想还是要有，万一实现了呢？

这就是白日梦先生秉持的信念！

## 头号敌人

白日梦先生考虑好自己的目标，花了大概三个小时详细体验实现之后自己在知乎论坛、CSDN、博客园、大型交友网站上获得大量反馈的情景，再详细设计自己面对网友如潮的点赞、收藏、关注和评论之后的表情、语气、回复。

然后，就没有然后了，因为白日梦先生他毕竟是白日梦先生，他只需要梦想，不需要现实。

白日梦先生的头号敌人，名叫不可能先生。

他哈哈大笑（他总是先哈哈大笑），对着白日梦先生说：做梦吧你，这不可能！

![Quicklisp收录库链接](/lisp-img/quicklisp-library.png)

白日梦先生和不可能先生打开了`quicklisp`收录库链接

[Quicklisp收录库链接](https://quicklisp.org/beta/releases.html)

这个排名是按照`#'string<`函数的结果排序的，也就是按照字母顺序排序的。

白日梦先生脸微微发热，心怦怦跳，难道，这一次的梦想真的能够实现？

![Quicklisp收录库](/lisp-img/quicklisp-release.png)


那么很简单，只需要编一个库，取名为`0xxxx`，那么就可以排在目前排第一名的`1am`之前了！


那么这个`1am`是什么呢？好奇先生已经停不住了。

## **排名第一**的`1am`

不管三七二十一，好奇先生先在REPL中输入命令，想看看这个库到底是什么。



```common-lisp
(ql:quickload '1am)
```

    To load "1am":
      Load 1 ASDF system:
        1am

    (1AM)
    ; Loading "1am"
    


可以看到，这个库定义了一个`ASDF`系统，名字叫`1am`。只要能够加载，好奇先生就能够为所欲为。   


```common-lisp
(require 'explore-lisp)

(multiple-value-list (el:dir '1am))
```

    NIL

    ((1AM:RUN 1AM:*TESTS* 1AM:SIGNALS 1AM:TEST 1AM:IS) 5)



只有区区五个符号，而且，看起来似乎是一个测试库。好奇先生猜测。

这完全难不倒好奇先生，只要运行下面的命令，这个库的深浅或者长短就完全暴漏出来了呢！


```common-lisp
(el:export-all-external-symbols '1am :start-level 2)
```

    NIL



###  `*TESTS*`

```lisp
1AM:*TESTS*
  [symbol]

*TESTS* names a special variable:
  Value: NIL
  Documentation:
    A list of tests; the default argument to `run'.
```
###  `IS`

```lisp
1AM:IS
  [symbol]

IS names a macro:
  Lambda-list: (FORM)
  Documentation:
    Assert that `form' evaluates to non-nil.
  Source file: /home/qchen/quicklisp/dists/quicklisp/software/1am-20141106-git/1am.lisp
```
###  `RUN`

```lisp
1AM:RUN
  [symbol]

RUN names a compiled function:
  Lambda-list: (&OPTIONAL (TESTS *TESTS*))
  Derived type: (FUNCTION (&OPTIONAL T) (VALUES &OPTIONAL))
  Documentation:
    Run each test in the sequence `tests'. Default is `*tests*'.
  Source file: /home/qchen/quicklisp/dists/quicklisp/software/1am-20141106-git/1am.lisp
```
###  `SIGNALS`

```lisp
1AM:SIGNALS
  [symbol]

SIGNALS names a macro:
  Lambda-list: (CONDITION &BODY BODY)
  Documentation:
    Assert that `body' signals a condition of type `condition'.
  Source file: /home/qchen/quicklisp/dists/quicklisp/software/1am-20141106-git/1am.lisp
```
###  `TEST`

```lisp
1AM:TEST
  [symbol]

TEST names a macro:
  Lambda-list: (NAME &BODY BODY)
  Documentation:
    Define a test function and add it to `*tests*'.
  Source file: /home/qchen/quicklisp/dists/quicklisp/software/1am-20141106-git/1am.lisp
```

## `1am`的五脏六腑

而且，好奇先生还直接看到源文件的地址，那就去好好看看！

```bash
> wc -l *
  27 1am.asd
 105 1am.lisp
 114 README.md
 246 total
```

原来，这个**排名第一**的库，就只有三个文件，总共246行，代码就只有132行！

### ASDF 定义文件
```lisp
;;; Copyright (c) 2014 James M. Lawrence
;;; 
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(defsystem :1am
  :description "A minimal testing framework."
  :license "MIT"
  :author "James M. Lawrence <llmjjmll@gmail.com>"
  :components ((:file "1am")))
```

### 代码文件
```lisp
;;; Copyright (c) 2014 James M. Lawrence
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(defpackage #:1am
  (:use #:cl)
  (:export #:test #:is #:signals #:run #:*tests*))

(in-package #:1am)

(defvar *tests* nil "A list of tests; the default argument to `run'.")
(defvar *pass-count* nil)
(defvar *running* nil)
(defvar *failed-random-state* nil)

(defun %shuffle (vector)
  (loop for i downfrom (- (length vector) 1) to 1
        do (rotatef (aref vector i) (aref vector (random (1+ i)))))
  vector)

(defun shuffle (sequence)
  (%shuffle (map 'vector #'identity sequence)))

(defun call-with-random-state (fn)
  (let ((*random-state* (or *failed-random-state*
                            (load-time-value (make-random-state t)))))
    (setf *failed-random-state* (make-random-state nil))
    (multiple-value-prog1 (funcall fn)
      (setf *failed-random-state* nil))))

(defun report (test-count pass-count)
  (format t "~&Success: ~s test~:p, ~s check~:p.~%" test-count pass-count))

(defun %run (fn test-count)
  (let ((*pass-count* 0))
    (multiple-value-prog1 (call-with-random-state fn)
      (report test-count *pass-count*))))

(defun run (&optional (tests *tests*))
  "Run each test in the sequence `tests'. Default is `*tests*'."
  (let ((*running* t))
    (%run (lambda () (map nil #'funcall (shuffle tests)))
          (length tests)))
  (values))

(defun call-test (name fn)
  (format t "~&~s" name)
  (finish-output)
  (if *running*
      (funcall fn)
      (%run fn 1)))

(defmacro test (name &body body)
  "Define a test function and add it to `*tests*'."
  `(progn
     (defun ,name ()
       (call-test ',name (lambda () ,@body)))
     (pushnew ',name *tests*)
     ',name))

(defun passed ()
  (write-char #\.)
  ;; Checks done outside a test run are not tallied.
  (when *pass-count*
    (incf *pass-count*))
  (values))

(defmacro is (form)
  "Assert that `form' evaluates to non-nil."
  `(progn
     (assert ,form)
     (passed)))

(defun %signals (expected fn)
  (flet ((handler (condition)
           (cond ((typep condition expected)
                  (passed)
                  (return-from %signals (values)))
                 (t (error "Expected to signal ~s, but got ~s:~%~a"
                           expected (type-of condition) condition)))))
    (handler-bind ((condition #'handler))
      (funcall fn)))
  (error "Expected to signal ~s, but got nothing." expected))

(defmacro signals (condition &body body)
  "Assert that `body' signals a condition of type `condition'."
  `(%signals ',condition (lambda () ,@body)))
```

这样就非常清楚了，这个库通过`ASDF`定义了一个系统，然后定义了几个宏和函数，用来进行测试。

- `test` 定义一个测试函数，然后将这个函数加入到`*tests*`列表中
- `is` 用来断言一个表达式是否为真
- `signals` 用来断言一个表达式是否会抛出一个特定的异常
- `run` 用来运行`*tests*`列表中的所有测试函数
- `*tests*` 用来存放所有的测试函数，作为`run`的默认参数


## `1am`使用示例

完全照搬自`1am`的`README.md`文件，挠痒痒先生对此负全责。

首先是定义测试的部分，相当透明。


```common-lisp
(defpackage :example (:use :cl :1am))
(in-package :example)

(test foo-test
    (is (= 1 1))
    (is (zerop 0)))

(test bar-test
    (signals simple-error
    (error "bar!")))



```

    #<PACKAGE "EXAMPLE">

    #<PACKAGE "EXAMPLE">

    FOO-TEST

    BAR-TEST



### 简单测试

接下来是简单的测试，有几种运行测试的办法。

- 直接运行`run`函数，会运行`*tests*`列表中的所有测试函数
- 运行`test`函数（真的是一个普通的函数），会运行指定的测试函数
- 直接构造一个测试函数列表，作为参数传递给`run`函数
- 设置`*tests*`列表，然后运行`run`函数

实际上，这样就可以满足定位bug，快速测试的所有需求了。由于Lisp的动态特性，还能够在调试中动态定义测试函数，加入运行。


```common-lisp
(in-package :example)
(run)
```

    #<PACKAGE "EXAMPLE">

    FOO-TEST..
    BAR-TEST.
    Success: 2 tests, 3 checks.



```common-lisp
(foo-test)
```

    FOO-TEST..
    Success: 1 test, 2 checks.



```common-lisp
(run '(bar-test))
```

    BAR-TEST.
    Success: 1 test, 1 check.



```common-lisp
(setf *tests* '(foo-test)) 
(run)
```

    (FOO-TEST)

    FOO-TEST..
    Success: 1 test, 2 checks.


## 探索永不停止

这个**排名第一**的测试库`1am`，用起来非常简单，整个设计非常透明，代码量也很小，其实对于复杂系统的测试而言，这可能是一个很大的优点。

好奇先生看了一眼在做白日梦的白日梦先生和觉得什么都不可能的不可能先生，心里想，其实Common Lisp还真有些库是大家都在引用的，比如`alexandria`，不行再把`alexandria`好好探索一下？

好奇先生的时间快速飞逝，deadline也不停tik-tok-tik-tok。
