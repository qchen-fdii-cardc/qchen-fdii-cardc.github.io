+++
title = 'Racket FFI调用GSL库'
date = 2026-03-31T10:15:40+08:00
draft = false
mathkatex = true
categories = ['racket','lisp', 'programming-language',  'ffi']
tags = ['lisp', '编程', 'racket', 'ffi', 'gsl', '科学计算']
toc = true
tocBorder = true
+++

## Racket 是什么？

Racket是Lisp编程语言家族的一员，它与SBCL所实现的Common Lisp不同，所有符号都定义在同一个命名空间中，这个方式称为Lisp-1，而SBCL的方式称为Lisp-2。

Racket的设计使得它更适合于教学和快速开发，并且它采用了更有一致性的语法。所有的函数和变量都采用define来定义。可以看看[Racket的Cheat Sheet](https://docs.racket-lang.org/racket-cheat/index.html)，就会发现Racket的语法非常简洁和一致，几乎所有的定义都是通过define来完成的，无论是函数、变量、模块还是类等等。相比之下，SBCL的Common Lisp有更多的定义形式，比如defun、defvar、defmacro、defclass等等，这些定义形式虽然功能强大，但也增加了学习的复杂度。

![Over-Non-Lispify](/racket/over-non-lisp.jpg)

Racket的生态系统可能不见得有SBCL丰富（见仁见智），但是Racket的文档和社区可比Common Lisp好太多了。我整天看Lisp的文档简直像看天书，大佬们都说，看源代码就行了……好吧，你写的你说啥就是啥……

Racket的文档，有统一的风格，清晰的结构，大概率有较为详细的说明。而且Racket的社区也确实比common lisp更活跃，尤其是在教学和快速开发方面。

官方安装的Racket自带一个包管理系统，叫做`raco`，可以用来安装各种库和工具。Racket的包管理系统相当于Python的pip或者Node.js的npm，使用起来非常方便。而且，虽然丑丑的，Racke他还带了一个叫做DrRacket的IDE，虽然~~不太好用~~比较简陋，但对于初学者来说还是挺友好的。

当然用`raco`可以管理racket的包，当然我们也可以直接用racket的ffi接口来调用C语言的库，这样就可以利用C语言库的性能和功能了。GSL（GNU Scientific Library）就是一个非常强大的科学计算库，提供了各种数学函数、数值方法和算法，可以用来进行数值分析、线性代数、微分方程求解等等。通过Racket的FFI接口，我们可以调用GSL库中的函数来进行科学计算，这样就可以在Racket中愉快算数字。

下面是一个一个简单的例子，展示了如何在Racket中调用GSL库来求解多项式的复数根。

## GSL中的多项式求根

下面是[官方文档](https://www.gnu.org/software/gsl/doc/html/poly.html#examples)中求取$x^5 - 1 = 0$的根的例子。例子要求解$x^5 - 1 = 0$的根，也就是求解$x^5 - 1 = 0$的复数根。显然，方程有五个根，分别是1（实数根）和四个复数根。

$$
1, e^{2\pi i/5}, e^{4\pi i/5}, e^{6\pi i/5}, e^{8\pi i/5}
$$


这个多项式的系数可以用六个浮点数的数组表达，分别是，`-1`（常数项）和`1`（最高次项）和中间的四个0。数组按照从常数项到最高次项的顺序排列。调用GSL库中的函数来求解这个多项式的复数根。从下面的C语言代码可以看到，求解的复数根存在一个长度为10的数组中，前两个元素是第一个根的实部和虚部，接下来的两个元素是第二个根的实部和虚部，以此类推。最后打印出每个根的实部和虚部。

```c
#include <stdio.h>
#include <gsl/gsl_poly.h>

int
main (void)
{
  int i;
  /* coefficients of P(x) =  -1 + x^5  */
  double a[6] = { -1, 0, 0, 0, 0, 1 };
  double z[10];

  gsl_poly_complex_workspace * w
      = gsl_poly_complex_workspace_alloc (6);

  gsl_poly_complex_solve (a, 6, w, z);

  gsl_poly_complex_workspace_free (w);

  for (i = 0; i < 5; i++)
    {
      printf ("z%d = %+.18f %+.18f\n",
              i, z[2*i], z[2*i+1]);
    }

  return 0;
}
```

## Racket FFI调用GSL库

```racket
#lang racket

```
下面就是Racket调用GSL库的代码了，首先是一些基础设施的定义，包括加载动态库和定义抽象的FFI对象的宏。这个宏调用了Racket的`get-ffi-obj`函数来从动态库中获取函数（指针），这个函数需要输入C语言函数的名称、库对象和函数类型。我们在这个宏中间，用`define`来把这个对象绑定到一个Racket变量上，这样我们就可以在后续的代码中直接使用这个变量来调用GSL函数了。

```racket
(require ffi/unsafe)
(require ffi/vector)

(define gsl-lib
  (let ([lib "gsl.dll"])
    (ffi-lib lib)))

;; Abstract FFI object definition
(define-syntax-rule (define-ffi-obj racket-name c-name type)
  (define racket-name (get-ffi-obj c-name gsl-lib type)))
```

下面就利用上述宏来导入GSL库中求解多项式根的相关函数了。我们定义了三个FFI对象：`poly-complex-workspace-alloc`用于分配工作空间，`poly-complex-workspace-free`用于释放工作空间，`poly-complex-solve`用于求解多项式的复数根。从这里可以看到Racket的ffi接口中定义了对应于C语言类型的Racket符号，比如`_fun`表示函数类型，`_ulong`表示无符号长整数（通常`size_t`就是这个类型），`_pointer`表示指针类型，`_void`表示无返回值的函数，`_int`表示整数类型等等。

```racket
;; Minimal FFI bindings for GSL polynomial solver using idiomatic Racket names
(define-ffi-obj poly-complex-workspace-alloc
  "gsl_poly_complex_workspace_alloc"
  (_fun _ulong -> _pointer))

(define-ffi-obj poly-complex-workspace-free
  "gsl_poly_complex_workspace_free"
  (_fun _pointer -> _void))

(define-ffi-obj poly-complex-solve
  "gsl_poly_complex_solve"
  (_fun _pointer _ulong _pointer _pointer -> _int))
```


```racket
;; Format a complex number as a string with specified precision (12 decimal places by default)
(define (format-complex re im [precision 12])
  (string-append
    (if (negative? re)
        "-"
        " ")
    (real->decimal-string (abs re) precision)
    (if (negative? im)
        " - "
        " + ")
    (real->decimal-string (abs im) precision)
    "i"))
```

其实看到上面这个函数，感觉Racket或者Lisp的语言最好的一点就是，修改的时候，非常自然，直接插入一个括号对在函数的参数中，完全没有心理负担。

在racket中，可以用`let`来构造局部作用域的变量绑定；但是在这里也可以直接使用`define`来定义变量。这里`define`的作用域是整个函数体，所以在函数体中都可以访问这些变量，有时候，感觉这样比let要思维负担小一点。也可能这只是我的大脑还不够Lisp化吧……

我们定义了一个函数`solve-poly-demo`，它接受一个参数`n`，表示多项式的阶数，当然采用`[]`这个Lisp异端来表示可选参数也是挺清晰的，其默认值为7。这个函数的作用是求解多项式$P(x) = -1 + x^n$的复数根，并打印出来。

```racket
;; Minimal usage: solve P(x) = -1 + x^n
(define (solve-poly-demo [n 7])
  ;; We want to solve x^n - 1 = 0, which has n roots (the nth roots of unity)
  (define poly-size (+ 1 n)) ; degree n polynomial has n+1 coefficients
  (define a (make-f64vector poly-size 0.0))
  (f64vector-set! a 0 -1.0)
  (f64vector-set! a n 1.0)
  (define z (make-f64vector (* 2 n) 0.0))
  (define w (poly-complex-workspace-alloc poly-size))
  (poly-complex-solve (f64vector->cpointer a) poly-size w (f64vector->cpointer z))
  (poly-complex-workspace-free w)
  (printf "x^~a - 1 = 0\n" n)
  (for ([i (in-range n)])
    (printf "  z~a = ~a\n"
            i
            (format-complex (f64vector-ref z (* 2 i))
                            (f64vector-ref z (+ 1 (* 2 i)))))))
```

在这之后，就可以调用`solve-poly-demo`函数来求解不同阶数的多项式解：

```racket
;; Run the demo
(for ([n (in-list '(3 4 5 6 7 8 9 10))])
  (solve-poly-demo n))
```

或者，采用更加Racket式的方式：

```racket
;; Run the demo
(for-each solve-poly-demo '(3 4 5 6 7 8 9 10))
```

当然，这个循环跟传统的`map`非常类似，一般而言就是`map`是为了得到一个新的列表，而`for-each`是为了执行副作用（比如打印输出），所以在这里用`for-each`更合适一些。

当然，要运行这个程序，还需要把一个`gsl.dll`文件放在当前目录或者任何系统可以找到的路径下，这个DLL文件就是GSL库的动态链接库，里面包含了我们需要调用的函数的实现。这个DLL文件可以从GSL的官方网站上下载，或者自己编译GSL库来生成这个DLL文件。

推荐下载方式：[nuget gsl-msvc-x64](https://www.nuget.org/packages/gsl-msvc-x64)。这个包提供了预编译的GSL库的DLL文件，适用于Windows系统，并且是针对x64架构编译的。下载这个包之后，可以从其中提取出`gsl.dll`文件。

Linux类似的系统就简单了，直接安装GSL库就行了，安装完成之后，系统会把GSL库的动态链接库放在一个标准的位置，Racket的FFI接口就可以找到并加载这个库了。通常情况下，在Linux系统上安装GSL库之后，Racket的FFI接口就可以直接使用这些库，当然`gsl.dll`是Windows特有的，Linux上是`libgsl.so`之类。