+++
title = 'Racket FFI调用GSL库'
date = 2026-03-31T10:15:40+08:00
draft = true
mathkatex = true
categories = ['racket','lisp', 'programming-language',  'ffi']
tags = ['lisp', '编程', 'racket', 'ffi', 'gsl', '科学计算']
toc = true
tocBorder = true
+++

## Racket 是什么？

Racket是Lisp编程语言家族的一员，它与SBCL所实现的Common Lisp不同，所有符号都定义在同一个命名空间中，这个方式称为Lisp-1，而SBCL的方式称为Lisp-2。

Racket的设计使得它更适合于教学和快速开发，并且它采用了更有一致性的语法。所有的函数和变量都采用define来定义。

Racket的生态系统可能不见得有SBCL丰富（见仁见智），但是Racket的文档和社区可比Common Lisp好太多了。我整天看Lisp的文档简直像看天书，大佬们都说，看源代码就行了……好吧，你写的你说啥就是啥……

Racket的文档，有统一的风格，清晰的结构，大概率有较为详细的说明。而且Racket的社区也确实比common lisp更活跃，尤其是在教学和快速开发方面。

官方安装的Racket自带一个包管理系统，叫做raco，可以用来安装各种库和工具。Racket的包管理系统相当于Python的pip或者Node.js的npm，使用起来非常方便。而且，虽然丑丑的，Racke他还带了一个叫做DrRacket的IDE，虽然~~不太好用~~比较简陋，但对于初学者来说还是挺友好的。



## GSL中的多项式求根

下面是官方文档中求取$x^5 - 1 = 0$的根的例子：

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

```racket
(require ffi/unsafe)
(require ffi/vector)

(define gsl-lib
  (let ([lib "gsl.dll"])
    (ffi-lib lib)))

;; Abstract FFI object definition
(define-syntax-rule (define-ffi-obj racket-name c-name type)
  (define racket-name (get-ffi-obj c-name gsl-lib type)))

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


```racket
;; Run the demo
(for ([n (in-list '(3 4 5 6 7 8 9 10))])
  (solve-poly-demo n))
```