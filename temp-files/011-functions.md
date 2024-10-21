## 没人先生

没人先生一直都在, 但是没人注意到他. 他从头到尾一直都在跟大家一起学习Lisp. 

1. [001 粗鲁先生Lisp再出发](https://www.windtunnel.cn/posts/001-rude-start-application/)
2. [002 懒惰先生的Lisp开发流程](https://www.windtunnel.cn/posts/002-lazy-process/)
3. [003 颠倒先生的数学表达式](https://www.windtunnel.cn/posts/003-lazy-process/)
4. [004 完美先生的完美Lisp](https://www.windtunnel.cn/posts/004-perfect/)
5. [005 好奇先生用Lisp来探索Lisp](https://www.windtunnel.cn/posts/005-explore-lisp/)
6. [006 好奇先生在Lisp的花园里挖呀挖呀挖](https://www.windtunnel.cn/posts/006-sequence-in-lisp/)
7. [007 挑剔先生给出终止迭代的条件](https://www.windtunnel.cn/posts/007-recursive-eq/)
8. [008 挠痒痒先生建网站记](https://www.windtunnel.cn/posts/008-real-app/)
9. [009 小小先生学习Lisp表达式](https://www.windtunnel.cn/posts/009-expression/)
10. [010 聪明先生拒(ji)绝(xu)造轮子](https://www.windtunnel.cn/posts/010-smart-cl-classification/)

他感觉他跟Lisp中的函数很像, 天天都在, 每次都要用, 却还是有好多东西是不知道的. 他决定要好好学习一下Lisp中的函数.

## 函数的返回值

函数式编程一个重要的内涵就是值和值之间的替换,这个在Lisp中体现的淋漓尽致.每个函数的调用,放到一个表达式中,都与直接把函数的返回值放在那里是等价的.

函数可以返回任何数量以及任何类型的Lisp对象, 包括函数对象. 当函数返回多个值时, 推荐使用`values`来构造返回值, 而不是使用`list`, 当返回值是`values`,
可以直接使用第一个值, 也可以用`multiple-value-bind`来绑定返回值.


绑定多个值的方式如下:
```lisp
(multiple-value-bind (a b c) (values 1 2 3)
  (list a b c))
```

记得我们可以把一个值替换为另外一个值

```lisp
(defun values-func ()
  (values 1 2 3))

(multiple-value-bind (a b c) (values-func)
  (format t "a: ~a, b: ~a, c: ~a~%" a b c))
;; a: 1, b: 2, c: 3
;; NIL

(multiple-value-list (values-func))
;; (1 2 3)
```

当我们直接使用上面的`values-func`时, 只会返回第一个值, 也就是`1`.

```lisp
(values-func)
;; 1
```

如果我们使用`explore-lisp`来看`Common-Lisp`中的函数定义, 哪些返回空值的函数, 会返回`(values)`, 而返回多个值的函数, 会返回`(values ...)`. 照着这个来抄总是没错的.

就比如,  `floor`函数的返回值是两个, 第一个是`integer`, 第二个是`real`, 这个是通过`values`来返回的.  请仔细看`Delcared type`和`Derived type`两个字段的描述.


```lisp
(describe 'floor)
COMMON-LISP:FLOOR
  [symbol]
FLOOR names a compiled function:
  Lambda-list: (NUMBER &OPTIONAL (DIVISOR 1))
  Declared type: (FUNCTION (REAL &OPTIONAL REAL)
                  (VALUES INTEGER REAL &OPTIONAL))
  Derived type: (FUNCTION (T &OPTIONAL T)
                 (VALUES (OR NULL INTEGER) NUMBER &OPTIONAL))
  Documentation:
    Return the greatest integer not greater than number, or number/divisor.
      The second returned value is (mod number divisor).
  Known attributes: foldable, flushable, unsafely-flushable, movable
  Source file: SYS:SRC;CODE;NUMBERS.LISP
NIL
```

最后,我们还可以直接使用`nth-value`来获取返回值的第n个值.

```lisp
(nth-value 1 (values 1 2 3))
;; 2
```

这个函数就很好地配合函数式编程的思想,可以很容易进行值和表达式的替换,而不需要引入额外的变量.


## 函数的参数


函数的参数定义方式:

1. 必须参数;
2. `&optional`可选参数;
3. `&key`关键字参数;
4. `&rest`剩余参数;
5. `&allow-other-keys`允许其他关键字参数;
   

具体语法和使用方法可以参考[CLCB](https://lispcookbook.github.io/cl-cookbook/functions.html)中,这本书还有[中文版](https://oneforalone.github.io/cl-cookbook-cn/#/zh-cn/01.functions).

```lisp
(defun test-func (a &optional b c &key d e &allow-other-keys)
  (list a b c d e))
```

这里的`optional`和`key`可以同时使用,SBCL可能会报警,但是不影响使用.


## 产生函数

根据CLHS的定义,函数是一种表示当提供恰当个数的参数即可以执行的代码的对象.函数有几种来源:

1. 函数表达式:`function special form`;
2. 函数转换:`function coerce`
3. 函数编译:`function compile`

### 函数`special form`

`special form`是一种特殊的语法形式, 这玩意看起来像是函数, 但却是解释器特殊处理的语法结构. 例如`if`和`cond`就是`special form`. 得益于聪明先生的聪明举动,可以在[special operator](https://www.windtunnel.cn/posts/010-appendix-cl-symbols/#special-operator)中查看所有的`special form`, 一共有`25`个.

这里面的有一个`function`,是一个`special form`, 用来产生函数对象的.

```lisp
(function name)
```

或者使用`#'`来简写:

```lisp
#'name
```

因为Lisp的函数和变量可以用一样的符号,所以这个`#'`就是为了区分函数和变量的. 

此外,`function`还可以接受一个`lambda`来产生一个函数对象.

```lisp
(setf func (function (lambda (x) (+ x 1))))
(func 1)
;; 2
```

总的来说,这个很平凡,我们记得在访问函数的时候明确使用`#'`就可以了.

### 函数`coerce`

这个函数是强制类型转换的意思,可以将一个对象转换为另外一个对象. 当我们使用`coerce`来转换一个函数对象时,会返回一个函数对象.

> If the result-type is function, and object is any function name that is fbound but that is globally defined neither as a macro name nor as a special operator, then the result is the functional value of object.
> If the result-type is function, and object is a lambda expression, then the result is a closure of object in the null lexical environment.

```lisp
; 第一种情况,把一个函数名(不能是宏名或者特殊操作符)转换为函数对象,结果返回的是一个函数对象.
(coerce 'car 'function)

; 第二种情况,把一个lambda表达式转换为函数对象,结果返回的是一个**闭包**,这个闭包是在一个空的词法环境中的.
(coerce #'(lambda (x) (+ x 1)) 'function)
```

### 函数`compile`

大部分函数都是表现为这一类,例如,文件被加载, 其中的函数就是这样,还可以直接调用`compile`来编译一个函数.

```lisp
(defun test-func (x)
  (+ x 1))

(compile 'test-func)

(compiled-function-p #'test-func)
;; T
```


## 函数调用

调用函数的三种方式:

1. `funcall`;
2. `apply`;
3. `multiple-value-call`.


### `funcall`

`funcall`调用一个函数对象时,第一个参数是函数对象,后面的参数是函数的参数.

```lisp
(funcall #'(lambda (x) (+ x 1)) 1)
;; 2
```

### `apply`

`apply`调用一个函数对象时,函数对象之后的参数是一个列表,这个列表的元素是函数的参数.

```lisp
(apply #'(lambda (x y) (+ x y)) '(1 2))
;; 3
```

### `multiple-value-call`

`multiple-value-call`调用一个函数对象时,把每一个`values`都分别展开,然后作为参数传递给函数.

```lisp
(multiple-value-call #'(lambda (x y) (values (+ x y) (- x y))) 1 2)
;; 3 -1

(multiple-value-call #'+ (values 3 4) 2 1 (values 8 7 3))
;; 28
```



## 总结

1. 函数是一种对象,可以返回任何数量以及任何类型的Lisp对象, 包括函数对象;
2. 函数的返回值推荐使用`values`来构造,此时直接调用返回第一个值,可使用`multiple-value-bind`来绑定返回值,或者使用`nth-value`来获取返回值的第n个值；
3. 函数的参数定义方式有必须参数,可选参数,关键字参数,剩余参数,允许其他关键字参数;
4. 函数的产生方式有函数表达式,函数转换,函数编译;
5. 函数的调用方式有`funcall`, `apply`, `multiple-value-call`;
6. 是谁学习了上面这些? 没有人.
