# Lisp function 入门


## 函数入门第一步: 调用函数

### 函数的返回值


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


### 函数的参数


函数的参数定义方式:

1. 必须参数;
2. `&optional`可选参数;
3. `&key`关键字参数;
4. `&rest`剩余参数;
5. `&allow-other-keys`允许其他关键字参数;
   



## 从对象开始的概念

根据CLHS的定义,Lisp中的`object`有以下几种情况:

1. 任何Lisp数据,特别的包括通过`cons`构造的两个数据的链接;
2. 函数对象,这里的主要**对象**（！）;
3. 类的对象（几百年以后再说到这个）.

又根据CLHS的定义,函数是一种表示当提供恰当个数的参数即可以执行的代码的对象.产生函数的方式有几种:

1. 函数表达式:`function special form`;
2. 函数转换:`function coerce`
3. 函数编译:`function compile`

调用函数的三种方式:

1. `funcall`;
2. `apply`;
3. `multiple-value-call`.



## 概念参考

### `form`表达式

这个


### `special form`特殊

`special form`是是一个列表, 它不是`macro form`,这个列表有特定的语法和特殊的求值规则,可能会改变求值环境和/或控制流.特殊form的第一个元素称为特殊操作符（`special operator`）.

### `special operator`特殊操作符