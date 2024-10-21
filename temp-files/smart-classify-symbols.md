## 聪明先生

聪明先生很聪明, 他仔细观察好兄弟们手忙脚乱慌慌张张急急忙忙学习Lisp的全过程, 默默地把知识点都记在大脑里:

1. [001 粗鲁先生Lisp再出发](/posts/001-rude-start-application/)
2. [002 懒惰先生的Lisp开发流程](/posts/002-lazy-process/)
3. [003 颠倒先生的数学表达式](/posts/003-lazy-process/)
4. [004 完美先生的完美Lisp](/posts/004-perfect/)
5. [005 好奇先生用Lisp来探索Lisp](/posts/005-explore-lisp/)
6. [006 好奇先生在Lisp的花园里挖呀挖呀挖](/posts/006-sequence-in-lisp/)
7. [007 挑剔先生给出终止迭代的条件](/posts/007-recursive-eq/)
8. [008 挠痒痒先生建网站记](/posts/008-real-app/)
9. [009 小小先生学习Lisp表达式](/posts/009-expression/)

对他来说, 影响最深刻的无疑是好奇先生的探索Lisp工具包. 这个工程的地址在[explore-lisp](https://github.com/qchen-fdii-cardc/explore-lisp)。当把这个源代码下载（clone）到本地之后，可以通过`quicklisp`的`quickload`函数加载这个包。

加载之前, 可以把这个包放到`quicklisp`的`local-projects`文件夹里面; 也可以通过修改`asdf:*central-registry*`, 然后在REPL里面执行`(ql:quickload :explore-lisp)`。

具体的方法, 好奇先生已经写得很清楚, 聪明先生觉得自己只需要再练习个七次八次就可以, 如果是一个稍微不聪明一点的人, 可能会觉得不需要练习就会呢! 但是聪明先生不会犯那种错误, 他总是把搞清楚是什么原理和实际练习若干遍结合起来, 所以他才是聪明先生嘛!

聪明先生, 决定彻底学会Lisp!

## 聪明先生学习编程的方法

聪明先生总是清楚地认识自己的不足之处, 并谋定而后动, 学习Lisp也是这样. 对于编程语言的学习, 聪明先生觉得自己最大的问题可能会是重复造轮子! 因为, 你知道, 聪明先生之所以是聪明先生, 就是因为他有一种觉得自己很聪明什么都能做出来, 再搭配上Lisp这样灵活都不足以形容必须称其为毫无底线的语言, 那么, 你知道, 聪明先生就会不停地造出各种轮子...

为了避免如此, 聪明先生觉得自己应该把Common Lisp已经提供的工具好好学习一遍, 当然! 在学习的过程中他可是会免不了造点毫无意义的轮子呢!


### `explore-lisp`工具包

聪明先生不费吹灰之力就把`explore-lisp`工具整好, 接下来就是, 把`common-lisp`这个包的内容好好批判一番.

```lisp
(ql:quickload :explore-lisp)
(require :explore-lisp)
```

主要是先用, `(el:dir :cl)`来列出`cl`的符号(毕竟, 所有的东西都是符号!), 然后在用`describe`, 或者`el:describe-symbol`来查看每个符号的帮助.

看着看着, 聪明先生发现一个大秘密, 每一个符号的帮助都带有一句:

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

> FLOOR names a compiled function:

这不就给聪明先生一个灵感了吗? 他觉得可以把所有符号都用这个`compiled function`类似的描述分下类.

说干就干! 聪明先生开始写代码:

### 符号分类

基本思路, 利用`el:export-all-external-symbols`导出`:cl`所有符号到一个文件中, 然后一行一行的循环, 所有的`xxxx names a xxxx:`这样的行都提取出来.

```lisp
(let ((tfn "temp-files/all-external-symbols.md"))
  (el:export-all-external-symbols :cl :fn tfn)
  (with-open-file (fn tfn)
    ;; read line by line
    (loop for line = (read-line fn nil)
          while line
          do (when (and (search " names a " line) (string-ends-with-p line ":"))
                   (format t "~a~%" line)))))
```

这个代码利用了, `with-open-file`来打开文件, 然后`loop`循环读取每一行, 利用`search`和`string-ends-with-p`来判断是否是我们需要的行.

这里的函数`string-ends-with-p`是一个自定义的函数, 用来判断一个字符串是否以另一个字符串结尾. 聪明先生完全没有忍住, 又造了一个轮子!

```lisp
(defun string-ends-with-p (str ending)
  "Return t if str ends with ending."
  (let ((elength (length ending))
        (slength (length str)))
    (if (>= slength elength)
        (string= (subseq str (- slength elength)) ending)
        nil)))
```

```lisp
......
WRITE-BYTE names a compiled function:
WRITE-CHAR names a compiled function:
WRITE-LINE names a compiled function:
WRITE-SEQUENCE names a compiled function:
WRITE-STRING names a compiled function:
WRITE-TO-STRING names a compiled function:
Y-OR-N-P names a compiled function:
YES-OR-NO-P names a compiled function:
ZEROP names a compiled function:
```

找到了所有这样的行之后, 就需要把字符串的两个部分提取出来. 聪明先生又写了一个函数:

```lisp
(defun split-string-by-substring (str sep)
  ;; split string by substring
  (let ((start 0)
        (end 0)
        (result '())
        (sub-len (length sep)))
    (loop while (setq end (search sep str :start2 start))
          do (progn
              (push (subseq str start end) result)
              (setq start (+ sub-len end))))
    (push (subseq str start) result)
    (nreverse result)))
```

啊, 一个轮子又造好了! 聪明先生觉得自己真是太聪明了!

```lisp
(let ((tfn "temp-files/all-external-symbols.md"))
  (el:export-all-external-symbols :cl :fn tfn)
  (with-open-file (fn tfn)
    ;; read line by line
    (loop for line = (read-line fn nil)
          while line
          do (when (and (search " names a " line) (string-ends-with-p line ":"))
                   (let* ((parts (split-string-by-substring line " names a "))
                          (symbol (first parts))
                          (type-string (second parts))
                          (type-name (trim-string type-string ":")))
                     (format t "~a:~A~%" type-name symbol))))))
```

其实, 这里面聪明先生又犯了一次!

```lisp
(defun trim-string (str &optional (chars " \t\n"))
  (let ((start (position-if (complement (lambda (c) (find c chars))) str))
        (end (position-if (complement (lambda (c) (find c chars))) (reverse str))))
    (if (and start end)
        (subseq str start (- (length str) end))
        "")))
```

这样就可以打印出, 符号类型: 符号名字了.

```lisp
......
macro:WITH-SIMPLE-RESTART
macro:WITH-SLOTS
macro:WITH-STANDARD-IO-SYNTAX
compiled function:WRITE
compiled function:WRITE-BYTE
compiled function:WRITE-CHAR
compiled function:WRITE-LINE
compiled function:WRITE-SEQUENCE
compiled function:WRITE-STRING
compiled function:WRITE-TO-STRING
compiled function:Y-OR-N-P
compiled function:YES-OR-NO-P
compiled function:ZEROP
```

接下来就简单了, 聪明先生觉得只需要整一个`hash-table`就可以了, 然后把符号名字放到对应的类型里面.

```lisp
(defparameter *symbol-types* (make-hash-table :test 'equalp))


(defun add-symbol-with-type (type symbol)
  (let ((symbols (gethash type *symbol-types*)))
    (if (not (member symbol symbols))
        (setf (gethash type *symbol-types*) (cons symbol symbols)))))
```

这里唯一要注意的就是, 要把`hash-table`的比较函数设置成`equalp`, 因为聪明先生过目不忘, [007 挑剔先生给出终止迭代的条件](/posts/007-recursive-eq/) 对比较说得可清楚啦!

这样下来, 就可以把所有的符号都放到`hash-table`里面了.

```lisp
(let ((tfn "temp-files/all-external-symbols.md"))
  (el:export-all-external-symbols :cl :fn tfn)
  (with-open-file (fn tfn)
    ;; read line by line
    (loop for line = (read-line fn nil)
          while line
          do (when (and (search " names a " line) (string-ends-with-p line ":"))
                   (let* ((parts (split-string-by-substring line " names a "))
                          (symbol (first parts))
                          (type-string (second parts))
                          (type-name (trim-string type-string ":")))
                     (add-symbol-with-type type-name symbol)
                     (format t "~a:~A~%" type-name symbol))))))
```

这样, 聪明先生就可以把所有的符号都分类了.

```lisp

(with-open-file (fn "temp-files/symbols.md" :direction :output :if-exists :supersede)

  (loop for key being the hash-keys of *symbol-types*
        do (let* ((symbols-string (gethash key *symbol-types*))
                  (symbols (mapcar #'intern symbols-string)))
             ;; print documents for each symbol
             (format fn "~%## ~a~%~%" key)
             (format fn "#~a" (el:format-descriptions symbols 2)))))
```

这样, 再打开一个文件, 按照每个类型一个二级标题, 把每个类型的符号按照`el:format-descriptions`的格式打印出来.

结果也相当可爱:[Appendix 001: Common Lisp Symbols分类参考](/posts/010-appendix-cl-symbols/). 有一个完整的Common Lisp的符号分类参考.

## 聪明先生的总结

1. 聪明先生非常聪明, 他总是能认识到自己的错误: 造轮子;
2. 聪明先生学习编程序总是把原理和实践结合起来;
3. 聪明先生一忍不住又造了若干个方形的轮子, 每次都是这样! 


