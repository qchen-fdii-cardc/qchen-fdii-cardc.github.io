## 乐观先生

## 


```lisp
(defun twofuncs (x)
(list (function (lambda (y) (+ x y)))
(function (lambda (y) (- x y)))
(function (lambda (y) (* x y)))))
```