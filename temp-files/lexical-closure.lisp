(defun twofuncs (x)
  (list (function (lambda (y) (+ x y)))
        (function (lambda (y) (- x y)))
        (function (lambda (y) (* x y)))))


(defparameter *f* (twofuncs 2))

(funcall (first *f*) 3)
;; => 5

(funcall (second *f*) 3)
;; => -1

(funcall (third *f*) 3)
;; => 6

(defparameter *fs2* (let ((x 10))
                      (list (lambda (y) (+ x y))
                            (lambda (y) (- x y))
                            (lambda (y) (* x y)))))

(funcall (first *fs2*) 3)
;; => 13

(funcall (second *fs2*) 3)
;; => 7

(funcall (third *fs2*) 3)
;; => 30

(ql:quickload :explore-lisp);;:verbose verbose :silent silent :prompt prompt :explain explain)
(require 'explore-lisp)

(el:search-symbols "environment" :cl :doc-string t)
(describe '&environment)

(macroexpand '(defun foo (x) (+ x 1)))
;; (PROGN
;;  (EVAL-WHEN (COMPILE-TOPLEVEL) (%COMPILER-DEFUN 'FOO T NIL NIL))
;;  (%DEFUN 'FOO
;;          (NAMED-LAMBDA FOO
;;                        (X)
;;                        (BLOCK FOO (+ X 1)))))

(macroexpand-1 '(defun foo (x) (+ x 1)))
;; (PROGN
;;  (EVAL-WHEN (COMPILE-TOPLEVEL) (%COMPILER-DEFUN 'FOO T NIL NIL))
;;  (%DEFUN 'FOO
;;          (NAMED-LAMBDA FOO
;;              (X)
;;            (BLOCK FOO (+ X 1)))))