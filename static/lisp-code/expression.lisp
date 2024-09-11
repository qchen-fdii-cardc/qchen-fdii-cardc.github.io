;;;; xiaoxiao-expression.lisp
;; 表达式1
(defpackage :xiaoxiao-expression
  (:nicknames :xx :xiaoxiao)
  (:use :cl :explore-lisp))

;; 表达式2
(in-package :xiaoxiao-expression)

;; 表达式3
(defun hello-xiaoxiao ()
  (format t "Hello, Xiaoxiao!~%"))

; https://windtunnel.cn/posts/001-rude-start-application/
; https://windtunnel.cn/posts/002-lazy-process/
; https://windtunnel.cn/posts/003-upsidedown-infix/
; https://windtunnel.cn/posts/004-perfect.md/
; https://windtunnel.cn/posts/005-explore-lisp/
; https://windtunnel.cn/posts/006-sequence-in-lisp/
; https://windtunnel.cn/posts/007-recursive-eq/
; https://windtunnel.cn/posts/008-expression/


(require 'explore-lisp)

(el:search-symbols "stop" 'hunchentoot)

(el:export-all-external-symbols 'hunchentoot)


;; start hunchentoot server with public as root
(defun start-server ()
  (start (make-instance 'easy-acceptor :port 8000 :document-root "E:/lisp-writing/qchen-fdii-cardc.github.io/public")))

;; stop hunchentoot server
(start-server)
