(defun values-func ()
  (values 1 2 3))


(values-func)

(multiple-value-bind (a b c) (values-func)
  (format t "a: ~a, b: ~a, c: ~a~%" a b c))