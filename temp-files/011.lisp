(describe 'multiple-value-call)


(multiple-value-call #'list 1 2 3 4 5)
;; => (1 2 3 4 5)

(multiple-value-call #'+ (values 1 2 3) 2 3)
;; => 11

(+ (values 1 2 3) 2 3)
;; => 6

(multiple-value-call #'+ 1 2 3)
;; => 6

(apply #'+ '(1 2 3))
;; => 6

(describe 'apply)

(apply #'+ (list 1 2 3))

(apply #'+ (list 1 2 3 4 5))


(coerce 'if `function)


(multiple-value-call #'+ (values 3 4) 2 1 (values 8 7 3))
