(require '1am)

(defpackage sequence-tutor
  (:use :1am :cl))


(in-package sequence-tutor)

(setf *tests* nil)

;; make-sequence

(test make-sequence-examples
      (is (equal (make-sequence 'list 3) '(nil nil nil))) ; list
      (is (equalp (make-sequence 'vector 3) #(0 0 0))) ; vector
      (is (string= (make-sequence 'string 3) (make-string 3 :initial-element #\Nul))) ; string
      )

(make-sequence-examples)
;; concatenate

(test concatenate-examples
      (is (equal (concatenate 'list '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))) ; list
      (is (equal (concatenate 'list '(1 2 3) #(4 5 6)) '(1 2 3 4 5 6))) ; list and vector

      ; to see how list is concatenated without copying
      (let* ((s1 '(1 2 3))
             (s2 '(4 5 6))
             (s12 (concatenate 'list s1 s2)))
        (is (eq (nth 0 s1) (nth 0 s12))) ; s1 is not copied
        (is (eq (nth 0 s2) (nth 3 s12))) ; s2 is not copied       
        ; s12 is a new list with the elements of s1 and s2
        (is (equal s12 '(1 2 3 4 5 6))))

      ; not good at all 
      (is (equalp (concatenate 'vector #(1 2 3) #(4 5 6)) #(1 2 3 4 5 6))) ; vector
      (is (equalp (concatenate 'vector '(1 2 3) #(4 5 6)) #(1 2 3 4 5 6))) ; list and vector
      (is (equal (concatenate 'list "abc" "def") '(#\a #\b #\c #\d #\e #\f))) ; string to list
      (is (equalp (concatenate 'vector "abc" "def") #(#\a #\b #\c #\d #\e #\f))) ; string to vector
      (is (equal (concatenate 'string "abc" "def") "abcdef"))) ; string to string

(concatenate-examples)


;; copy-seq

(test copy-seq-examples
      ; 返回一个拷贝，不是原对象，不是深拷贝
      (is (equal (copy-seq "abc") "abc")) ; string
      (is (equal (copy-seq '(1 2 3)) '(1 2 3))) ; list
      (is (equalp (copy-seq #(1 2 3)) #(1 2 3)))
      ; vector(simple-vector) breaks the protocol of copy-seq, 
      ;array other than string/bit-vectors must be eq to be equal
      (is (equalp (copy-seq '(#(2 1) #(2 3))) '(#(2 1) #(2 3)))) ; list of vectors

      (let* ((l1 (list #(1 2) #(3 4)))
             (l2 (copy-seq l1))) ; shallow copy
        (is (equal l1 l2))
        (is (eq (first l1) (first l2)))))

(copy-seq-examples)


(run)