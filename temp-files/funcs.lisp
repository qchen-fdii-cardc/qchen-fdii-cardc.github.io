(defun values-func ()
  (values 1 2 3))


(values-func)

(multiple-value-bind (a b c) (values-func)
  (format t "a: ~a, b: ~a, c: ~a~%" a b c))


(require 'explore-lisp)


(el:export-descriptions (el:search-symbols "special operator" :cl :doc-string t) "temp-files/special-operators.md")


(el:export-descriptions (el:search-symbols "macro form" :cl :doc-string t) "temp-files/macro-form.md")


(el:export-descriptions (el:search-symbols "special form" :cl :doc-string t) "temp-files/special-forms.md")


; special variable
; compiled function
; generic function
; macro
; built-in-class
; constant variable
; primitive type-specifier

(el:export-descriptions (el:search-symbols "compiled function" :cl :doc-string t) "temp-files/compiled-functions.md")


(el:export-all-external-symbols :cl :fn "temp-files/all-external-symbols.md")


(defun string-ends-with-p (str ending)
  "Return t if str ends with ending."
  (let ((elength (length ending))
        (slength (length str)))
    (if (>= slength elength)
        (string= (subseq str (- slength elength)) ending)
        nil)))

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

(split-string-by-substring "a,b,c" ",b,")

(el:search-symbols "unique" :cl :doc-string t)

(el:describe-symbol `push)

(defparameter *symbol-types* (make-hash-table :test 'equalp))

(let ((f "WRITE") (funcs (gethash "compiled function" *symbol-types* '())))
  (when (not (member f funcs))
        (setf (gethash "compiled function" *symbol-types*) (cons f funcs)))
  (format t "~a~%" (hash-table-count *symbol-types*))
  (format t "~a~%" (gethash "compiled function" *symbol-types*)))

(eq (intern "compiled function") (intern "compiled function"))

(defun add-symbol-with-type (type symbol)
  (let ((symbols (gethash type *symbol-types*)))
    (if (not (member symbol symbols))
        (setf (gethash type *symbol-types*) (cons symbol symbols)))))

(defun trim-string (str &optional (chars " \t\n"))
  (let ((start (position-if (complement (lambda (c) (find c chars))) str))
        (end (position-if (complement (lambda (c) (find c chars))) (reverse str))))
    (if (and start end)
        (subseq str start (- (length str) end))
        "")))

(trim-string "string" "g")

; get all keys
(el:search-symbols "hash" :cl :doc-string t)

(with-open-file (fn "temp-files/symbols.md" :direction :output :if-exists :supersede)

  (loop for key being the hash-keys of *symbol-types*
        do (let* ((symbols-string (gethash key *symbol-types*))
                  (symbols (mapcar #'intern symbols-string)))
             ;; print documents for each symbol
             (format fn "~%## ~a~%~%" key)
             (format fn "#~a" (el:format-descriptions symbols 2)))))


(hash-table-size *symbol-types*)


;; define and use a set
(defparameter *symbol-types* (make-hash-table :test 'equal))
