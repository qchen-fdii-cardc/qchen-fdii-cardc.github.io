;; load el package
(ql:quickload `explore-lisp)
(require 'explore-lisp)

(ql:quickload 'temporary-file)
(require 'temporary-file)


;;; string functions
(defun string-ends-with-p (str ending)
  "Return t if str ends with ending."
  (let ((elength (length ending))
        (slength (length str)))
    (if (>= slength elength)
        (string= (subseq str (- slength elength)) ending)
        nil)))

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

(defun not-start-with-p (str start)
  "Return t if str does not start with start."
  (let ((slen (length start))
        (strlen (length str)))
    (if (>= strlen slen)
        (not (equalp (subseq str 0 slen) start))
        t)))

(defun trim-string (str &optional (chars " \t\n"))
  (let ((start (position-if (complement (lambda (c) (find c chars))) str))
        (end (position-if (complement (lambda (c) (find c chars))) (reverse str))))
    (if (and start end)
        (subseq str start (- (length str) end))
        "")))


(let ((tfn "temp-files/all-external-symbols.md"))
  (el:export-all-external-symbols :cl :fn tfn)
  (with-open-file (fn tfn)
    ;; read line by line
    (loop for line = (read-line fn nil)
          while line
          do (when (and (search " names a " line) (string-ends-with-p line ":") (not-start-with-p line "(setf "))
                   (let* ((parts (split-string-by-substring line " names a "))
                          (symbol (first parts))
                          (type-string (second parts))
                          (type-name (trim-string type-string ":")))
                     (add-symbol-with-type type-name symbol)
                     (format t "~a:~A~%" type-name symbol))))))

;;; make type-symbols hash table
(defparameter *symbol-types* (make-hash-table :test 'equalp))

(defun add-symbol-with-type (type symbol)
  (let ((symbols (gethash type *symbol-types*)))
    (if (not (member symbol symbols))
        (setf (gethash type *symbol-types*) (cons symbol symbols)))))

(temporary-file:with-open-temporary-file (s :direction :io :if-exists :supersede)
  (el:export-all-external-symbols-to-stream :cl s)
  (finish-output s)
  (file-position s 0)
  (loop for line = (read-line s nil)
        while line
        do (when (and (search " names a " line) (string-ends-with-p line ":") (not-start-with-p line "(setf "))
                 (let* ((parts (split-string-by-substring line " names a "))
                        (symbol (first parts))
                        (type-string (second parts))
                        (type-name (trim-string type-string ":")))
                   (add-symbol-with-type type-name symbol)
                   (format t "~a:~A~%" type-name symbol)))))


(with-open-file (fn "symbols.md" :direction :output :if-exists :supersede)
  (loop for key being the hash-keys of *symbol-types*
        do (let* ((symbols-string (gethash key *symbol-types*))
                  (symbols (nreverse (mapcar #'intern symbols-string))))
             ;; print documents for each symbol
             (format fn "~%## ~a~%~%" key)
             (format fn "#~a" (el:format-descriptions symbols 2)))))


;;; functions

;; function special form
(setf func (function (lambda (x) (+ x 1))))
(func 1)