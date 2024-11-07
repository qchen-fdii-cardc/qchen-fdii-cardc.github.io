(ql:quickload :1am)
(ql:quickload :explore-lisp)

(defpackage eight-queens
  (:use :cl :1am :explore-lisp))


(in-package :eight-queens)


(setf *tests* nil)

(defvar *board-size* 8)
(defvar *board* (make-array (list *board-size* *board-size*) :initial-element nil))

(defun reset-board ()
  (setf *board* (make-array (list *board-size* *board-size*) :initial-element nil)))


(defun print-board (&optional (stream t))
  "print out results"
  (format stream "~%")
  (loop for i from 0 to (1- (array-dimension *board* 0)) do
          (format stream "~{~a~^~}~%" (loop for j from 0 to (1- (array-dimension *board* 1)) collect (if (aref *board* i j) "⚫" "⚪")))))


(defun place-queen (row col)
  (setf (aref *board* row col) t))

(defun remove-queen (row col)
  (setf (aref *board* row col) nil))

(defun at (row col)
  (if (and (>= row 0) (>= col 0) (< row *board-size*) (< col *board-size*))
      (aref *board* row col)
      nil))

(defun queen-in-row-p (row)
  (loop for i from 0 to (1- *board-size*) thereis (at row i)))

(defun queen-in-col-p (col)
  (loop for i from 0 to (1- *board-size*) thereis (at i col)))


(defun closest-distance-to-top-left (row col)
  (min row col))

(defun closest-distance-to-bottom-right (row col)
  (min (- *board-size* row) (- *board-size* col)))

(defun closest-distance-to-top-right (row col)
  (min row (- *board-size* col)))

(defun closest-distance-to-bottom-left (row col)
  (min (- *board-size* row) col))

(defun plus-line-1 (row col)
" +1, +1"
  (loop for i from 1 to (closest-distance-to-bottom-right row col) thereis (at (+ row i) (+ col i))))

(defun minus-line-1 (row col)
  "-1, -1"
  (loop for i from 1 to (closest-distance-to-top-left row col) thereis (at (- row i) (- col i))))

(defun line-1-p (row col)
  (or (plus-line-1 row col) (minus-line-1 row col)))

(defun plus-line-2 (row col)
  "-1, +1"
  (loop for i from 1 to (closest-distance-to-top-right row col) thereis (at (- row i) (+ col i))))

(defun minus-line-2 (row col)
  "+1, -1"
  (loop for i from 1 to (closest-distance-to-bottom-left row col) thereis (at (+ row i) (- col i))))

(defun line-2-p (row col)
  (or (plus-line-2 row col) (minus-line-2 row col)))

(defun safe-to-place-p (row col)
  (and (not (queen-in-row-p row))
       (not (queen-in-col-p col))
       (not (line-1-p row col))
       (not (line-2-p row col))))


(defun place-queens-greedy (row &optional (stream t))
  (if (= row *board-size*)
      (progn (print-board stream)
             ; flush stream
             ;  ()(error "Find first solution")
             )
      ; (print-board stream)
      (loop for col from 0 to (1- *board-size*) do
              (if (safe-to-place-p row col)
                  (progn
                   (place-queen row col)
                   (place-queens-greedy (1+ row) stream)
                   (remove-queen row col))))))


(with-open-file (fn #P"x.txt" :direction :output :if-exists :supersede)
  (reset-board)
  (place-queens-greedy 0 fn))


(progn
 (reset-board)
 (place-queens-greedy 0))
