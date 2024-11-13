(ql:quickload :1am)
(ql:quickload :explore-lisp)

(defpackage eight-queens
  (:nicknames :8q :queens)
  (:use :cl :1am :explore-lisp)
  (:export :solve-eight-queens
           :print-board
           :get-solution
           :set-solution))


(in-package :eight-queens)


(setf *tests* nil)

(defvar *board-size* 8)
(defvar *board* (make-array (list *board-size* *board-size*) :initial-element nil))
(defvar *solutions* nil)

(defun at (row col)
  (if (and (>= row 0) (>= col 0) (< row *board-size*) (< col *board-size*))
      (aref *board* row col)
      nil))

;; define a setf function for the board
(defun (setf at) (value row col)
  (when (and (>= row 0) (>= col 0) (< row *board-size*) (< col *board-size*))
        (setf (aref *board* row col) value)))
(defun place-queen (row col)
  (setf (at row col) t))

(defun remove-queen (row col)
  (setf (at row col) nil))


(defun reset-board ()
  (setf *board* (make-array (list *board-size* *board-size*) :initial-element nil)))

(defun set-board-size (size)
  (setf *board-size* size)
  (reset-board)
  (setf *solutions* nil))

(defun get-solution ()
  "represent the solution as a list of column numbers"
  (loop for i from 0 to (1- *board-size*) collect (loop for j from 0 to (1- *board-size*) thereis (if (aref *board* i j) j nil))))

(defun set-solution (solution)
  (setf *board-size* (length solution))
  (loop for i from 0 to (1- *board-size*) do
          (place-queen i (nth i solution))))

(defun print-board (&optional (stream t))
  "print out results"
  (format stream "~%")
  (loop for i from 0 to (1- (array-dimension *board* 0)) do
          (format stream "~{~a~^~}~%" (loop for j from 0 to (1- (array-dimension *board* 1)) collect (if (aref *board* i j) "⚫" "⚪")))))

;; test queen placement for safty
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


;; greedy algorithm to place queens
(defun place-queens-greedy (row &optional (stream t))
  (if (= row *board-size*)
      (progn
       (print-board stream)
       (push (get-solution) *solutions*))

      (loop for col from 0 to (1- *board-size*) do
              (if (safe-to-place-p row col)
                  (progn
                   (place-queen row col)
                   (place-queens-greedy (1+ row) stream)
                   (remove-queen row col))))))

;; solve the eight queens problem
(defun solve-eight-queens (&optional (size 8))
  (set-board-size size)
  (let ((s (with-output-to-string (stream)
             (place-queens-greedy 0 stream))))
    (values (length *solutions*) (nreverse *solutions*) s)))


; (multiple-value-list (solve-eight-queens 5))