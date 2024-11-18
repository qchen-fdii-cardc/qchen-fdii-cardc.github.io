(defun clear-screen ()
  "Clear the terminal screen using ANSI escape codes."
  (format t "~c[2J~c[H" #\Escape #\Escape))

(defun move-cursor (row col)
  "Move the cursor to the specified position using ANSI escape codes."
  (format t "~c[~d;~dH" #\Escape row col))

(defun set-green-color ()
  "Set the text color to green using ANSI escape codes."
  (format t "~c[32m" #\Escape))

(defun reset-color ()
  "Reset the text color to default using ANSI escape codes."
  (format t "~c[0m" #\Escape))

(defun random-char ()
  "Generate a random character."
  (code-char (+ 33 (random 94)))) ; Characters from '!' to '~'

(defun matrix-effect (width height)
  "Simulate the Matrix effect with green characters falling down the terminal."
  (clear-screen)
  (set-green-color)
  (loop
 with columns = (make-array width :initial-element 0)
 for i from 0
 do (progn
     (loop for col from 0 below width do
             (when (zerop (random 10))
                   (setf (aref columns col) (random height)))
             (move-cursor (aref columns col) col)
             (format t "~c" (random-char))
             (incf (aref columns col))
             (when (>= (aref columns col) height)
                   (setf (aref columns col) 0))))
   (sleep 0.001)
   (finish-output))
  (reset-color))

;; Example usage:
;; (matrix-effect 80 24)