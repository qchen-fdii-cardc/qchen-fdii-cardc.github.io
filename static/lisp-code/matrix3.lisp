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
  "Generate a random character with a higher probability of being a space."
  (if (< (random 10) 3) ; 30% chance to return a space
      #\Space
      (code-char (+ 33 (random 94))))) ; Characters from '!' to '~'

(defun matrix-effect (width height)
  "Simulate the Matrix effect with green characters falling down the terminal."
  (clear-screen)
  (set-green-color)
  (loop
 with columns = (make-array width :initial-element 0)
 with buffer = (make-array (list height width) :initial-element #\Space)
 for i from 0
 do (progn
     ;; Update buffer with new characters
     (loop for col from 0 below width do
             (when (zerop (random 10))
                   (setf (aref columns col) (random height)))
             (setf (aref buffer (aref columns col) col) (random-char))
             (incf (aref columns col))
             (when (>= (aref columns col) height)
                   (setf (aref columns col) 0)))
     ;; Clear screen and draw buffer
     (clear-screen)
     (loop for row from 0 below height do
             (move-cursor row 0)
             (loop for col from 0 below width do
                     (format t "~c " (aref buffer row col))))
     (sleep 0.5)
     (finish-output)))
  (reset-color))

;; Example usage:
;; (matrix-effect 40 20)