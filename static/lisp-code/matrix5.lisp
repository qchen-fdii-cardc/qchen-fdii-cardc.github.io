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
  (if (< (random 10) 1) ; 20% chance to return a space
      #\Space
      (code-char (+ 33 (random 94))))) ; Characters from '!' to '~'

(defun matrix-effect (width height)
  "Simulate the Matrix effect with green characters falling down the terminal."
  (clear-screen)
  (set-green-color)
  (loop
 with columns = (make-array width :initial-element 0)
 with buffer = (make-array (list height width) :initial-element #\Space)
 with prev-buffer = (make-array (list height width) :initial-element #\Space)
 for i from 0
 do (progn
     ;; Update buffer with new characters
     (loop for col from 0 below width do
             (when (zerop (random 10))
                   (setf (aref columns col) (random height)))
             (let ((row (aref columns col)))
               (setf (aref buffer row col) (random-char))
               (incf (aref columns col))
               (when (>= (aref columns col) height)
                     (setf (aref columns col) 0))))
     ;; Draw only the changed parts of the buffer
     (loop for row from 0 below height do
             (loop for col from 0 below width do
                     (unless (char= (aref buffer row col) (aref prev-buffer row col))
                       (move-cursor row col)
                       (format t "~c " (aref buffer row col)))))
     ;; Copy buffer to prev-buffer
     (loop for row from 0 below height do
             (loop for col from 0 below width do
                     (setf (aref prev-buffer row col) (aref buffer row col))))
     (sleep 0.001) ; Reduce sleep time for smoother effect
     (finish-output)))
  (reset-color))

;; Example usage:
;; (matrix-effect 40 20)