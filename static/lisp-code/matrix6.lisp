; Hmm... Let's think about how to modify the code to make the characters form three vertical strips instead of falling down in a single column. We can achieve this by dividing the terminal width into three equal parts and making each part act as an independent strip where characters fall.

; To implement this, we need to:
; 1. Divide the width into three equal parts.
; 2. Create separate columns and buffers for each strip.
; 3. Update and draw each strip independently.

; Let's update the code to implement these changes.


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
  (if (< (random 10) 1) ; 10% chance to return a space
      #\Space
      (code-char (+ 33 (random 94))))) ; Characters from '!' to '~'

(defun matrix-effect (width height)
  "Simulate the Matrix effect with green characters falling down the terminal in three vertical strips."
  (clear-screen)
  (set-green-color)
  (let ((strip-width (/ width 3)))
    (loop
   with columns1 = (make-array strip-width :initial-element 0)
   with buffer1 = (make-array (list height strip-width) :initial-element #\Space)
   with prev-buffer1 = (make-array (list height strip-width) :initial-element #\Space)
   with columns2 = (make-array strip-width :initial-element 0)
   with buffer2 = (make-array (list height strip-width) :initial-element #\Space)
   with prev-buffer2 = (make-array (list height strip-width) :initial-element #\Space)
   with columns3 = (make-array strip-width :initial-element 0)
   with buffer3 = (make-array (list height strip-width) :initial-element #\Space)
   with prev-buffer3 = (make-array (list height strip-width) :initial-element #\Space)
   for i from 0
   do (progn
       ;; Update buffer with new characters for each strip
       (loop for col from 0 below strip-width do
               (when (zerop (random 10))
                     (setf (aref columns1 col) (random height)))
               (let ((row (aref columns1 col)))
                 (setf (aref buffer1 row col) (random-char))
                 (incf (aref columns1 col))
                 (when (>= (aref columns1 col) height)
                       (setf (aref columns1 col) 0))))
       (loop for col from 0 below strip-width do
               (when (zerop (random 10))
                     (setf (aref columns2 col) (random height)))
               (let ((row (aref columns2 col)))
                 (setf (aref buffer2 row col) (random-char))
                 (incf (aref columns2 col))
                 (when (>= (aref columns2 col) height)
                       (setf (aref columns2 col) 0))))
       (loop for col from 0 below strip-width do
               (when (zerop (random 10))
                     (setf (aref columns3 col) (random height)))
               (let ((row (aref columns3 col)))
                 (setf (aref buffer3 row col) (random-char))
                 (incf (aref columns3 col))
                 (when (>= (aref columns3 col) height)
                       (setf (aref columns3 col) 0))))
       ;; Draw only the changed parts of the buffer for each strip
       (loop for row from 0 below height do
               (loop for col from 0 below strip-width do
                       (unless (char= (aref buffer1 row col) (aref prev-buffer1 row col))
                         (move-cursor row col)
                         (format t "~c " (aref buffer1 row col)))))
       (loop for row from 0 below height do
               (loop for col from 0 below strip-width do
                       (unless (char= (aref buffer2 row col) (aref prev-buffer2 row col))
                         (move-cursor row (+ col strip-width))
                         (format t "~c " (aref buffer2 row col)))))
       (loop for row from 0 below height do
               (loop for col from 0 below strip-width do
                       (unless (char= (aref buffer3 row col) (aref prev-buffer3 row col))
                         (move-cursor row (+ col (* 2 strip-width)))
                         (format t "~c " (aref buffer3 row col)))))
       ;; Copy buffer to prev-buffer for each strip
       (loop for row from 0 below height do
               (loop for col from 0 below strip-width do
                       (setf (aref prev-buffer1 row col) (aref buffer1 row col))))
       (loop for row from 0 below height do
               (loop for col from 0 below strip-width do
                       (setf (aref prev-buffer2 row col) (aref buffer2 row col))))
       (loop for row from 0 below height do
               (loop for col from 0 below strip-width do
                       (setf (aref prev-buffer3 row col) (aref buffer3 row col))))
       (sleep 0.001) ; Reduce sleep time for smoother effect
       (finish-output))))
  (reset-color))

;; Example usage:
;; (matrix-effect 120 20)