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
  (if (zerop (random 5)) ; 1/20 chance to return a space
      #\Space
      (code-char (+ 33 (random 94))))) ; Characters from '!' to '~'

(defun copy-2d-array (src target w h)
  "Copy the contents of a 2D array to another 2D array."
  (loop for row from 0 below h do
          (loop for col from 0 below w do
                  (setf (aref target row col) (aref src row col)))))


(defun update-with-changed-chars (buffer prev-buffer width height)
  "Update the terminal with only the changed characters."
  (loop for row from 0 below height do
          (loop for col from 0 below width do
                  (unless (char= (aref buffer row col) (aref prev-buffer row col))
                    (move-cursor row col)
                    (format t "~c " (aref buffer row col))))))

(defun random-2d-array-with-columns (buffer columns width height)
  (loop for col from 0 below width do
          (when (zerop (random 10)) ; 1/10 chance to start a new character
                (setf (aref columns col) (random height)))
          (setf (aref buffer (aref columns col) col) (random-char))
          (incf (aref columns col))
          (when (>= (aref columns col) height)
                (setf (aref columns col) 0))))

(defun matrix-effect (width height delay-time)
  "Simulate the Matrix effect with green characters falling down the terminal."
  (clear-screen)
  (set-green-color)
  (loop
 with columns = (make-array width :initial-element 0)
 with buffer = (make-array (list height width) :initial-element #\Space)
 with prev-buffer = (make-array (list height width) :initial-element #\Space)
 for i from 0
 do (progn
     ; Update buffer with new characters
     (random-2d-array-with-columns buffer columns width height)
     ; Draw only the changed parts of the buffer
     (update-with-changed-chars buffer prev-buffer width height)
     ; Copy buffer to prev-buffer
     (copy-2d-array buffer prev-buffer width height)
     ; Sleep for a short delay
     (sleep delay-time)
     ; Force the output to be displayed
     (finish-output))))

; call `tput cols` and `tput lines` to get the terminal size

(defun get-width ()
  "Get the width of the terminal."
  (parse-integer (uiop:run-program '("tput" "cols") :output :string)))

(defun get-height ()
  "Get the height of the terminal."
  (parse-integer (uiop:run-program '("tput" "lines") :output :string)))

(defun run-matrix-effect (&optional (w nil) (h nil) (delay-time 0.05))
  ; dealing with errors and Ctrl+C
  (let ((w (or w (get-width)))
        (h (or h (get-height))))
    (unwind-protect
        (matrix-effect w h delay-time)
      (reset-color))))

;; sbcl --load "matrix4.lisp" --eval "(run-matrix-effect `tput cols` `tput lines`)"