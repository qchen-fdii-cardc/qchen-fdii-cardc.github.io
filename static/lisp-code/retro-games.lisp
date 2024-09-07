(ql:quickload '(:hunchentoot :cl-who :parenscript :trivial-open-browser :usocket))

(defpackage :retro-games
  (:nicknames :rg)
  (:use :cl :hunchentoot :cl-who :parenscript)
  (:export :main))


(in-package :retro-games)

(defclass game ()
    ((name :initarg :name
           :reader name)
     (votes :initform 0
            :accessor votes)))

(defmethod vote-for (user-selected-game)
  (incf (votes user-selected-game)))

(defmethod print-object ((object game) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name votes) object
      (format stream "name: ~s with ~d votes" name votes))))

(defvar *games* '())

(defun game-from-name (name)
  (find name *games* :key #'name :test #'string-equal))

(defun game-stored-p (name)
  (game-from-name name))

(defun games ()
  (sort (copy-list *games*) #'> :key #'votes))

(defun add-game (name)
  (unless (game-stored-p name)
    (push (make-instance 'game :name name) *games*)))


(mapcar #'add-game '("魔兽世界" "魔兽争霸" "魔兽争霸2" "魔兽争霸3" "风暴英雄"))

;; examples begin

(game-from-name "魔兽世界")

;; examples end

(setf (html-mode) :html5)

(defmacro standard-page ((&key title) &body body)
  `(with-html-output-to-string
       (*standard-output* nil :prologue t :indent t)
     (:html :lang "en"
            (:head
             (:meta :charset "utf-8")
             (:title ,title)
             (:link :type "text/css"
                    :rel "stylesheet"
                    :href "/retro.css"))
            (:body
             (:div :id "header" ; Retro games header
                   (:img :src "/logo.png"
                         :alt "Comodore 64"
                         :class "logo")
                   (:span :class "strapline"
                          "Vote on your favarourite retro games"))
             ,@body))))

;; begin example

(standard-page
    (:title "Page 1"))

(standard-page
    (:title "Page 2")
  (:h1 "Hello World"))


;; end example


(defvar *hunchentoot-directory*
        (pathname (directory-namestring #.(or *compile-file-pathname* *load-truename*))))

(defvar *server*
        (make-instance 'easy-acceptor
          :port 8080
          :document-root *hunchentoot-directory*))

(defun start-server ()
  (start *server*))

(defun stop-server ()
  (stop *server* :soft nil))


(define-easy-handler (retro-games :uri "/retro-games") ()
  (standard-page
      (:title "Top Tetro Games")
    (:h1 "Vote on your all time favorite retro games")
    (:p "Missiong a game? Make it available for votes"
        (:a :href "new-game" "here"))
    (:h2 "Current stand")
    (:div :id "chart"
          (:ol
           (dolist (game (games))
             (htm
              (:li (:a :href (format nil "vote?name=~a" (name game)) "Vote!")
                   (fmt "~A with ~d votes" (escape-string (name game))
                        (votes game)))))))))


(define-easy-handler (vote :uri "/vote") (name)
  (when (game-stored-p name)
        (vote-for (game-from-name name)))
  (redirect "/retro-games"))


(define-easy-handler (new-game :uri "/new-game") ()
  (standard-page
      (:title "Add a new game")
    (:h1 "Add a new game to the chart")
    (:form :action "game-added" :method "post" :id "addform"
           (:p "What is the name of the game?" (:br))
           (:input :type "text" :name "name" :class "txt")
           (:p
            (:input :type "submit" :value "Add" :class "btn")))))

(define-easy-handler (game-added :uri "/game-added") (name)
  (unless (or (null name) (zerop (length name)))
    (add-game name))
  (redirect "/retro-games"))

(require 'uiop)

; run open browser by ignoring the error
(defun open-browser (url)
  (ignore-errors
    (trivial-open-browser:open-browser url)))


(defun wait-till-quit ()
  (sb-thread:join-thread (find-if
                             (lambda (th)
                               (search "hunchentoot-listener-" (sb-thread:thread-name th)))
                             (sb-thread:list-all-threads))))


(defun kill-server-thread ()
  (sb-thread:terminate-thread (find-if
                                  (lambda (th)
                                    (search "hunchentoot-listener-" (sb-thread:thread-name th)))
                                  (sb-thread:list-all-threads))))

(defun main ()
  (start-server)
  (open-browser "http://localhost:8080/retro-games")
  ;; handle break signal
  (handler-case (wait-till-quit)
    (sb-sys:interactive-interrupt ()
                                  (format t "Try Stop the Server.~%")
                                  (stop-server)
                                  (when (started-p *server*)
                                        (kill-server-thread)
                                        (format t "Server Stopped.~%")))))


; I have the same problem when using this gorgeous package on Windows 11 for SBCL 2.4.6.

; However, I can permanently terminate the process or press C-c many times to exit.

; It's not perfect in **Windows**, so I blame Windows for now.