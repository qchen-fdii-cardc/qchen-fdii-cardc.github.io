(load "retro-games.lisp")

(sb-ext:save-lisp-and-die #p"retro-games.exe"
                          :toplevel #'rg:main
                          :executable t)