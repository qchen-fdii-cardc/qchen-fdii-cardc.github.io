(macrolet ((whenlet (test then &optional else)
                    `(if ,test
                         ,then
                         ,@(when else (list else)))))
  (whenlet (numberp 10) (print "It's a number")
           (print "This won't be printed")))