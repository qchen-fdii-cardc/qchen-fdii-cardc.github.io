(require 'explore-lisp)


(sort (el:search-symbols "sequence" :cl :doc-string t) #'string<)


; (BASE-STRING BIT-VECTOR CONCATENATE CONS COPY-SEQ COUNT COUNT-IF
;  COUNT-IF-NOT DELETE DELETE-DUPLICATES DELETE-IF DELETE-IF-NOT ELT EVERY FILL
;  FIND FIND-IF FIND-IF-NOT LENGTH LIST MAKE-SEQUENCE MAP MAP-INTO MAPHASH MERGE
;  MISMATCH NOTANY NOTEVERY NREVERSE NSUBSTITUTE NSUBSTITUTE-IF
;  NSUBSTITUTE-IF-NOT NULL POSITION POSITION-IF POSITION-IF-NOT PROGRAM-ERROR
;  READ-SEQUENCE REDUCE REMOVE REMOVE-DUPLICATES REMOVE-IF REMOVE-IF-NOT REPLACE
;  REVERSE SEARCH SEQUENCE SIMPLE-BASE-STRING SIMPLE-BIT-VECTOR SIMPLE-CONDITION
;  SIMPLE-STRING SIMPLE-VECTOR SOME SORT STABLE-SORT STRING STRING-LEFT-TRIM
;  STRING-RIGHT-TRIM STRING-TRIM SUBSEQ SUBSTITUTE SUBSTITUTE-IF
;  SUBSTITUTE-IF-NOT T THE TYPE-ERROR VECTOR WITH-HASH-TABLE-ITERATOR
;  WRITE-SEQUENCE)


(search "built-in-class" (el:describe-symbol 'base-string))

(let* ((seq (sort (el:search-symbols "sequence" :cl :doc-string t) #'string<))

       (types '("built-in-class" "function" "macro" "special-operator")))
  (loop for type in types
        do (loop for sym in seq
                 do (when (search type (el:describe-symbol sym))
                          (format t "~A: ~A~%" type sym)))))
; (mapcar (lambda (type)
;           (mapcar (lambda (sym)
;                     (when (search type (el:describe-symbol sym))
;                           (format nil "~A: ~A~%" type sym)))
;               seq))
;     types))
