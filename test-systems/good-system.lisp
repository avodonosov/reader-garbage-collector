(defpackage #:good-system ;; temporarily made it an uninterned symbol, to test issue #2 "ABCL failure"
  (:use cl)
  (:export good-func-1
           good-func-2
           good-var))
