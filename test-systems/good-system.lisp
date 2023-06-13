(uiop:define-package #:good-system ;; temporarily switched to uiop:define-package and uninterned symbol, to test issue #2 "ABCL failure"
  (:use cl)
  (:export good-func-1
           good-func-2
           good-var))
