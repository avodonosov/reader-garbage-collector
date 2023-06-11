(defpackage #:reader-gc
  (:export #:call-with-garbage-package
            #:call-with-garbage-package-named)
  (:use #:cl))

(in-package #:reader-gc)

(defun call-with-garbage-package-named (pkg-name body-fn)
  (let* ((pkg (or (find-package pkg-name)
                  ;; TODO: warn if the package exists?
                  (make-package pkg-name :use :cl))))
    (do-external-symbols (sym 'cl)
      (export sym pkg))
    
    (let ((*package* pkg))
      (funcall body-fn))
    
    (delete-package pkg)
    ))

(defun call-with-garbage-package (body-fn)
  (call-with-garbage-package-named '#:default-reader-garbage-package
                                   body-fn))
