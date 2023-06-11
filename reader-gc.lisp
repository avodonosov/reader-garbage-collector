#|

  ;; Do not pollute the system with symbols interned
  ;; while READ'ing the DEFPACKAGE, without affecting
  ;; human redability, as would happen if we
  ;; prepended #: to every symbol.
  (cl:defpackage #:cl+ssl-reader-garbage (:use))
  (cl:in-package #:cl+ssl-reader-garbage)


  ;; reader GC
  (cl:in-package #:cl-user)
  (eval-when (:load-toplevel :compile-toplevel :execute)
    (delete-package '#:cl+ssl-reader-garbage)))

|#

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
