(in-package :cl-user)

(require :asdf)

(let* ((base-dir (or (and *load-truename*
                          (make-pathname :directory (pathname-directory *load-truename*)))
                     #P"/home/anton/my/prj/reader-garbage-collector/"))
       (asdf:*central-registry* (append (list base-dir
                                              (merge-pathnames "test-systems/"
                                                               base-dir))
                                        asdf:*central-registry*)))
  (asdf:load-system "good-system" :force t)
  (asdf:load-system "bad-system" :force t)
  )

(let ((test-results
        (list (cons "good-system successfully defines its symbol"
                    (find-symbol (string'#:good-func-1) '#:good-system))
              (cons "good-system does not pollute the CL-USER package"
                    (not (find-symbol (string'#:good-func-1) :cl-user)))
              (cons "bad-system successfully defines its symbol"
                    (find-symbol (string'#:bad-func-1) '#:bad-system))
              (cons "bad-system does pollute Cl-USER"
                    (find-symbol (string'#:bad-func-1) :cl-user)))))
  (defparameter *fail-count*
    (loop for (descr . passed-p) in test-results
          counting (not passed-p) into fail-count
          do (unless passed-p
               (format t "Failed expectation: ~A~%" descr))
          finally (return fail-count))))

(when (> *fail-count* 0)
  (format t "Total failures: ~A~%" *fail-count*)
  (uiop:quit 1))

(format t "All passed.")
(uiop:quit 0)
