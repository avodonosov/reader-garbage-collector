(flet ((format-test-step (lisp &optional other-vars)
         (format t "      - run: |~%")
         (format t "           ~@[~A ~]LISP=~A docker-home/reader-garbage-collector/.github/workflows/test.sh~%"
                 other-vars lisp)
         (format t "        timeout-minutes: 1~%")
         (format t "        if: success() || failure()~%")
         )
       (format-retrying-test-step (lisp &optional other-vars)
         ;; Note the `<  /dev/null` at the end of the cmd-line.
         ;; This is needed to prevent CCL to hang waiting
         ;; for user input when CCL Kernel Debugger is entered
         ;; upon unhandled exception.
         ;; The standard Guthub Actions `run` step closes the
         ;; stdin of the child shell process automatically.
         ;; But the nick-fields/retry step keeps it open,
         ;; so we need this workaround.
         ;; Reported this as a bug: https://github.com/nick-fields/retry/issues/98
         (let ((cmd-line (format nil "~@[~A ~]LISP=~A docker-home/reader-garbage-collector/.github/workflows/test.sh < /dev/null"
                                 other-vars lisp)))
           (format t "      - uses: nick-fields/retry@v2.8.2~%")
           (format t "        name: Run with retries ~A~%" cmd-line)
           (format t "        with:~%")
           (format t "          command: |~%")
           (format t "             ~A~%" cmd-line)
           (format t "          timeout_minutes: 1~%")
           (format t "          max_attempts: 3~%")
           ;; don't hide timeouts
           (format t "          retry_on: error~%")
           ;; don't hide error situations other than the known crashes
           (format t "          retry_on_exit_code: 137~%")
           (format t "        if: success() || failure()~%"))))
  (dolist (lisp '("sbcl" "ccl" "abcl" "clisp"))
    (if (string= lisp "ccl")
        ;; because of https://github.com/Clozure/ccl/issues/85
        (format-retrying-test-step lisp)
        (format-test-step lisp))))

