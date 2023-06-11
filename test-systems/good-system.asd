(defsystem "good-system"
  :depends-on ("reader-garbage-collector")
  :around-compile "reader-gc:call-with-garbage-package"
  :components ((:file "good-system")))
