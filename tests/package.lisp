(in-package #:common-lisp-user)

(defpackage #:docudown-test
  (:use #:common-lisp #:lift #:cl-markdown #:trivial-shell)
  (:shadowing-import-from #:lift
                          #:with-timeout))
