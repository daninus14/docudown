#|

Author: Gary King

|#

(defpackage #:docudown-test-system (:use #:cl #:asdf))
(in-package #:docudown-test-system)

(defsystem docudown-test
  :version "0.1"
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :components ((:module 
		"tests"
		:components ((:file "package")
			     (:file "tests"
				    :depends-on ("package")))))
  :depends-on (:lift
	       :docudown
	       :cl-markdown-test))
