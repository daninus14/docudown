#| copyright

See the file COPYING for details

|#

(defpackage #:docudown-system (:use #:asdf #:common-lisp))
(in-package #:docudown-system)

(defsystem docudown 
  :version "0.1.1"
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :components
  ((:static-file "COPYING")
   (:module "setup"
	    :pathname "dev/"
	    :serial t
	    :components
	    ((:file "package")
	     (:file "api")))
   (:module "dev"
	    :depends-on ("setup")
	    :components
	    ((:file "main")
	     (:file "docs" :depends-on ("main"))
	     (:file "reports" :depends-on ("main"))
	     (:file "asdf-integration"))))
  :depends-on (:cl-markdown
	       :moptilities)
  :in-order-to ((test-op (load-op docudown-test)))
  :perform (test-op :after (op c)
		    (funcall
		      (intern (symbol-name '#:run-tests) :lift)
		      :config :generic)))

(defmethod operation-done-p 
    ((o test-op) (c (eql (find-system '#:docudown))))
  (values nil))
