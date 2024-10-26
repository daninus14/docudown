(in-package #:common-lisp-user)

(defpackage #:metabang.docudown
  (:use #:common-lisp #:cl-markdown #:metabang.bind #:cl-containers
	#:metatilities)
  (:nicknames #:docudown)
  (:import-from #:cl-markdown
		#:link-info
		#:metadata
		#:markdown-warning
		#:stream-string-for-html
		#:html-safe-name
		#:ensure-string
		#:output-anchor
		#:*current-format*
		#:result
		#:short-source
		#:source
		#:warnings)
  (:import-from #:cl-markdown
		#:document
		#:multi-document)
  (:import-from #:asdf
		#:system-relative-pathname)
  (:export #:explicitly-undocumented-symbols
	   #:additional-markdown-extensions-for-system
	   #:search-locations-for-system
	   ))
