(in-package #:metabang.docudown)

(defparameter *symbol-identities*
  '((thing-names-class-p class type)
    (thing-names-condition-p condition nil)
    (thing-names-constant-p constant variable)
    (thing-names-function-p function nil)
    ;; FIXME - for now, we don't separate 'em
    (thing-names-generic-function-p function function)
    (thing-names-macro-p macro function)
    (thing-names-setf-function-p setf)
    (thing-names-slot-accessor-p function function)
    (thing-names-structure-p structure structure)
    (thing-names-type-p type type structure)
    (thing-names-variable-p variable nil)))


(defmethod additional-markdown-extensions-for-system append (system)
  (declare (ignore system))
  '(cl-markdown::docs 
    cl-markdown::docs-index
    cl-markdown::glossary
    cl-markdown::comment
    cl-markdown::ifdef
    cl-markdown::abbrev
    ))

(defmethod explicitly-undocumented-symbols (system)
  (declare (ignore system))
  nil)

(defmethod search-locations-for-system append (system)
  (declare (ignore system))
  nil)

#+(or)
(defun build-documentation-report 
    (&optional (document (or *multi-document*
			     (first *documents*))))
  (cl-markdown::build-documentation-report
   (asdf:system-relative-pathname
    'db.agraph.documentation "resources/documentation-report.md")
   (asdf:system-relative-pathname
    'db.agraph.documentation "output/documentation-report.html")
    document
    (list :db.agraph
	  :net.cluster
	  :db.agraph.parser
	  :db.agraph.serializer
	  :db.agraph.sparql) 
   :format :html 
   :excluded-symbols (symbols-explicitly-undocumented-for-agraph)
   :docs-package (find-package :db.agraph)
   :search-locations (agraph-search-locations))) 

#+(or)
(defun markdown-extensions-for-agraph ()
  '(cl-markdown::docs cl-markdown::docs-index
    cl-markdown::glossary cl-markdown::links-list
    cl-markdown::comment cl-markdown::ifdef
    cl-markdown::abbrev cl-markdown-user:docs-group
   
    cl-markdown::ag-eval-freetext-stop-words
    cl-markdown::ag-eval-freetext-example-phrase
    cl-markdown::ag-eval-java-agraph-cfg-file))
  
#+(or)
(defun cl-markdown-for-agraph (filter in out 
			       &rest args &key &allow-other-keys)
  (remf args :root)
  (push (cons :|Content-Type| "text/html; charset=UTF-8")
	(getf args :properties))
  (push (list filter t)
	(getf args :properties))
  (push (cons :search-locations 
	      (agraph-search-locations))
	(getf args :properties))
  (let* ((md (apply #'markdown:markdown 
		    in
		    :stream out
		    :format :html
		    :additional-extensions (markdown-extensions-for-agraph)
		    args)))
    (push (list in out md) *documents*)
    md))

#+(or)
(defun cl-markdown-many-for-agraph (filter docs &rest args)
  (remf args :root)
  (push (cons :|Content-Type| "text/html; charset=UTF-8")
	(getf args :properties))
  (push (list filter t) (getf args :properties))
  (push (cons :search-locations 
	      (agraph-search-locations))
	(getf args :properties))
  (let ((*package* (find-package :cl-markdown)))
    (multiple-value-bind (main outputs)
	(apply #'markdown:markdown-many
	       docs
	       :format :html
	       :additional-extensions (markdown-extensions-for-agraph)
	       args)
      (loop for (in nil) in docs
	  for (md dest) in outputs do (push (list in dest md) *documents*))
      main)))
