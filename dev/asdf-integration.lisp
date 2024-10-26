(in-package #:asdf)

(defvar *visited-table*)

(defgeneric map-components (component function)
  (:documentation "Funcall `function` on component and its
sub-components and their sub-components all the way down. 

Note that if component is a system, then `map-components` 
does not descend into the system's dependencies."))

(defmethod map-components :around ((thing component) function)
  (declare (ignore function))
  (let ((*visited-table* (make-hash-table)))
    (call-next-method)))

(defmethod map-components ((module module) function)
  (mapc (lambda (thing)
	  (map-components thing function)) 
	(module-components module))
  (maybe-visit-thing module function))

(defmethod map-components ((component component) function)
  (maybe-visit-thing component function))

(defun maybe-visit-thing (component function)
  (unless (gethash component *visited-table*)
    (setf (gethash component *visited-table*) t)
    (funcall function component)))

;;;;

(defgeneric document (operation component mode))

(defclass docudown-source (static-file)
  ((mode :initform nil
	 :initarg :mode
	 :accessor mode)))

(defclass docudown-op (operation)
  ())

(defmethod asdf:component-depends-on ((operation docudown-op)
				      (c system))
  (cons (list 'load-op (component-name c))
        (call-next-method)))

(defmethod operation-done-p ((operation docudown-op)
			     (component system))
  nil)

(defmethod output-files ((operation docudown-op)
			 (component system))
  nil)


(defmethod output-files ((operation docudown-op)
			 (component docudown-source))
  (list 
   (let* ((source-root 
	   (component-pathname (component-parent component)))
	  (wild-source 
	   (merge-pathnames
	    (make-pathname 
	     :name :wild
	     :type :wild
	     :directory `(:relative :wild-inferiors))
	    source-root))
	  (output-root
	   (merge-pathnames
	    (make-pathname 
	     :name :wild
	     :type :wild
	     :directory `(:relative :back "output" :wild-inferiors))
	    source-root)))
     (translate-pathname 
      (namestring (component-pathname component)) wild-source output-root))))

(defun change-extension (pathname new-extension)
  (merge-pathnames (make-pathname :type new-extension) pathname))

(defmethod input-components ((operation docudown-op) (system system))
  (let ((components nil))
    (map-components system
		    (lambda (component)
		      (when (typep component 'docudown-source)
			(push component components))))
    components))

(defmethod input-files ((operation docudown-op)
			(system system))
  (mapcar #'component-pathname (input-components operation system)))

(defmethod input-files ((operation docudown-op) component)
  (declare (ignore component))
  nil)

(defmethod perform ((operation docudown-op) (system component))
  nil)

(defmethod modes-for-extension (kind)
  (list (mode-for-extension kind)))

(defmethod modes-for-extension ((kind (eql :mmd)))
  '((:copy-as-text "text") (:markdown-many "html")))

(defmethod modes-for-extension ((kind (eql :md)))
  '((:copy-as-text "text") (:markdown "html")))

(defmethod mode-for-extension (kind)
  (declare (ignore kind))
  :ignore)

(defmethod mode-for-extension ((kind (eql :mmd)))
  (list :markdown-many "html")) 

(defmethod mode-for-extension ((kind (eql :md)))
  (list :markdown "html"))

(defmethod mode-for-extension ((kind (eql :css)))
  (list :copy nil))

(defmethod mode-for-extension ((kind (eql :vcf)))
  (list :copy nil))

(defmethod mode-for-extension ((kind (eql :xml)))
  (list :copy nil))

(defmethod mode-for-extension ((kind (eql :swf)))
  ;; Flash animation
  (list :copy nil))

(defmethod mode-for-extension ((kind (eql :ico)))
  (list :copy nil))

(defmethod mode-for-extension ((kind (eql :png)))
  (list :copy nil))

(defmethod mode-for-extension ((kind (eql :gif)))
  (list :copy nil))

(defmethod mode-for-extension ((kind (eql :zip)))
  (list :copy nil))

(defmethod mode-for-extension ((kind (eql :jpg)))
  (list :copy nil))

(defmethod mode-for-extension ((kind (eql :pdf)))
  (list :copy nil))

(defmethod mode-for-extension ((kind (eql :asc)))
  ;; PGP files
  (list :copy nil))

(defmethod mode-for-extension ((kind (eql :html)))
  ;; Take HTML directly
  (list :copy nil))

(defun component-pathname-type (component)
    ;;?? GWK 27 Mar 2006 - hack to get around probably correct allegro behavior!
    ;; problem is that sometimes the asdf::name is, e.g., tinaa.css so we get
    ;; a pathname with name "tinaa.css" and type "nil" and we don't know how 
    ;; to handle that downstream...
  (pathname-type (namestring (component-pathname component))))

(defmethod perform ((operation docudown-op) (system system))
  (let ((multi nil))
    (mapc (lambda (component)
	    (loop for (mode extension) in
		 (modes-for-extension 
		  (intern (component-pathname-type component)
			  (load-time-value (find-package :keyword)))) do
	      (if (eq mode :markdown-many)
		  (push (list (component-pathname component)
			      (change-extension 
			       (first (output-files operation component))
			       extension)) multi)
		  (document 
		   system 
		   (list (component-pathname component)
			 (change-extension
			  (first (output-files operation component))
			  extension)) mode))))
	  (input-components operation system))
    (when multi
      (document system multi :markdown-many))))

(defmethod document (operation component (mode (eql :copy)))
  (declare (ignore operation component))
  )

(defmethod document (operation component (mode (eql :copy-as-text)))
  (declare (ignore operation component))
  )

(defmethod document (system pair (mode (eql :markdown))) 
  (markdown:markdown
   (first pair)
   :stream (second pair)
   :format :html 
   :additional-extensions 
   (docudown:additional-markdown-extensions-for-system system)
   :properties
   `((:search-locations . ,(docudown:search-locations-for-system system)))))

(defmethod document (system (components list) (mode (eql :markdown-many)))
  (markdown:markdown-many 
   components
   :format :html 
   :additional-extensions 
   (docudown:additional-markdown-extensions-for-system system)
   :properties 
   `((:search-locations . ,(docudown:search-locations-for-system system)))))

