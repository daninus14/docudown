(in-package #:metabang.docudown)

(defgeneric explicitly-undocumented-symbols (system)
  )

(defgeneric additional-markdown-extensions-for-system (system)
  (:method-combination append))

(defgeneric search-locations-for-system (system)
  (:method-combination append))

(defgeneric maybe-symbol-is-p (symbol what)
  (:documentation "Returns true if symbol looks like `what`."))
