;;; -*- mode: lisp; indent-tabs: nil -*-

(defsystem :conium
  :serial t
  ;; add new files to this list:
  :components ((:file "package")
	       (:file "conium")
	       #+sbcl (:file "sbcl")
	       #+ccl (:file "openmcl"))
  :depends-on (:closer-mop))
