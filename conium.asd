;;; -*- mode: lisp; indent-tabs: nil -*-

(defsystem :conium
    :serial t
    ;; add new files to this list:
    :components ((:file "package")
		 (:file "conium")

	         #+cmu (:file "source-path-parser")
		 #+cmu (:file "source-file-cache")
		 
		 #+scl (:file "source-path-parser")
		 #+scl (:file "swank-source-file-cache")

		 #+sbcl (:file "source-path-parser")
		 #+sbcl (:file "source-file-cache")

		 #+ccl (:file "metering")

		 #+clisp (:file "xref")
		 #+clisp (:file "metering")

		 #+armedbear '("abcl")

		 #+ecl (:file "source-path-parser")
		 #+ecl (:file "source-file-cache")

		 #+allegro (:file "swank-allegro")
		 #+ccl (:file "openmcl")
		 #+clisp (:file "clisp")
		 #+ecl (:file "ecl")
		 #+lispworks (:file "lispworks")
		 #+sbcl (:file "sbcl")
		 #+scl (:file "scl"))
    :depends-on (:closer-mop))
