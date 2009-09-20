;;; -*- mode: lisp; indent-tabs: nil -*-

(defsystem :conium
  :serial t
  ;; add new files to this list:
  :components ((:file "package") (:file "conium"))
  :depends-on (#+nil :cl-ppcre))
