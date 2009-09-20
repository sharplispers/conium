;;; -*- mode: lisp; indent-tabs: nil -*-

(defpackage :conium
  (:use :common-lisp)
  (:export #:sldb-condition
           #:original-condition
           #:compiler-condition
           #:message
           #:short-message
           #:condition
           #:severity
           #:with-compilation-hooks
           #:location
           #:location-p
           #:location-buffer
           #:location-position
           #:position-p
           #:position-pos
           #:print-output-to-string
           #:quit-lisp
           #:references
           #:unbound-slot-filler
           #:declaration-arglist
           #:type-specifier-arglist
           ;; interrupt macro for the backend
           #:*pending-slime-interrupts*
           #:check-slime-interrupts
           #:*interrupt-queued-handler*
           ;; inspector related symbols
           #:emacs-inspect
           #:label-value-line
           #:label-value-line*
           
           #:with-struct
           ))
