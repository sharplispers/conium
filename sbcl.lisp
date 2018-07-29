;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; swank-sbcl.lisp --- SLIME backend for SBCL.
;;;
;;; Created 2003, Daniel Barlow <dan@metacircles.com>
;;;
;;; This code has been placed in the Public Domain.  All warranties are
;;; disclaimed.

;;; Requires the SB-INTROSPECT contrib.

;;; Administrivia

(in-package :conium)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sb-introspect)
  (require 'sb-posix)
  (require 'sb-cltl2))

(declaim (optimize (debug 2) 
                   (sb-c::insert-step-conditions 0)
                   (sb-c::insert-debug-catch 0)
                   (sb-c::merge-tail-calls 2)))

;;; backwards compability tests

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Generate a form suitable for testing for stepper support (0.9.17)
  ;; with #+.
  (defun sbcl-with-new-stepper-p ()
    (with-symbol 'enable-stepping 'sb-impl))
  ;; Ditto for weak hash-tables
  (defun sbcl-with-weak-hash-tables ()
    (with-symbol 'hash-table-weakness 'sb-ext))
  ;; And for xref support (1.0.1)
  (defun sbcl-with-xref-p ()
    (with-symbol 'who-calls 'sb-introspect))
  ;; ... for restart-frame support (1.0.2)
  (defun sbcl-with-restart-frame ()
    (with-symbol 'frame-has-debug-tag-p 'sb-debug)))

;;; Connection info

(defimplementation lisp-implementation-type-name ()
  "sbcl")

;; Declare return type explicitly to shut up STYLE-WARNINGS about
;; %SAP-ALIEN in ENABLE-SIGIO-ON-FD below.
(declaim (ftype (function () (values (signed-byte 32) &optional)) getpid))
(defimplementation getpid ()
  (sb-posix:getpid))

#-win32
(defimplementation install-sigint-handler (function)
  (sb-sys:enable-interrupt sb-unix:sigint 
                           (lambda (&rest args)
                             (declare (ignore args))
                             (sb-sys:invoke-interruption 
                              (lambda ()
                                (sb-sys:with-interrupts 
                                  (funcall function)))))))

(defvar *sigio-handlers* '()
  "List of (key . fn) pairs to be called on SIGIO.")

(defun sigio-handler (signal code scp)
  (declare (ignore signal code scp))
  (mapc (lambda (handler)
          (funcall (the function (cdr handler))))
        *sigio-handlers*))

(defun set-sigio-handler ()
  (sb-sys:enable-interrupt sb-unix:sigio (lambda (signal code scp)
                                           (sigio-handler signal code scp))))

(defun enable-sigio-on-fd (fd)
  (sb-posix::fcntl fd sb-posix::f-setfl sb-posix::o-async)
  (sb-posix::fcntl fd sb-posix::f-setown (getpid))
  (values))

(defvar *external-format-to-coding-system*
  '((:iso-8859-1 
     "latin-1" "latin-1-unix" "iso-latin-1-unix" 
     "iso-8859-1" "iso-8859-1-unix")
    (:utf-8 "utf-8" "utf-8-unix")
    (:euc-jp "euc-jp" "euc-jp-unix")
    (:us-ascii "us-ascii" "us-ascii-unix")))

;; C.f. R.M.Kreuter in <20536.1219412774@progn.net> on sbcl-general, 2008-08-22.
(defvar *physical-pathname-host* (pathname-host (user-homedir-pathname)))

(defimplementation filename-to-pathname (filename)
  (sb-ext:parse-native-namestring filename *physical-pathname-host*))

(defimplementation find-external-format (coding-system)
  (car (rassoc-if (lambda (x) (member coding-system x :test #'equal))
                  *external-format-to-coding-system*)))

(defimplementation call-without-interrupts (fn)
  (declare (type function fn))
  (sb-sys:without-interrupts (funcall fn)))



;;;; Support for SBCL syntax

;;; SBCL's source code is riddled with #! reader macros.  Also symbols
;;; containing `!' have special meaning.  We have to work long and
;;; hard to be able to read the source.  To deal with #! reader
;;; macros, we use a special readtable.  The special symbols are
;;; converted by a condition handler.

(defun feature-in-list-p (feature list)
  (etypecase feature
    (symbol (member feature list :test #'eq))
    (cons (flet ((subfeature-in-list-p (subfeature)
		   (feature-in-list-p subfeature list)))
	    (ecase (first feature)
	      (:or  (some  #'subfeature-in-list-p (rest feature)))
	      (:and (every #'subfeature-in-list-p (rest feature)))
	      (:not (destructuring-bind (e) (cdr feature)
                      (not (subfeature-in-list-p e)))))))))

(defun shebang-reader (stream sub-character infix-parameter)
  (declare (ignore sub-character))
  (when infix-parameter
    (error "illegal read syntax: #~D!" infix-parameter))
  (let ((next-char (read-char stream)))
    (unless (find next-char "+-")
      (error "illegal read syntax: #!~C" next-char))
    ;; When test is not satisfied
    ;; FIXME: clearer if order of NOT-P and (NOT NOT-P) were reversed? then
    ;; would become "unless test is satisfied"..
    (when (let* ((*package* (find-package "KEYWORD"))
		 (*read-suppress* nil)
		 (not-p (char= next-char #\-))
		 (feature (read stream)))
	    (if (feature-in-list-p feature *features*)
		not-p
		(not not-p)))
      ;; Read (and discard) a form from input.
      (let ((*read-suppress* t))
	(read stream t nil t))))
 (values))

(defvar *shebang-readtable*
  (let ((*readtable* (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\!
                                  (lambda (s c n) (shebang-reader s c n))
                                  *readtable*)
    *readtable*))

(defun shebang-readtable ()
  *shebang-readtable*)

(defun sbcl-package-p (package)
  (let ((name (package-name package)))
    (eql (mismatch "SB-" name) 3)))

(defun sbcl-source-file-p (filename)
  (when filename
    (loop for (nil pattern) in (logical-pathname-translations "SYS")
          thereis (pathname-match-p filename pattern))))

(defun guess-readtable-for-filename (filename)
  (if (sbcl-source-file-p filename)
      (shebang-readtable)
      *readtable*))

(defvar *debootstrap-packages* t)

(defun call-with-debootstrapping (fun)
  (handler-bind ((sb-int:bootstrap-package-not-found
                  #'sb-int:debootstrap-package))
    (funcall fun)))

(defmacro with-debootstrapping (&body body)
  `(call-with-debootstrapping (lambda () ,@body)))

(defimplementation call-with-syntax-hooks (fn)
  (cond ((and *debootstrap-packages*
              (sbcl-package-p *package*))
         (with-debootstrapping (funcall fn)))
        (t
         (funcall fn))))

(defimplementation default-readtable-alist ()
  (let ((readtable (shebang-readtable)))
    (loop for p in (remove-if-not #'sbcl-package-p (list-all-packages))
          collect (cons (package-name p) readtable))))

;;; Utilities

#+#.(conium::with-symbol 'function-lambda-list 'sb-introspect)
(defimplementation arglist (fname)
  (sb-introspect:function-lambda-list fname))

#-#.(conium::with-symbol 'function-lambda-list 'sb-introspect)
(defimplementation arglist (fname)
  (sb-introspect:function-arglist fname))

(defimplementation function-name (f)
  (check-type f function)
  (sb-impl::%fun-name f))

(defmethod declaration-arglist ((decl-identifier (eql 'optimize)))
  (flet ((ensure-list (thing) (if (listp thing) thing (list thing))))
    (let* ((flags (sb-cltl2:declaration-information decl-identifier)))
      (if flags
          ;; Symbols aren't printed with package qualifiers, but the FLAGS would
          ;; have to be fully qualified when used inside a declaration. So we
          ;; strip those as long as there's no better way. (FIXME)
          `(&any ,@(remove-if-not #'(lambda (qualifier)
                                      (find-symbol (symbol-name (first qualifier)) :cl))
                                  flags :key #'ensure-list))
          (call-next-method)))))

#+#.(conium::with-symbol 'deftype-lambda-list 'sb-introspect)
(defmethod type-specifier-arglist :around (typespec-operator)
  (multiple-value-bind (arglist foundp)
      (sb-introspect:deftype-lambda-list typespec-operator)
    (if foundp arglist (call-next-method))))

(defvar *buffer-name* nil)
(defvar *buffer-offset*)
(defvar *buffer-substring* nil)

(defvar *previous-compiler-condition* nil
  "Used to detect duplicates.")

(defun handle-notification-condition (condition)
  "Handle a condition caused by a compiler warning.
This traps all compiler conditions at a lower-level than using
C:*COMPILER-NOTIFICATION-FUNCTION*. The advantage is that we get to
craft our own error messages, which can omit a lot of redundant
information."
  (unless (or (eq condition *previous-compiler-condition*))
    ;; First resignal warnings, so that outer handlers -- which may choose to
    ;; muffle this -- get a chance to run.
    (when (typep condition 'warning)
      (signal condition))
    (setq *previous-compiler-condition* condition)
    (signal-compiler-condition condition (sb-c::find-error-context nil))))

(defun signal-compiler-condition (condition context)
  (signal (make-condition
           'compiler-condition
           :original-condition condition
           :severity (etypecase condition
                       (sb-c:compiler-error  :error)
                       (sb-ext:compiler-note :note)
                       (style-warning        :style-warning)
                       (warning              :warning)
                       (reader-error         :read-error)
                       (error                :error))
           :short-message (brief-compiler-message-for-emacs condition)
           :references (condition-references (real-condition condition))
           :message (long-compiler-message-for-emacs condition context)
           :location (compiler-note-location condition context))))

(defun real-condition (condition)
  "Return the encapsulated condition or CONDITION itself."
  (typecase condition
    (sb-int:encapsulated-condition (sb-int:encapsulated-condition condition))
    (t condition)))

(defun condition-references (condition)
  (if (typep condition 'sb-int:reference-condition)
      (externalize-reference
       (sb-int:reference-condition-references condition))))

(defun compiler-note-location (condition context)
  (flet ((bailout ()
           (list :error "No error location available")))
    (cond (context
           (locate-compiler-note
            (sb-c::compiler-error-context-file-name context)
            (compiler-source-path context)
            (sb-c::compiler-error-context-original-source context)))
          ((typep condition 'reader-error)
           (let* ((stream (stream-error-stream condition))
                  (file   (pathname stream)))
             (unless (open-stream-p stream)
               (bailout))
             (if (compiling-from-buffer-p file)
                 ;; The stream position for e.g. "comma not inside backquote"
                 ;; is at the character following the comma, :offset is 0-based,
                 ;; hence the 1-.
                 (make-location (list :buffer *buffer-name*)
                                (list :offset *buffer-offset*
                                      (1- (file-position stream))))
                 (progn
                   (assert (compiling-from-file-p file))
                   ;; No 1- because :position is 1-based.
                   (make-location (list :file (namestring file))
                                  (list :position (file-position stream)))))))
          (t (bailout)))))

(defun compiling-from-buffer-p (filename)
  (and (not (eq filename :lisp)) *buffer-name*))

(defun compiling-from-file-p (filename)
  (and (pathnamep filename) (null *buffer-name*)))

(defun compiling-from-generated-code-p (filename source)
  (and (eq filename :lisp) (stringp source)))

(defun locate-compiler-note (file source-path source)
  (cond ((compiling-from-buffer-p file)
         (make-location (list :buffer *buffer-name*)
                        (list :offset  *buffer-offset* 
                              (source-path-string-position
                               source-path *buffer-substring*))))
        ((compiling-from-file-p file)
         (make-location (list :file (namestring file))
                        (list :position (1+ (source-path-file-position
                                             source-path file)))))
        ((compiling-from-generated-code-p file source)
         (make-location (list :source-form source)
                        (list :position 1)))
        (t
         (error "unhandled case in compiler note ~S ~S ~S" file source-path source))))

(defun brief-compiler-message-for-emacs (condition)
  "Briefly describe a compiler error for Emacs.
When Emacs presents the message it already has the source popped up
and the source form highlighted. This makes much of the information in
the error-context redundant."
  (let ((sb-int:*print-condition-references* nil))
    (princ-to-string condition)))

(defun long-compiler-message-for-emacs (condition error-context)
  "Describe a compiler error for Emacs including context information."
  (declare (type (or sb-c::compiler-error-context null) error-context))
  (multiple-value-bind (enclosing source)
      (if error-context
          (values (sb-c::compiler-error-context-enclosing-source error-context)
                  (sb-c::compiler-error-context-source error-context)))
    (let ((sb-int:*print-condition-references* nil))
      (format nil "~@[--> ~{~<~%--> ~1:;~A~> ~}~%~]~@[~{==>~%~A~%~}~]~A"
              enclosing source condition))))

(defun compiler-source-path (context)
  "Return the source-path for the current compiler error.
Returns NIL if this cannot be determined by examining internal
compiler state."
  (cond ((sb-c::node-p context)
         (reverse
          (sb-c::source-path-original-source
           (sb-c::node-source-path context))))
        ((sb-c::compiler-error-context-p context)
         (reverse
          (sb-c::compiler-error-context-original-source-path context)))))

(defimplementation call-with-compilation-hooks (function)
  (declare (type function function))
  (handler-bind ((sb-c:fatal-compiler-error #'handle-file-compiler-termination)
                 (sb-c:compiler-error  #'handle-notification-condition)
                 (sb-ext:compiler-note #'handle-notification-condition)
                 (warning              #'handle-notification-condition))
    (funcall function)))

(defun handle-file-compiler-termination (condition)
  "Handle a condition that caused the file compiler to terminate."
  (handle-notification-condition
   (sb-int:encapsulated-condition condition)))

(defvar *trap-load-time-warnings* nil)

(defimplementation swank-compile-file (input-file output-file 
                                       load-p external-format)
  (handler-case
      (multiple-value-bind (output-file warnings-p failure-p)
          (with-compilation-hooks ()
            (compile-file input-file :output-file output-file
                          :external-format external-format))
        (values output-file warnings-p
                (or failure-p
                    (when load-p
                      ;; Cache the latest source file for definition-finding.
                      (source-cache-get input-file 
                                        (file-write-date input-file))
                      (not (load output-file))))))
    (sb-c:fatal-compiler-error () nil)))

;;;; compile-string

;;; We copy the string to a temporary file in order to get adequate
;;; semantics for :COMPILE-TOPLEVEL and :LOAD-TOPLEVEL EVAL-WHEN forms
;;; which the previous approach using
;;;     (compile nil `(lambda () ,(read-from-string string)))
;;; did not provide.

(sb-alien:define-alien-routine "tmpnam" sb-alien:c-string
  (dest (* sb-alien:c-string)))

(defun temp-file-name ()
  "Return a temporary file name to compile strings into."
  (concatenate 'string (tmpnam nil) ".lisp"))

(defun get-compiler-policy (default-policy)
  (declare (ignorable default-policy))
  #+#.(conium::with-symbol 'restrict-compiler-policy 'sb-ext)
  (remove-duplicates (append default-policy (sb-ext:restrict-compiler-policy))
                     :key #'car))

(defun set-compiler-policy (policy)
  (declare (ignorable policy))
  #+#.(conium::with-symbol 'restrict-compiler-policy 'sb-ext)
   (loop for (qual . value) in policy
         do (sb-ext:restrict-compiler-policy qual value)))

(defimplementation swank-compile-string (string &key buffer position filename
                                         policy)
  (let ((*buffer-name* buffer)
        (*buffer-offset* position)
        (*buffer-substring* string)
        (temp-file-name (temp-file-name))
        (saved-policy (get-compiler-policy '((debug . 0) (speed . 0)))))
    (when policy
      (set-compiler-policy policy))
    (flet ((load-it (filename)
             (when filename (load filename)))
           (compile-it (cont)
             (with-compilation-hooks ()
               (with-compilation-unit
                   (:source-plist (list :emacs-buffer buffer
                                        :emacs-filename filename
                                        :emacs-string string
                                        :emacs-position position))
                 (funcall cont (compile-file temp-file-name))))))
      (with-open-file (s temp-file-name :direction :output :if-exists :error)
        (write-string string s))
      (unwind-protect
           (if *trap-load-time-warnings*
               (compile-it #'load-it)
               (load-it (compile-it #'identity)))
        (ignore-errors
          (set-compiler-policy saved-policy)
          (delete-file temp-file-name)
          (delete-file (compile-file-pathname temp-file-name)))))))

;;;; Definitions

(defvar *debug-definition-finding* nil
  "When true don't handle errors while looking for definitions.
This is useful when debugging the definition-finding code.")

(defparameter *definition-types*
  '(:variable defvar
    :constant defconstant
    :type deftype
    :symbol-macro define-symbol-macro
    :macro defmacro
    :compiler-macro define-compiler-macro
    :function defun
    :generic-function defgeneric
    :method defmethod
    :setf-expander define-setf-expander
    :structure defstruct
    :condition define-condition
    :class defclass
    :method-combination define-method-combination
    :package defpackage
    :transform :deftransform
    :optimizer :defoptimizer
    :vop :define-vop
    :source-transform :define-source-transform)
  "Map SB-INTROSPECT definition type names to Slime-friendly forms")

(defun definition-specifier (type name)
  "Return a pretty specifier for NAME representing a definition of type TYPE."
  (if (and (symbolp name)
           (eq type :function)
           (sb-int:info :function :ir1-convert name))
      :def-ir1-translator
      (getf *definition-types* type)))


(defimplementation find-definitions (name)
  (loop for type in *definition-types* by #'cddr
        for locations = (sb-introspect:find-definition-sources-by-name
                         name type)
        append (loop for source-location in locations collect
                     (make-source-location-specification type name
                                                         source-location))))

(defimplementation find-source-location (obj)
  (flet ((general-type-of (obj)
           (typecase obj
             (method             :method)
             (generic-function   :generic-function)
             (function           :function)
             (structure-class    :structure-class)
             (class              :class)
             (method-combination :method-combination)
             (package            :package)
             (condition          :condition)             
             (structure-object   :structure-object)
             (standard-object    :standard-object)
             (t                  :thing)))
         (to-string (obj)
           (typecase obj
             (package (princ-to-string obj)) ; Packages are possibly named entities.
             ((or structure-object standard-object condition)
              (with-output-to-string (s)
                (print-unreadable-object (obj s :type t :identity t))))
             (t (princ-to-string obj)))))
    (handler-case
        (make-definition-source-location
         (sb-introspect:find-definition-source obj) (general-type-of obj) (to-string obj))
      (error (e)
        (list :error (format nil "Error: ~A" e))))))


(defun make-source-location-specification (type name source-location)
  (list (make-dspec type name source-location)
        (if *debug-definition-finding*
            (make-definition-source-location source-location type name)
            (handler-case
                (make-definition-source-location source-location type name)
              (error (e)
                (list :error (format nil "Error: ~A" e)))))))

(defun make-dspec (type name source-location)
  (list* (definition-specifier type name)
         name
         (sb-introspect::definition-source-description source-location)))

(defun make-definition-source-location (definition-source type name)
  (with-struct (sb-introspect::definition-source-
                   pathname form-path character-offset plist
                   file-write-date)
      definition-source
    (destructuring-bind (&key emacs-buffer emacs-position emacs-directory
                              emacs-string &allow-other-keys)
        plist
      (cond
        (emacs-buffer
         (let* ((*readtable* (guess-readtable-for-filename emacs-directory))
                (pos (if form-path
                         (with-debootstrapping
                           (source-path-string-position form-path emacs-string))
                         character-offset))
                (snippet (string-path-snippet emacs-string form-path pos)))
           (make-location `(:buffer ,emacs-buffer)
                          `(:offset ,emacs-position ,pos)
                          `(:snippet ,snippet))))
        ((not pathname)
         `(:error ,(format nil "Source definition of ~A ~A not found"
                           (string-downcase type) name)))
        (t
         (let* ((namestring (namestring (translate-logical-pathname pathname)))
                (pos (source-file-position namestring file-write-date form-path
                                           character-offset))
                (snippet (source-hint-snippet namestring file-write-date pos)))
           (make-location `(:file ,namestring)
                          ;; /file positions/ in Common Lisp start
                          ;; from 0, in Emacs they start from 1.
                          `(:position ,(1+ pos))
                          `(:snippet ,snippet))))))))

(defun string-path-snippet (string form-path position)
  (if form-path
      ;; If we have a form-path, use it to derive a more accurate
      ;; snippet, so that we can point to the individual form rather
      ;; than just the toplevel form.
      (multiple-value-bind (data end)
          (let ((*read-suppress* t))
            (read-from-string string nil nil :start position))
        (declare (ignore data))
        (subseq string position end))
      string))    
    
(defun source-file-position (filename write-date form-path character-offset)
  (let ((source (get-source-code filename write-date))
        (*readtable* (guess-readtable-for-filename filename)))
    (with-debootstrapping
      (if form-path
          (source-path-string-position form-path source)
          (or character-offset 0)))))

(defun source-hint-snippet (filename write-date position)
  (let ((source (get-source-code filename write-date)))
    (with-input-from-string (s source)
      (read-snippet s position))))

(defun function-source-location (function &optional name)
  (declare (type function function))
  (let ((location (sb-introspect:find-definition-source function)))
    (make-definition-source-location location :function name)))

(defun safe-function-source-location (fun name)
  (if *debug-definition-finding*
      (function-source-location fun name)
      (handler-case (function-source-location fun name)
        (error (e)
          (list :error (format nil "Error: ~A" e))))))

(defimplementation describe-symbol-for-emacs (symbol)
  "Return a plist describing SYMBOL.
Return NIL if the symbol is unbound."
  (let ((result '()))
    (flet ((doc (kind)
             (or (documentation symbol kind) :not-documented))
           (maybe-push (property value)
             (when value
               (setf result (list* property value result)))))
      (maybe-push
       :variable (multiple-value-bind (kind recorded-p)
		     (sb-int:info :variable :kind symbol)
		   (declare (ignore kind))
		   (if (or (boundp symbol) recorded-p)
		       (doc 'variable))))
      (when (fboundp symbol)
	(maybe-push
	 (cond ((macro-function symbol)     :macro)
	       ((special-operator-p symbol) :special-operator)
	       ((typep (fdefinition symbol) 'generic-function)
                :generic-function)
	       (t :function))
	 (doc 'function)))
      (maybe-push
       :setf (if (or (sb-int:info :setf :inverse symbol)
		     (sb-int:info :setf :expander symbol))
		 (doc 'setf)))
      (maybe-push
       :type (if (sb-int:info :type :kind symbol)
		 (doc 'type)))
      result)))

(defimplementation describe-definition (symbol type)
  (case type
    (:variable
     (describe symbol))
    (:function
     (describe (symbol-function symbol)))
    (:setf
     (describe (or (sb-int:info :setf :inverse symbol)
                   (sb-int:info :setf :expander symbol))))
    (:class
     (describe (find-class symbol)))
    (:type
     (describe (sb-kernel:values-specifier-type symbol)))))
  
#+#.(conium::sbcl-with-xref-p)
(progn
  (defmacro defxref (name)
    `(defimplementation ,name (what)
       (sanitize-xrefs   
        (mapcar #'source-location-for-xref-data
                (,(find-symbol (symbol-name name) "SB-INTROSPECT")
                  what)))))
  (defxref who-calls)
  (defxref who-binds)
  (defxref who-sets)
  (defxref who-references)
  (defxref who-macroexpands)
  #+#.(conium::with-symbol 'who-specializes 'sb-introspect)
  (defxref who-specializes))

(defun source-location-for-xref-data (xref-data)
  (let ((name (car xref-data))
        (source-location (cdr xref-data)))
    (list name
          (handler-case (make-definition-source-location source-location
                                                         'function
                                                         name)
            (error (e)
              (list :error (format nil "Error: ~A" e)))))))

(defimplementation list-callers (symbol)
  (let ((fn (fdefinition symbol)))
    (sanitize-xrefs
     (mapcar #'function-dspec (sb-introspect:find-function-callers fn)))))

(defimplementation list-callees (symbol)
  (let ((fn (fdefinition symbol)))
    (sanitize-xrefs
     (mapcar #'function-dspec (sb-introspect:find-function-callees fn)))))

(defun sanitize-xrefs (xrefs)
  (remove-duplicates
   (remove-if (lambda (f)
                (member f (ignored-xref-function-names)))
              (loop for entry in xrefs
                    for name = (car entry)
                    collect (if (and (consp name)
                                     (member (car name)
                                             '(sb-pcl::fast-method
                                               sb-pcl::slow-method
                                               sb-pcl::method)))
                                (cons (cons 'defmethod (cdr name))
                                      (cdr entry))
                                entry))
              :key #'car)
   :test (lambda (a b)
           (and (eq (first a) (first b))
                (equal (second a) (second b))))))

(defun ignored-xref-function-names ()
  #-#.(conium::sbcl-with-new-stepper-p)
  '(nil sb-c::step-form sb-c::step-values)
  #+#.(conium::sbcl-with-new-stepper-p)
  '(nil))

(defun function-dspec (fn)
  "Describe where the function FN was defined.
Return a list of the form (NAME LOCATION)."
  (let ((name (sb-kernel:%fun-name fn)))
    (list name (safe-function-source-location fn name))))

;;; macroexpansion

(defimplementation macroexpand-all (form)
  (let ((sb-walker:*walk-form-expand-macros-p* t))
    (sb-walker:walk-form form)))


;;; Debugging

(defvar *sldb-stack-top*)

(defun make-invoke-debugger-hook (hook)
  #'(lambda (condition old-hook)
      ;; Notice that *INVOKE-DEBUGGER-HOOK* is tried before
      ;; *DEBUGGER-HOOK*, so we have to make sure that the latter gets
      ;; run when it was established locally by a user (i.e. changed meanwhile.)
      (if *debugger-hook*
          (funcall *debugger-hook* condition old-hook)
          (funcall hook condition old-hook))))

(defimplementation install-debugger-globally (function)
  (setq *debugger-hook* function)
  (setq sb-ext:*invoke-debugger-hook* (make-invoke-debugger-hook function)))

(defimplementation condition-extras (condition)
  (cond #+#.(conium::sbcl-with-new-stepper-p)
        ((typep condition 'sb-impl::step-form-condition)
         `((:show-frame-source 0)))
        ((typep condition 'sb-int:reference-condition)
         (let ((refs (sb-int:reference-condition-references condition)))
           (if refs
               `((:references ,(externalize-reference refs))))))))

(defun externalize-reference (ref)
  (etypecase ref
    (null nil)
    (cons (cons (externalize-reference (car ref))
                (externalize-reference (cdr ref))))
    ((or string number) ref)
    (symbol 
     (cond ((eq (symbol-package ref) (symbol-package :test))
            ref)
           (t (symbol-name ref))))))

(defimplementation call-with-debugging-environment (debugger-loop-fn)
  (declare (type function debugger-loop-fn))
  (let* ((*sldb-stack-top* (or sb-debug:*stack-top-hint* (sb-di:top-frame)))
         (sb-debug:*stack-top-hint* nil))
    (handler-bind ((sb-di:debug-condition
		    (lambda (condition)
                      (signal (make-condition
                               'sldb-condition
                               :original-condition condition)))))
      (funcall debugger-loop-fn))))

#+#.(conium::sbcl-with-new-stepper-p)
(progn
  (defimplementation activate-stepping (frame)
    (declare (ignore frame))
    (sb-impl::enable-stepping))
  (defimplementation sldb-stepper-condition-p (condition)
    (typep condition 'sb-ext:step-form-condition))
  (defimplementation sldb-step-into ()
    (invoke-restart 'sb-ext:step-into))
  (defimplementation sldb-step-next ()
    (invoke-restart 'sb-ext:step-next))
  (defimplementation sldb-step-out ()
    (invoke-restart 'sb-ext:step-out)))

(defimplementation call-with-debugger-hook (hook fun)
  (let ((*debugger-hook* hook)
        (sb-ext:*invoke-debugger-hook* (make-invoke-debugger-hook hook))
        #+#.(conium::sbcl-with-new-stepper-p)
        (sb-ext:*stepper-hook*
         (lambda (condition)
           (typecase condition
             (sb-ext:step-form-condition
              (let ((sb-debug:*stack-top-hint* (sb-di::find-stepped-frame)))
                (sb-impl::invoke-debugger condition)))))))
    (handler-bind (#+#.(conium::sbcl-with-new-stepper-p)
                   (sb-ext:step-condition #'sb-impl::invoke-stepper))
      (funcall fun))))

(defun nth-frame (index)
  (do ((frame *sldb-stack-top* (sb-di:frame-down frame))
       (i index (1- i)))
      ((zerop i) frame)))

(defimplementation compute-backtrace (start end)
  "Return a list of frames starting with frame number START and
continuing to frame number END or, if END is nil, the last frame on the
stack."
  (let ((end (or end most-positive-fixnum)))
    (loop for f = (nth-frame start) then (sb-di:frame-down f)
	  for i from start below end
	  while f collect f)))

(defimplementation print-frame (frame stream)
  (sb-debug::print-frame-call frame stream))

(defimplementation frame-restartable-p (frame)
  #+#.(conium::sbcl-with-restart-frame)
  (not (null (sb-debug:frame-has-debug-tag-p frame))))

;;;; Code-location -> source-location translation

;;; If debug-block info is avaibale, we determine the file position of
;;; the source-path for a code-location.  If the code was compiled
;;; with C-c C-c, we have to search the position in the source string.
;;; If there's no debug-block info, we return the (less precise)
;;; source-location of the corresponding function.

(defun code-location-source-location (code-location)
  (let* ((dsource (sb-di:code-location-debug-source code-location))
         (plist (sb-c::debug-source-plist dsource)))
    (if (getf plist :emacs-buffer)
        (emacs-buffer-source-location code-location plist)
        #+#.(conium::with-symbol 'debug-source-from 'sb-di)
        (ecase (sb-di:debug-source-from dsource)
          (:file (file-source-location code-location))
          (:lisp (lisp-source-location code-location)))
        #-#.(conium::with-symbol 'debug-source-from 'sb-di)
        (if (sb-di:debug-source-namestring dsource)
            (file-source-location code-location)
            (lisp-source-location code-location)))))

;;; FIXME: The naming policy of source-location functions is a bit
;;; fuzzy: we have FUNCTION-SOURCE-LOCATION which returns the
;;; source-location for a function, and we also have FILE-SOURCE-LOCATION &co
;;; which returns the source location for a _code-location_.
;;;
;;; Maybe these should be named code-location-file-source-location,
;;; etc, turned into generic functions, or something. In the very
;;; least the names should indicate the main entry point vs. helper
;;; status.

(defun file-source-location (code-location)
  (if (code-location-has-debug-block-info-p code-location)
      (source-file-source-location code-location)
      (fallback-source-location code-location)))

(defun fallback-source-location (code-location)
  (let ((fun (code-location-debug-fun-fun code-location)))
    (cond (fun (function-source-location fun))
          (t (error "Cannot find source location for: ~A " code-location)))))

(defun lisp-source-location (code-location)
  (let ((source (prin1-to-string
                 (sb-debug::code-location-source-form code-location 100))))
    (make-location `(:source-form ,source) '(:position 1))))

(defun emacs-buffer-source-location (code-location plist)
  (if (code-location-has-debug-block-info-p code-location)
      (destructuring-bind (&key emacs-buffer emacs-position emacs-string
                                &allow-other-keys)
          plist
        (let* ((pos (string-source-position code-location emacs-string))
               (snipped (with-input-from-string (s emacs-string)
                          (read-snippet s pos))))
          (make-location `(:buffer ,emacs-buffer)
                         `(:offset ,emacs-position ,pos)
                         `(:snippet ,snipped))))
      (fallback-source-location code-location)))

(defun source-file-source-location (code-location)
  (let* ((code-date (code-location-debug-source-created code-location))
         (filename (code-location-debug-source-name code-location))
         (*readtable* (guess-readtable-for-filename filename))
         (source-code (get-source-code filename code-date)))
    (with-debootstrapping
      (with-input-from-string (s source-code)
        (let* ((pos (stream-source-position code-location s))
               (snippet (read-snippet s pos)))
          (make-location `(:file ,filename)
                         `(:position ,pos)
                         `(:snippet ,snippet)))))))

(defun code-location-debug-source-name (code-location)
  (namestring (truename (#+#.(conium::with-symbol
                              'debug-source-name 'sb-di)
                             sb-c::debug-source-name
                             #-#.(conium::with-symbol
                                  'debug-source-name 'sb-di)
                             sb-c::debug-source-namestring
                         (sb-di::code-location-debug-source code-location)))))

(defun code-location-debug-source-created (code-location)
  (sb-c::debug-source-created
   (sb-di::code-location-debug-source code-location)))

(defun code-location-debug-fun-fun (code-location)
  (sb-di:debug-fun-fun (sb-di:code-location-debug-fun code-location)))

(defun code-location-has-debug-block-info-p (code-location)
  (handler-case
      (progn (sb-di:code-location-debug-block code-location)
             t)
    (sb-di:no-debug-blocks  () nil)))

(defun stream-source-position (code-location stream)
  (let* ((cloc (sb-debug::maybe-block-start-location code-location))
	 (tlf-number (sb-di::code-location-toplevel-form-offset cloc))
	 (form-number (sb-di::code-location-form-number cloc)))
    (multiple-value-bind (tlf pos-map) (read-source-form tlf-number stream)
      (let* ((path-table (sb-di::form-number-translations tlf 0))
             (path (cond ((<= (length path-table) form-number)
                          (warn "inconsistent form-number-translations")
                          (list 0))
                         (t
                          (reverse (cdr (aref path-table form-number)))))))
        (source-path-source-position path tlf pos-map)))))

(defun string-source-position (code-location string)
  (with-input-from-string (s string)
    (stream-source-position code-location s)))

;;; source-path-file-position and friends are in swank-source-path-parser

(defun safe-source-location-for-emacs (code-location)
  (if *debug-definition-finding*
      (code-location-source-location code-location)
      (handler-case (code-location-source-location code-location)
        (error (c) (list :error (format nil "~A" c))))))

(defimplementation frame-source-location-for-emacs (index)
  (safe-source-location-for-emacs
   (sb-di:frame-code-location (nth-frame index))))

(defun frame-debug-vars (frame)
  "Return a vector of debug-variables in frame."
  (sb-di::debug-fun-debug-vars (sb-di:frame-debug-fun frame)))

(defun debug-var-value (var frame location)
  (ecase (sb-di:debug-var-validity var location)
    (:valid (sb-di:debug-var-value var frame))
    ((:invalid :unknown) ':<not-available>)))

(defimplementation frame-locals (index)
  (let* ((frame (nth-frame index))
	 (loc (sb-di:frame-code-location frame))
	 (vars (frame-debug-vars frame)))
    (loop for v across vars collect
          (list :name (sb-di:debug-var-symbol v)
                :id (sb-di:debug-var-id v)
                :value (debug-var-value v frame loc)))))

(defimplementation frame-var-value (frame var)
  (let* ((frame (nth-frame frame))
         (dvar (aref (frame-debug-vars frame) var)))
    (debug-var-value dvar frame (sb-di:frame-code-location frame))))

(defimplementation frame-catch-tags (index)
  (mapcar #'car (sb-di:frame-catches (nth-frame index))))

(defimplementation eval-in-frame (form index)
  (let ((frame (nth-frame index)))
    (funcall (the function
               (sb-di:preprocess-for-eval form
                                          (sb-di:frame-code-location frame)))
             frame)))

#+#.(conium::sbcl-with-restart-frame)
(progn
  (defimplementation return-from-frame (index form)
    (let* ((frame (nth-frame index)))
      (cond ((sb-debug:frame-has-debug-tag-p frame)
             (let ((values (multiple-value-list (eval-in-frame form index))))
               (sb-debug:unwind-to-frame-and-call frame
                                                   (lambda ()
                                                     (values-list values)))))
            (t (format nil "Cannot return from frame: ~S" frame)))))
  
  (defimplementation restart-frame (index)
    (let* ((frame (nth-frame index)))
      (cond ((sb-debug:frame-has-debug-tag-p frame)
             (let* ((call-list (sb-debug::frame-call-as-list frame))
                    (fun (fdefinition (car call-list)))
                    (thunk (lambda () 
                             ;; Ensure that the thunk gets tail-call-optimized
                             (declare (optimize (debug 1)))
                             (apply fun (cdr call-list)))))
               (sb-debug:unwind-to-frame-and-call frame thunk)))
            (t (format nil "Cannot restart frame: ~S" frame))))))

;; FIXME: this implementation doesn't unwind the stack before
;; re-invoking the function, but it's better than no implementation at
;; all.
#-#.(conium::sbcl-with-restart-frame)
(progn
  (defun sb-debug-catch-tag-p (tag)
    (and (symbolp tag)
         (not (symbol-package tag))
         (string= tag :sb-debug-catch-tag)))
  
  (defimplementation return-from-frame (index form)
    (let* ((frame (nth-frame index))
           (probe (assoc-if #'sb-debug-catch-tag-p
                            (sb-di::frame-catches frame))))
      (cond (probe (throw (car probe) (eval-in-frame form index)))
            (t (format nil "Cannot return from frame: ~S" frame)))))
  
  (defimplementation restart-frame (index)
    (let ((frame (nth-frame index)))
      (return-from-frame index (sb-debug::frame-call-as-list frame)))))

;;;;; reference-conditions

(defimplementation format-sldb-condition (condition)
  (let ((sb-int:*print-condition-references* nil))
    (princ-to-string condition)))


;;;; Profiling

(defimplementation profile (fname)
  (when fname (eval `(sb-profile:profile ,fname))))

(defimplementation unprofile (fname)
  (when fname (eval `(sb-profile:unprofile ,fname))))

(defimplementation unprofile-all ()
  (sb-profile:unprofile)
  "All functions unprofiled.")

(defimplementation profile-report ()
  (sb-profile:report))

(defimplementation profile-reset ()
  (sb-profile:reset)
  "Reset profiling counters.")

(defimplementation profiled-functions ()
  (sb-profile:profile))

(defimplementation profile-package (package callers methods)
  (declare (ignore callers methods))
  (eval `(sb-profile:profile ,(package-name (find-package package)))))


;;;; Inspector

(defmethod emacs-inspect ((o t))
  (cond ((sb-di::indirect-value-cell-p o)
         (label-value-line* (:value (sb-kernel:value-cell-ref o))))
	(t
	 (multiple-value-bind (text label parts) (sb-impl::inspected-parts o)
           (list* (format nil "~a~%" text)
                  (if label
                      (loop for (l . v) in parts
                            append (label-value-line l v))
                      (loop for value in parts  for i from 0
                            append (label-value-line i value))))))))

(defmethod emacs-inspect ((o function))
  (cond ((sb-kernel:simple-fun-p o)
         (label-value-line*
          (:name (sb-kernel:%simple-fun-name o))
          (:arglist (sb-kernel:%simple-fun-arglist o))
          (:next (sb-kernel:%simple-fun-next o))
          (:type (sb-kernel:%simple-fun-type o))
          (:code (sb-kernel:fun-code-header o))))
        ((sb-kernel:closurep o)
         (append
          (label-value-line :function (sb-kernel:%closure-fun o))
          `("Closed over values:" (:newline))
          (loop for i below (1- (sb-kernel:get-closure-length o))
                append (label-value-line
                        i (sb-kernel:%closure-index-ref o i)))))
	(t (call-next-method o))))

(defmethod emacs-inspect ((o sb-kernel:code-component))
          (append
           (label-value-line*
            (:code-size (sb-kernel:%code-code-size o))
            (:entry-points (sb-kernel:%code-entry-points o))
            (:debug-info (sb-kernel:%code-debug-info o)))
           `("Constants:" (:newline))
           (loop for i from sb-vm:code-constants-offset
                 below (sb-kernel:get-header-data o)
                 append (label-value-line i (sb-kernel:code-header-ref o i)))
           `("Code:" (:newline)
             , (with-output-to-string (s)
                 (cond ((sb-kernel:%code-debug-info o)
                        (sb-disassem:disassemble-code-component o :stream s))
                       (t
                        (sb-disassem:disassemble-memory
                         (sb-disassem::align
                          (+ (logandc2 (sb-kernel:get-lisp-obj-address o)
                                       sb-vm:lowtag-mask)
                             (* sb-vm:code-constants-offset
                                sb-vm:n-word-bytes))
                          (ash 1 sb-vm:n-lowtag-bits))
                         (ash (sb-kernel:%code-code-size o) sb-vm:word-shift)
                         :stream s)))))))

(defmethod emacs-inspect ((o sb-ext:weak-pointer))
          (label-value-line*
           (:value (sb-ext:weak-pointer-value o))))

(defmethod emacs-inspect ((o sb-kernel:fdefn))
          (label-value-line*
           (:name (sb-kernel:fdefn-name o))
           (:function (sb-kernel:fdefn-fun o))))

(defmethod emacs-inspect :around ((o generic-function))
            (append
             (call-next-method)
             (label-value-line*
              (:pretty-arglist (sb-pcl::generic-function-pretty-arglist o))
              (:initial-methods (sb-pcl::generic-function-initial-methods o))
              )))


(defimplementation quit-lisp ()
  #+sb-thread
  (dolist (thread (remove (current-thread) (all-threads)))
    (ignore-errors (sb-thread:interrupt-thread
                    thread (lambda () (sb-ext:quit :recklessly-p t)))))
  (sb-ext:quit))



;;Trace implementations
;;In SBCL, we have:
;; (trace <name>)
;; (trace :methods '<name>) ;to trace all methods of the gf <name>
;; (trace (method <name> <qualifier>? (<specializer>+)))
;; <name> can be a normal name or a (setf name)

(defun toggle-trace-aux (fspec &rest args)
  (cond ((member fspec (eval '(trace)) :test #'equal)
         (eval `(untrace ,fspec))
         (format nil "~S is now untraced." fspec))
        (t
         (eval `(trace ,@(if args `(:encapsulate nil) (list)) ,fspec ,@args))
         (format nil "~S is now traced." fspec))))

(defun process-fspec (fspec)
  (cond ((consp fspec)
         (ecase (first fspec)
           ((:defun :defgeneric) (second fspec))
           ((:defmethod) `(method ,@(rest fspec)))
           ((:labels) `(labels ,(process-fspec (second fspec)) ,(third fspec)))
           ((:flet) `(flet ,(process-fspec (second fspec)) ,(third fspec)))))
        (t
         fspec)))

(defimplementation toggle-trace (spec)
  (ecase (car spec)
    ((setf)
     (toggle-trace-aux spec))
    ((:defmethod)
     (toggle-trace-aux `(sb-pcl::fast-method ,@(rest (process-fspec spec)))))
    ((:defgeneric)
     (toggle-trace-aux (second spec) :methods t))
    ((:call)
     (destructuring-bind (caller callee) (cdr spec)
       (toggle-trace-aux callee :wherein (list (process-fspec caller)))))))

;;; Weak datastructures

(defimplementation make-weak-key-hash-table (&rest args)  
  #+#.(conium::sbcl-with-weak-hash-tables)
  (apply #'make-hash-table :weakness :key args)
  #-#.(conium::sbcl-with-weak-hash-tables)
  (apply #'make-hash-table args))

(defimplementation make-weak-value-hash-table (&rest args)
  #+#.(conium::sbcl-with-weak-hash-tables)
  (apply #'make-hash-table :weakness :value args)
  #-#.(conium::sbcl-with-weak-hash-tables)
  (apply #'make-hash-table args))

(defimplementation hash-table-weakness (hashtable)
  #+#.(conium::sbcl-with-weak-hash-tables)
  (sb-ext:hash-table-weakness hashtable))

#-win32
(defimplementation save-image (filename &optional restart-function)
  (let ((pid (sb-posix:fork)))
    (cond ((= pid 0) 
           (let ((args `(,filename 
                         ,@(if restart-function
                               `((:toplevel ,restart-function))))))
             (apply #'sb-ext:save-lisp-and-die args)))
          (t
           (multiple-value-bind (rpid status) (sb-posix:waitpid pid 0)
             (assert (= pid rpid))
             (assert (and (sb-posix:wifexited status)
                          (zerop (sb-posix:wexitstatus status)))))))))
