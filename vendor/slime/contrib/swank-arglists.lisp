;;; swank-arglists.lisp --- arglist related code ??
;;
;; Authors: Matthias Koeppe  <mkoeppe@mail.math.uni-magdeburg.de>
;;          Tobias C. Rittweiler <tcr@freebits.de>
;;          and others
;;
;; License: Public Domain
;;

(in-package :swank)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (swank-require :swank-c-p-c))

;;;; Utilities

(defun compose (&rest functions)
  "Compose FUNCTIONS right-associatively, returning a function"
  #'(lambda (x)
      (reduce #'funcall functions :initial-value x :from-end t)))

(defun length= (seq n)
  "Test for whether SEQ contains N number of elements. I.e. it's equivalent
 to (= (LENGTH SEQ) N), but besides being more concise, it may also be more
 efficiently implemented."
  (etypecase seq
    (list (do ((i n (1- i))
               (list seq (cdr list)))
              ((or (<= i 0) (null list))
               (and (zerop i) (null list)))))
    (sequence (= (length seq) n))))

(declaim (inline ensure-list))
(defun ensure-list (thing)
  (if (listp thing) thing (list thing)))

(declaim (inline memq))
(defun memq (item list)
  (member item list :test #'eq))

(defun remove-from-tree-if (predicate tree)
  (cond ((atom tree) tree)
        ((funcall predicate (car tree))
         (remove-from-tree-if predicate (cdr tree)))
        (t
         (cons (remove-from-tree-if predicate (car tree)) 
               (remove-from-tree-if predicate (cdr tree))))))

(defun remove-from-tree (item tree)
  (remove-from-tree-if #'(lambda (x) (eql x item)) tree))

(defun maybecall (bool fn &rest args)
  "Call FN with ARGS if BOOL is T. Otherwise return ARGS as multiple values."
  (if bool (apply fn args) (values-list args)))

(defun exactly-one-p (&rest values)
  "If exactly one value in VALUES is non-NIL, this value is returned.
Otherwise NIL is returned."
  (let ((found nil))
    (dolist (v values)
      (when v (if found
                  (return-from exactly-one-p nil)
                  (setq found v))))
    found))

(defun valid-operator-symbol-p (symbol)
  "Is SYMBOL the name of a function, a macro, or a special-operator?"
  (or (fboundp symbol)
      (macro-function symbol)
      (special-operator-p symbol)
      (eq symbol 'declare)))

(defun valid-operator-name-p (string)
  "Is STRING the name of a function, macro, or special-operator?"
  (let ((symbol (parse-symbol string)))
    (valid-operator-symbol-p symbol)))

(defun valid-function-name-p (form)
  (and (match form
         ((#'symbolp _)         t)
         (('setf (#'symbolp _)) t)
         (_                     nil))
       (fboundp form)
       t))


(defmacro multiple-value-or (&rest forms)
  (if (null forms)
      nil
      (let ((first (first forms))
            (rest (rest forms)))
        `(let* ((values (multiple-value-list ,first))
                (primary-value (first values)))
          (if primary-value
              (values-list values)
              (multiple-value-or ,@rest))))))

(defmacro with-available-arglist ((var &rest more-vars) form &body body)
  `(multiple-value-bind (,var ,@more-vars) ,form
     (if (eql ,var :not-available)
         :not-available
         (progn #+ignore (assert (arglist-p ,var)) ,@body))))


;;;; Arglist Definition

(defstruct (arglist (:conc-name arglist.) (:predicate arglist-p))
  provided-args         ; list of the provided actual arguments
  required-args         ; list of the required arguments
  optional-args         ; list of the optional arguments
  key-p                 ; whether &key appeared
  keyword-args          ; list of the keywords
  rest                  ; name of the &rest or &body argument (if any)
  body-p                ; whether the rest argument is a &body
  allow-other-keys-p    ; whether &allow-other-keys appeared
  aux-args              ; list of &aux variables
  any-p                 ; whether &any appeared
  any-args              ; list of &any arguments  [*]
  known-junk            ; &whole, &environment
  unknown-junk)         ; unparsed stuff

;;;
;;; [*] The &ANY lambda keyword is an extension to ANSI Common Lisp,
;;;     and is only used to describe certain arglists that cannot be
;;;     described in another way.
;;;
;;;     &ANY is very similiar to &KEY but while &KEY is based upon
;;;     the idea of a plist (key1 value1 key2 value2), &ANY is a
;;;     cross between &OPTIONAL, &KEY and *FEATURES* lists:
;;;
;;;        a) (&ANY :A :B :C) means that you can provide any (non-null)
;;;              set consisting of the keywords `:A', `:B', or `:C' in
;;;              the arglist. E.g. (:A) or (:C :B :A).
;;;
;;;        (This is not restricted to keywords only, but any self-evaluating
;;;         expression is allowed.)
;;;
;;;        b) (&ANY (key1 v1) (key2 v2) (key3 v3)) means that you can
;;;              provide any (non-null) set consisting of lists where
;;;              the CAR of the list is one of `key1', `key2', or `key3'.
;;;              E.g. ((key1 100) (key3 42)), or ((key3 66) (key2 23))
;;;
;;;
;;;     For example, a) let us describe the situations of EVAL-WHEN as
;;;
;;;       (EVAL-WHEN (&ANY :compile-toplevel :load-toplevel :execute) &BODY body)
;;;
;;;     and b) let us describe the optimization qualifiers that are valid
;;;     in the declaration specifier `OPTIMIZE':
;;;
;;;       (DECLARE (OPTIMIZE &ANY (compilation-speed 1) (safety 1) ...))
;;;

;; This is a wrapper object around anything that came from Slime and
;; could not reliably be read.
(defstruct (arglist-dummy
	     (:conc-name #:arglist-dummy.)
             (:constructor make-arglist-dummy (string-representation)))
  string-representation)

(defun empty-arg-p (dummy)
  (zerop (length (arglist-dummy.string-representation dummy))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +lambda-list-keywords+
    '(&provided &required &optional &rest &key &any)))

(defmacro do-decoded-arglist (decoded-arglist &body clauses)
  (assert (loop for clause in clauses
		thereis (member (car clause) +lambda-list-keywords+)))
  (flet ((parse-clauses (clauses)
	   (let* ((size    (length +lambda-list-keywords+))
		  (initial (make-hash-table :test #'eq :size size))
		  (main    (make-hash-table :test #'eq :size size))
		  (final   (make-hash-table :test #'eq :size size)))
	     (loop for clause in clauses
		   for lambda-list-keyword = (first clause)
		   for clause-parameter    = (second clause)
		   doing (cond ((eq clause-parameter :initially)
				(setf (gethash lambda-list-keyword initial) clause))
			       ((eq clause-parameter :finally)
				(setf (gethash lambda-list-keyword final) clause))
			       (t
				(setf (gethash lambda-list-keyword main) clause)))
		   finally
		(return (values initial main final)))))
	 (generate-main-clause (clause arglist)
	   (destructure-case clause
             ((&provided (&optional arg) . body)
              (let ((gensym (gensym "PROVIDED-ARG+")))
		`(dolist (,gensym (arglist.provided-args ,arglist))
		   (declare (ignorable ,gensym))
		   (let (,@(when arg `((,arg ,gensym))))
		     ,@body))))
	     ((&required (&optional arg) . body)
	      (let ((gensym (gensym "REQUIRED-ARG+")))
		`(dolist (,gensym (arglist.required-args ,arglist))
		   (declare (ignorable ,gensym))
		   (let (,@(when arg `((,arg ,gensym))))
		     ,@body))))
	     ((&optional (&optional arg init) . body)
	      (let ((optarg (gensym "OPTIONAL-ARG+")))
		`(dolist (,optarg (arglist.optional-args ,arglist))
		   (declare (ignorable ,optarg))
		   (let (,@(when arg  `((,arg (optional-arg.arg-name ,optarg))))
			 ,@(when init `((,init (optional-arg.default-arg ,optarg)))))
		     ,@body))))
	     ((&key (&optional keyword arg init) . body)
	      (let ((keyarg (gensym "KEY-ARG+")))
		`(dolist (,keyarg (arglist.keyword-args ,arglist))
		   (declare (ignorable ,keyarg))
		   (let (,@(when keyword `((,keyword (keyword-arg.keyword ,keyarg))))
			 ,@(when arg     `((,arg (keyword-arg.arg-name ,keyarg))))
			 ,@(when init    `((,init (keyword-arg.default-arg ,keyarg)))))
		     ,@body))))
	     ((&rest (&optional arg body-p) . body)
	      `(when (arglist.rest ,arglist)
		 (let (,@(when arg    `((,arg (arglist.rest ,arglist))))
		       ,@(when body-p `((,body-p (arglist.body-p ,arglist)))))
		   ,@body)))
	     ((&any (&optional arg) . body)
              (let ((gensym (gensym "REQUIRED-ARG+")))
                `(dolist (,gensym (arglist.any-args ,arglist))
                    (declare (ignorable ,gensym))
                    (let (,@(when arg `((,arg ,gensym))))
                      ,@body)))))))
    (let ((arglist (gensym "DECODED-ARGLIST+")))
      (multiple-value-bind (initially-clauses main-clauses finally-clauses)
	  (parse-clauses clauses)
	`(let ((,arglist ,decoded-arglist))
	   (block do-decoded-arglist
	     ,@(loop for keyword in '(&provided &required &optional &rest &key &any)
		     append (cddr (gethash keyword initially-clauses))
		     collect (let ((clause (gethash keyword main-clauses)))
			       (when clause (generate-main-clause clause arglist)))
		     append (cddr (gethash keyword finally-clauses)))))))))

;;;; Arglist Printing

(defun print-decoded-arglist (arglist &key operator provided-args highlight)
  (macrolet ((space ()
               ;; Kludge: When OPERATOR is not given, we don't want to
               ;; print a space for the first argument.
               `(if (not operator)
                    (setq operator t)
                    (progn (write-char #\space)
                           (pprint-newline :fill))))
             (with-highlighting ((&key index) &body body)
               `(if (eql ,index (car highlight))
                    (progn (princ "===> ") ,@body (princ " <==="))
                    (progn ,@body)))
             (print-arglist-recursively (argl &key index)
               `(if (eql ,index (car highlight))
                    (print-decoded-arglist ,argl :highlight (cdr highlight))
                    (print-decoded-arglist ,argl))))
    (let ((index 0))
      (pprint-logical-block (nil nil :prefix "(" :suffix ")")
        (when operator
          (princ-arg operator))
        (do-decoded-arglist (remove-given-args arglist provided-args)
          (&provided (arg)
             (space)
             (princ-arg arg)
             (incf index))
          (&required (arg)
             (space)
             (if (arglist-p arg)
                 (print-arglist-recursively arg :index index)
                 (with-highlighting (:index index)
                   (princ-arg arg)))
             (incf index))
          (&optional :initially
             (when (arglist.optional-args arglist)
               (space)
               (princ '&optional)))
          (&optional (arg init-value)
             (space)
             (if (arglist-p arg)
                 (print-arglist-recursively arg :index index)
                 (with-highlighting (:index index)
                   (if (null init-value)
                       (princ-arg arg)
                       (format t "~:@<~A ~S~@:>" arg init-value))))
             (incf index))
          (&key :initially
             (when (arglist.key-p arglist)
               (space) (princ '&key)))
          (&key (keyword arg init)
             (space)
             (if (arglist-p arg)
                 (pprint-logical-block (nil nil :prefix "(" :suffix ")")
                   (prin1 keyword) (space)
                   (print-arglist-recursively arg :index keyword))
                 (with-highlighting (:index keyword)
                   (cond ((and init (keywordp keyword))
                          (format t "~:@<~A ~S~@:>" arg init))
                         (init
                          (format t "~:@<(~S ..) ~S~@:>" keyword init))
                         ((not (keywordp keyword))
                          (format t "~:@<(~S ..)~@:>" keyword))
                         (t
                          (princ-arg keyword))))))
          (&key :finally
             (when (arglist.allow-other-keys-p arglist)
               (space)
               (princ '&allow-other-keys)))
          (&any :initially
             (when (arglist.any-p arglist)
               (space)
               (princ '&any)))
          (&any (arg)
             (space)
             (prin1-arg arg))
          (&rest (args bodyp)
             (space)
             (princ (if bodyp '&body '&rest))
             (space)
             (if (arglist-p args)
                 (print-arglist-recursively args :index index)
                 (with-highlighting (:index index)
                   (princ-arg args))))
          ;; FIXME: add &UNKNOWN-JUNK?
          )))))

(defun princ-arg (arg)
  (princ (if (arglist-dummy-p arg)
             (arglist-dummy.string-representation arg)
             arg)))

(defun prin1-arg (arg)
  (if (arglist-dummy-p arg)
      (princ (arglist-dummy.string-representation arg))
      (prin1 arg)))

(defun print-decoded-arglist-as-template (decoded-arglist &key
                                          (prefix "(") (suffix ")"))
  (let ((first-p t))
    (flet ((space ()
             (unless first-p
               (write-char #\space))
             (setq first-p nil))
           (print-arg-or-pattern (arg)
             (etypecase arg
               (symbol        (if (keywordp arg) (prin1 arg) (princ arg)))
               (string        (princ arg))
               (list          (princ arg))
               (arglist-dummy (princ (arglist-dummy.string-representation arg)))
               (arglist       (print-decoded-arglist-as-template arg)))
             (pprint-newline :fill)))
      (pprint-logical-block (nil nil :prefix prefix :suffix suffix)
        (do-decoded-arglist decoded-arglist
          (&required (arg)
            (space) (print-arg-or-pattern arg))
          (&optional (arg)
            (space) (princ "[") (print-arg-or-pattern arg) (princ "]"))
          (&key (keyword arg)
            (space) 
            (prin1 (if (keywordp keyword) keyword `',keyword))
            (space)
            (print-arg-or-pattern arg)
            (pprint-newline :linear))
          (&any (arg)
            (space) (print-arg-or-pattern arg))
          (&rest (args body-p)
            (when (or (not (arglist.keyword-args decoded-arglist))
                      (arglist.allow-other-keys-p decoded-arglist))
              (if body-p
                  (pprint-newline :mandatory)
                  (space))
              (format t "~A..." args))))))))

(defvar *arglist-pprint-bindings*
  '((*print-case*     . :downcase)
    (*print-pretty*   . t)
    (*print-circle*   . nil)
    (*print-readably* . nil)
    (*print-level*    . 10)
    (*print-length*   . 20)
    (*print-escape*   . nil))) ; no package qualifiers.

(defun decoded-arglist-to-string (decoded-arglist
                                  &key operator highlight
                                  print-right-margin print-lines)
  (with-output-to-string (*standard-output*)
    (with-standard-io-syntax
      (with-bindings *arglist-pprint-bindings*
	(let ((*print-right-margin* print-right-margin)
	      (*print-lines* print-lines))
	  (print-decoded-arglist decoded-arglist 
                                 :operator operator 
                                 :highlight highlight))))))

(defun decoded-arglist-to-template-string (decoded-arglist &key (prefix "(") (suffix ")"))
  (with-output-to-string (*standard-output*)
    (with-standard-io-syntax
      (with-bindings *arglist-pprint-bindings*
        (print-decoded-arglist-as-template decoded-arglist
                                           :prefix prefix
                                           :suffix suffix)))))

;;;; Arglist Decoding / Encoding

(defun decode-required-arg (arg)
  "ARG can be a symbol or a destructuring pattern."
  (etypecase arg
    (symbol        arg)
    (arglist-dummy arg)
    (list          (decode-arglist arg))))

(defun encode-required-arg (arg)
  (etypecase arg
    (symbol arg)
    (arglist (encode-arglist arg))))

(defstruct (keyword-arg
            (:conc-name keyword-arg.)
            (:constructor make-keyword-arg (keyword arg-name default-arg)))
  keyword
  arg-name
  default-arg)

(defun decode-keyword-arg (arg)
  "Decode a keyword item of formal argument list.
Return three values: keyword, argument name, default arg."
  (flet ((intern-as-keyword (arg)
           (intern (etypecase arg
                     (symbol (symbol-name arg))
                     (arglist-dummy (arglist-dummy.string-representation arg)))
                   keyword-package)))
    (cond ((or (symbolp arg) (arglist-dummy-p arg))
           (make-keyword-arg (intern-as-keyword arg) arg nil))
          ((and (consp arg)
                (consp (car arg)))
           (make-keyword-arg (caar arg)
                             (decode-required-arg (cadar arg))
                             (cadr arg)))
          ((consp arg)
           (make-keyword-arg (intern-as-keyword (car arg)) (car arg) (cadr arg)))
          (t
           (error "Bad keyword item of formal argument list")))))

(defun encode-keyword-arg (arg)
  (cond
    ((arglist-p (keyword-arg.arg-name arg))
     ;; Destructuring pattern
     (let ((keyword/name (list (keyword-arg.keyword arg)
                               (encode-required-arg
                                (keyword-arg.arg-name arg)))))
       (if (keyword-arg.default-arg arg)
           (list keyword/name
                 (keyword-arg.default-arg arg))
           (list keyword/name))))
    ((eql (intern (symbol-name (keyword-arg.arg-name arg))
                  keyword-package)
          (keyword-arg.keyword arg))
     (if (keyword-arg.default-arg arg)
         (list (keyword-arg.arg-name arg)
               (keyword-arg.default-arg arg))
         (keyword-arg.arg-name arg)))
    (t
     (let ((keyword/name (list (keyword-arg.keyword arg)
                               (keyword-arg.arg-name arg))))
       (if (keyword-arg.default-arg arg)
           (list keyword/name
                 (keyword-arg.default-arg arg))
           (list keyword/name))))))

(progn
  (assert (equalp (decode-keyword-arg 'x)
                  (make-keyword-arg :x 'x nil)))
  (assert (equalp (decode-keyword-arg '(x t))
                  (make-keyword-arg :x 'x t)))
  (assert (equalp (decode-keyword-arg '((:x y)))
                  (make-keyword-arg :x 'y nil)))
  (assert (equalp (decode-keyword-arg '((:x y) t))
                  (make-keyword-arg :x 'y t))))

;;; FIXME suppliedp?
(defstruct (optional-arg
            (:conc-name optional-arg.)
            (:constructor make-optional-arg (arg-name default-arg)))
  arg-name
  default-arg)

(defun decode-optional-arg (arg)
  "Decode an optional item of a formal argument list.
Return an OPTIONAL-ARG structure."
  (etypecase arg
    (symbol        (make-optional-arg arg nil))
    (arglist-dummy (make-optional-arg arg nil))
    (list          (make-optional-arg (decode-required-arg (car arg))
                                      (cadr arg)))))

(defun encode-optional-arg (optional-arg)
  (if (or (optional-arg.default-arg optional-arg)
          (arglist-p (optional-arg.arg-name optional-arg)))
      (list (encode-required-arg
             (optional-arg.arg-name optional-arg))
            (optional-arg.default-arg optional-arg))
      (optional-arg.arg-name optional-arg)))

(progn
  (assert (equalp (decode-optional-arg 'x)
                  (make-optional-arg 'x nil)))
  (assert (equalp (decode-optional-arg '(x t))
                  (make-optional-arg 'x t))))

(define-modify-macro nreversef () nreverse "Reverse the list in PLACE.")

(defun decode-arglist (arglist)
  "Parse the list ARGLIST and return an ARGLIST structure."
  (etypecase arglist
    ((eql :not-available) (return-from decode-arglist
                            :not-available))
    (list))
  (loop
    with mode = nil
    with result = (make-arglist)
    for arg = (if (consp arglist)
                  (pop arglist)
                  (progn
                    (setf mode '&rest)
                    arglist))
    do (cond
         ((eql mode '&unknown-junk)
          ;; don't leave this mode -- we don't know how the arglist
          ;; after unknown lambda-list keywords is interpreted
          (push arg (arglist.unknown-junk result)))
         ((eql arg '&allow-other-keys)
          (setf (arglist.allow-other-keys-p result) t))
         ((eql arg '&key)
          (setf (arglist.key-p result) t
                mode arg))
         ((memq arg '(&optional &rest &body &aux))
          (setq mode arg))
         ((memq arg '(&whole &environment))
          (setq mode arg)
          (push arg (arglist.known-junk result)))
         ((and (symbolp arg)
               (string= (symbol-name arg) (string '#:&any))) ; may be interned
          (setf (arglist.any-p result) t) ;  in any *package*.
          (setq mode '&any))
         ((memq arg lambda-list-keywords)
          (setq mode '&unknown-junk)
          (push arg (arglist.unknown-junk result)))
         (t
          (ecase mode
            (&key
               (push (decode-keyword-arg arg)
                     (arglist.keyword-args result)))
            (&optional
               (push (decode-optional-arg arg)
                     (arglist.optional-args result)))
            (&body
               (setf (arglist.body-p result) t
                     (arglist.rest result) arg))
            (&rest
               (setf (arglist.rest result) arg))
            (&aux
               (push (decode-optional-arg arg)
                     (arglist.aux-args result)))
            ((nil)
               (push (decode-required-arg arg)
                     (arglist.required-args result)))
            ((&whole &environment)
               (setf mode nil)
               (push arg (arglist.known-junk result)))
            (&any
               (push arg (arglist.any-args result))))))
    until (atom arglist)
    finally (nreversef (arglist.required-args result))
    finally (nreversef (arglist.optional-args result))
    finally (nreversef (arglist.keyword-args result))
    finally (nreversef (arglist.aux-args result))
    finally (nreversef (arglist.any-args result))
    finally (nreversef (arglist.known-junk result))
    finally (nreversef (arglist.unknown-junk result))
    finally (assert (or (and (not (arglist.key-p result)) 
                             (not (arglist.any-p result)))
                        (exactly-one-p (arglist.key-p result) 
                                       (arglist.any-p result))))
    finally (return result)))

(defun encode-arglist (decoded-arglist)
  (append (mapcar #'encode-required-arg (arglist.required-args decoded-arglist))
          (when (arglist.optional-args decoded-arglist)
            '(&optional))
          (mapcar #'encode-optional-arg (arglist.optional-args decoded-arglist))
          (when (arglist.key-p decoded-arglist)
            '(&key))
          (mapcar #'encode-keyword-arg (arglist.keyword-args decoded-arglist))
          (when (arglist.allow-other-keys-p decoded-arglist)
            '(&allow-other-keys))
          (when (arglist.any-args decoded-arglist)
            `(&any ,@(arglist.any-args decoded-arglist)))
          (cond ((not (arglist.rest decoded-arglist))
                 '())
                ((arglist.body-p decoded-arglist)
                 `(&body ,(arglist.rest decoded-arglist)))
                (t
                 `(&rest ,(arglist.rest decoded-arglist))))
          (when (arglist.aux-args decoded-arglist)
            `(&aux ,(arglist.aux-args decoded-arglist)))
          (arglist.known-junk decoded-arglist)
          (arglist.unknown-junk decoded-arglist)))

;;;; Arglist Enrichment

(defun arglist-keywords (lambda-list)
  "Return the list of keywords in ARGLIST.
As a secondary value, return whether &allow-other-keys appears."
  (let ((decoded-arglist (decode-arglist lambda-list)))
    (values (arglist.keyword-args decoded-arglist)
	    (arglist.allow-other-keys-p decoded-arglist))))


(defun methods-keywords (methods)
  "Collect all keywords in the arglists of METHODS.
As a secondary value, return whether &allow-other-keys appears somewhere."
  (let ((keywords '())
	(allow-other-keys nil))
    (dolist (method methods)
      (multiple-value-bind (kw aok)
	  (arglist-keywords
	   (swank-mop:method-lambda-list method))
	(setq keywords (remove-duplicates (append keywords kw)
                                          :key #'keyword-arg.keyword)
	      allow-other-keys (or allow-other-keys aok))))
    (values keywords allow-other-keys)))

(defun generic-function-keywords (generic-function)
  "Collect all keywords in the methods of GENERIC-FUNCTION.
As a secondary value, return whether &allow-other-keys appears somewhere."
  (methods-keywords
   (swank-mop:generic-function-methods generic-function)))

(defun applicable-methods-keywords (generic-function arguments)
  "Collect all keywords in the methods of GENERIC-FUNCTION that are
applicable for argument of CLASSES.  As a secondary value, return
whether &allow-other-keys appears somewhere."
  (methods-keywords
   (multiple-value-bind (amuc okp)
       (swank-mop:compute-applicable-methods-using-classes
        generic-function (mapcar #'class-of arguments))
     (if okp
         amuc
         (compute-applicable-methods generic-function arguments)))))

(defgeneric extra-keywords (operator &rest args)
   (:documentation "Return a list of extra keywords of OPERATOR (a
symbol) when applied to the (unevaluated) ARGS.
As a secondary value, return whether other keys are allowed.
As a tertiary value, return the initial sublist of ARGS that was needed
to determine the extra keywords."))

;;; We make sure that symbol-from-KEYWORD-using keywords come before
;;; symbol-from-arbitrary-package-using keywords. And we sort the
;;; latter according to how their home-packages relate to *PACKAGE*.
;;;
;;; Rationale is to show those key parameters first which make most
;;; sense in the current context. And in particular: to put
;;; implementation-internal stuff last.
;;;
;;; This matters tremendeously on Allegro in combination with
;;; AllegroCache as that does some evil tinkering with initargs,
;;; obfuscating the arglist of MAKE-INSTANCE.
;;;

(defmethod extra-keywords :around (op &rest args)
  (declare (ignorable op args))
  (multiple-value-bind (keywords aok enrichments) (call-next-method)
    (values (sort-extra-keywords keywords) aok enrichments)))

(defun make-package-comparator (reference-packages)
  "Returns a two-argument test function which compares packages
according to their used-by relation with REFERENCE-PACKAGES. Packages
will be sorted first which appear first in the PACKAGE-USE-LIST of the
reference packages."
  (let ((package-use-table (make-hash-table :test 'eq)))
    ;; Walk the package dependency graph breadth-fist, and fill
    ;; PACKAGE-USE-TABLE accordingly.
    (loop with queue = (copy-list reference-packages)
	  with bfn   = 0		; Breadth-First Number
	  for p      = (pop queue)
	  unless (gethash p package-use-table)
	    do      (setf (gethash p package-use-table) (shiftf bfn (1+ bfn)))
	    and do  (setf queue (nconc queue (copy-list (package-use-list p))))
	  while queue)
    #'(lambda (p1 p2)
	(let ((bfn1 (gethash p1 package-use-table))
	      (bfn2 (gethash p2 package-use-table)))
	  (cond ((and bfn1 bfn2) (<= bfn1 bfn2))
		(bfn1            bfn1)
		(bfn2            nil)	; p2 is used, p1 not
		(t (string<= (package-name p1) (package-name p2))))))))

(defun sort-extra-keywords (kwds)
  (stable-sort kwds (make-package-comparator (list keyword-package *package*))
               :key (compose #'symbol-package #'keyword-arg.keyword)))

(defun keywords-of-operator (operator)
  "Return a list of KEYWORD-ARGs that OPERATOR accepts.
This function is useful for writing EXTRA-KEYWORDS methods for
user-defined functions which are declared &ALLOW-OTHER-KEYS and which
forward keywords to OPERATOR."
  (with-available-arglist (arglist) (arglist-from-form (ensure-list operator))
    (values (arglist.keyword-args arglist)
            (arglist.allow-other-keys-p arglist))))

(defmethod extra-keywords (operator &rest args)
  ;; default method
  (declare (ignore args))
  (let ((symbol-function (symbol-function operator)))
    (if (typep symbol-function 'generic-function)
        (generic-function-keywords symbol-function)
        nil)))

(defun class-from-class-name-form (class-name-form)
  (when (and (listp class-name-form)
             (= (length class-name-form) 2)
             (eq (car class-name-form) 'quote))
    (let* ((class-name (cadr class-name-form))
           (class (find-class class-name nil)))
      (when (and class
                 (not (swank-mop:class-finalized-p class)))
        ;; Try to finalize the class, which can fail if
        ;; superclasses are not defined yet
        (handler-case (swank-mop:finalize-inheritance class)
          (program-error (c)
            (declare (ignore c)))))
      class)))

(defun extra-keywords/slots (class)
  (multiple-value-bind (slots allow-other-keys-p)
      (if (swank-mop:class-finalized-p class)
          (values (swank-mop:class-slots class) nil)
          (values (swank-mop:class-direct-slots class) t))
    (let ((slot-init-keywords
           (loop for slot in slots append
                 (mapcar (lambda (initarg)
                           (make-keyword-arg
                            initarg
                            (swank-mop:slot-definition-name slot)
                            (swank-mop:slot-definition-initform slot)))
                         (swank-mop:slot-definition-initargs slot)))))
      (values slot-init-keywords allow-other-keys-p))))

(defun extra-keywords/make-instance (operator &rest args)
  (declare (ignore operator))
  (unless (null args)
    (let* ((class-name-form (car args))
           (class (class-from-class-name-form class-name-form)))
      (when class
        (multiple-value-bind (slot-init-keywords class-aokp)
            (extra-keywords/slots class)
          (multiple-value-bind (allocate-instance-keywords ai-aokp)
              (applicable-methods-keywords
               #'allocate-instance (list class))
            (multiple-value-bind (initialize-instance-keywords ii-aokp)
                (ignore-errors
                  (applicable-methods-keywords
                   #'initialize-instance (list (swank-mop:class-prototype class))))
              (multiple-value-bind (shared-initialize-keywords si-aokp)
                  (ignore-errors
                    (applicable-methods-keywords
                     #'shared-initialize (list (swank-mop:class-prototype class) t)))
                (values (append slot-init-keywords
                                allocate-instance-keywords
                                initialize-instance-keywords
                                shared-initialize-keywords)
                        (or class-aokp ai-aokp ii-aokp si-aokp)
                        (list class-name-form))))))))))

(defun extra-keywords/change-class (operator &rest args)
  (declare (ignore operator))
  (unless (null args)
    (let* ((class-name-form (car args))
           (class (class-from-class-name-form class-name-form)))
      (when class
        (multiple-value-bind (slot-init-keywords class-aokp)
            (extra-keywords/slots class)
          (declare (ignore class-aokp))
          (multiple-value-bind (shared-initialize-keywords si-aokp)
              (ignore-errors
                (applicable-methods-keywords
                 #'shared-initialize (list (swank-mop:class-prototype class) t)))
            ;; FIXME: much as it would be nice to include the
            ;; applicable keywords from
            ;; UPDATE-INSTANCE-FOR-DIFFERENT-CLASS, I don't really see
            ;; how to do it: so we punt, always declaring
            ;; &ALLOW-OTHER-KEYS.
            (declare (ignore si-aokp))
            (values (append slot-init-keywords shared-initialize-keywords)
                    t
                    (list class-name-form))))))))

(defmethod extra-keywords ((operator (eql 'make-instance))
                           &rest args)
  (multiple-value-or (apply #'extra-keywords/make-instance operator args)
                     (call-next-method)))

(defmethod extra-keywords ((operator (eql 'make-condition))
                           &rest args)
  (multiple-value-or (apply #'extra-keywords/make-instance operator args)
                     (call-next-method)))

(defmethod extra-keywords ((operator (eql 'error))
                           &rest args)
  (multiple-value-or (apply #'extra-keywords/make-instance operator args)
                     (call-next-method)))

(defmethod extra-keywords ((operator (eql 'signal))
                           &rest args)
  (multiple-value-or (apply #'extra-keywords/make-instance operator args)
                     (call-next-method)))

(defmethod extra-keywords ((operator (eql 'warn))
                           &rest args)
  (multiple-value-or (apply #'extra-keywords/make-instance operator args)
                     (call-next-method)))

(defmethod extra-keywords ((operator (eql 'cerror))
                           &rest args)
  (multiple-value-bind (keywords aok determiners)
      (apply #'extra-keywords/make-instance operator
             (cdr args))
    (if keywords
        (values keywords aok
                (cons (car args) determiners))
        (call-next-method))))

(defmethod extra-keywords ((operator (eql 'change-class))
                           &rest args)
  (multiple-value-bind (keywords aok determiners)
      (apply #'extra-keywords/change-class operator (cdr args))
    (if keywords
        (values keywords aok
                (cons (car args) determiners))
        (call-next-method))))

(defun enrich-decoded-arglist-with-keywords (decoded-arglist keywords allow-other-keys-p)
  "Modify DECODED-ARGLIST using KEYWORDS and ALLOW-OTHER-KEYS-P."
  (when keywords
    (setf (arglist.key-p decoded-arglist) t)
    (setf (arglist.keyword-args decoded-arglist)
          (remove-duplicates
           (append (arglist.keyword-args decoded-arglist)
                   keywords)
           :key #'keyword-arg.keyword)))
  (setf (arglist.allow-other-keys-p decoded-arglist)
        (or (arglist.allow-other-keys-p decoded-arglist)
            allow-other-keys-p)))

(defun enrich-decoded-arglist-with-extra-keywords (decoded-arglist form)
  "Determine extra keywords from the function call FORM, and modify
DECODED-ARGLIST to include them.  As a secondary return value, return
the initial sublist of ARGS that was needed to determine the extra
keywords.  As a tertiary return value, return whether any enrichment
was done."
  (multiple-value-bind (extra-keywords extra-aok determining-args)
      (apply #'extra-keywords form)
    ;; enrich the list of keywords with the extra keywords
    (enrich-decoded-arglist-with-keywords decoded-arglist
                                          extra-keywords extra-aok)
    (values decoded-arglist
            determining-args
            (or extra-keywords extra-aok))))

(defgeneric compute-enriched-decoded-arglist (operator-form argument-forms)
  (:documentation
   "Return three values: DECODED-ARGLIST, DETERMINING-ARGS, and
ANY-ENRICHMENT, just like enrich-decoded-arglist-with-extra-keywords.
If the arglist is not available, return :NOT-AVAILABLE."))

(defmethod compute-enriched-decoded-arglist (operator-form argument-forms)
  (with-available-arglist (decoded-arglist) 
      (decode-arglist (arglist operator-form))
    (enrich-decoded-arglist-with-extra-keywords decoded-arglist
                                                (cons operator-form
                                                      argument-forms))))

(defmethod compute-enriched-decoded-arglist ((operator-form (eql 'with-open-file))
                                             argument-forms)
  (declare (ignore argument-forms))
  (multiple-value-bind (decoded-arglist determining-args)
      (call-next-method)
    (let ((first-arg (first (arglist.required-args decoded-arglist)))
          (open-arglist (compute-enriched-decoded-arglist 'open nil)))
      (when (and (arglist-p first-arg) (arglist-p open-arglist))
        (enrich-decoded-arglist-with-keywords
         first-arg
         (arglist.keyword-args open-arglist)
         nil)))
    (values decoded-arglist determining-args t)))

(defmethod compute-enriched-decoded-arglist ((operator-form (eql 'apply))
                                             argument-forms)
  (let ((function-name-form (car argument-forms)))
    (when (and (listp function-name-form)
               (length= function-name-form 2)
               (memq (car function-name-form) '(quote function)))
      (let ((function-name (cadr function-name-form)))
        (when (valid-operator-symbol-p function-name)
          (let ((function-arglist
                 (compute-enriched-decoded-arglist function-name
                                                   (cdr argument-forms))))
            (return-from compute-enriched-decoded-arglist
              (values (make-arglist :required-args
                                    (list 'function)
                                    :optional-args
                                    (append
                                     (mapcar #'(lambda (arg)
                                                 (make-optional-arg arg nil))
                                             (arglist.required-args function-arglist))
                                     (arglist.optional-args function-arglist))
                                    :key-p
                                    (arglist.key-p function-arglist)
                                    :keyword-args
                                    (arglist.keyword-args function-arglist)
                                    :rest
                                    'args
                                    :allow-other-keys-p
                                    (arglist.allow-other-keys-p function-arglist))
                      (list function-name-form)
                      t)))))))
  (call-next-method))

(defun delete-given-args (decoded-arglist args)
  "Delete given ARGS from DECODED-ARGLIST."
  (macrolet ((pop-or-return (list)
	       `(if (null ,list)
		    (return-from do-decoded-arglist)
		    (pop ,list))))
    (do-decoded-arglist decoded-arglist
      (&required ()
       (pop-or-return args)
       (pop (arglist.required-args decoded-arglist)))
      (&optional ()
       (pop-or-return args)
       (pop (arglist.optional-args decoded-arglist)))
      (&key (keyword)
       ;; N.b. we consider a keyword to be given only when the keyword
       ;; _and_ a value has been given for it.
       (loop for (key value) on args by #'cddr
	     when (and (eq keyword key) value)
	       do (setf (arglist.keyword-args decoded-arglist)
			(remove keyword (arglist.keyword-args decoded-arglist)
				:key #'keyword-arg.keyword))))))
  decoded-arglist)

(defun remove-given-args (decoded-arglist args)
  ;; FIXME: We actually needa deep copy here.
  (delete-given-args (copy-arglist decoded-arglist) args))

;;;; Arglist Retrieval

(defun arglist-from-form (form)
  (if (null form)
      :not-available
      (arglist-dispatch (car form) (cdr form))))

(defgeneric arglist-dispatch (operator arguments)
  ;; Default method
  (:method (operator arguments)
    (unless (and (symbolp operator) (valid-operator-symbol-p operator))
      (return-from arglist-dispatch :not-available))

    (multiple-value-bind (decoded-arglist determining-args)
        (compute-enriched-decoded-arglist operator arguments)
      (with-available-arglist (arglist) decoded-arglist
        ;; replace some formal args by determining actual args
        (setf arglist (delete-given-args arglist determining-args))
        (setf (arglist.provided-args arglist) determining-args)
        arglist))))

(defmethod arglist-dispatch ((operator (eql 'defmethod)) arguments)
  (match (cons operator arguments)
    (('defmethod (#'valid-function-name-p gf-name) . _)
     (let ((gf (fdefinition gf-name)))
       (when (typep gf 'generic-function)
         (with-available-arglist (arglist) (decode-arglist (arglist gf))
           (return-from arglist-dispatch
             (make-arglist :provided-args (list gf-name)
                           :required-args (list arglist)
                           :rest "body" :body-p t))))))
    (_)) ; Fall through
  (call-next-method))

(defmethod arglist-dispatch ((operator (eql 'define-compiler-macro)) arguments)
  (match (cons operator arguments)
    (('define-compiler-macro (#'valid-function-name-p gf-name) . _)
     (let ((gf (fdefinition gf-name)))
       (with-available-arglist (arglist) (decode-arglist (arglist gf))
         (return-from arglist-dispatch
           (make-arglist :provided-args (list gf-name)
                         :required-args (list arglist)
                         :rest "body" :body-p t)))))
    (_)) ; Fall through
  (call-next-method))


(defmethod arglist-dispatch ((operator (eql 'eval-when)) arguments)
  (declare (ignore arguments))
    (let ((eval-when-args '(:compile-toplevel :load-toplevel :execute)))
    (make-arglist 
     :required-args (list (make-arglist :any-p t :any-args eval-when-args))
     :rest '#:body :body-p t)))


(defmethod arglist-dispatch ((operator (eql 'declare)) arguments)
  (flet ((arglist-for-type-declaration (identifier typespec rest-var-name)
           (with-available-arglist (typespec-arglist) 
               (decoded-arglist-for-type-specifier typespec)
             (make-arglist 
              :required-args (list (make-arglist 
                                    :provided-args (list identifier)
                                    :required-args (list typespec-arglist)
                                    :rest rest-var-name))))))
    (match (cons operator (last arguments))
      (('declare ('type (#'consp typespec) . decl-args)) 
       (arglist-for-type-declaration 'type typespec '#:variables))
      (('declare ('ftype (#'consp typespec) . decl-args)) 
       (arglist-for-type-declaration 'ftype typespec '#:function-names))
      (('declare ((#'consp typespec) . decl-args)) 
       (with-available-arglist (typespec-arglist) 
           (decoded-arglist-for-type-specifier typespec)
         (make-arglist
          :required-args (list (make-arglist
                                :required-args (list typespec-arglist)
                                :rest '#:vars)))))
      (('declare (decl-identifier . decl-args))
       (decoded-arglist-for-declaration decl-identifier decl-args))
      (_ (make-arglist :rest '#:declaration-specifiers)))))

(defun decoded-arglist-for-declaration (decl-identifier decl-args)
  (declare (ignore decl-args))
    (with-available-arglist (arglist)
      (decode-arglist (declaration-arglist decl-identifier))
    (setf (arglist.provided-args arglist) (list decl-identifier))
    (make-arglist :required-args (list arglist))))

(defun decoded-arglist-for-type-specifier (type-specifier)
  (when (consp type-specifier)
    (setq type-specifier (car type-specifier)))
    (with-available-arglist (arglist)
      (decode-arglist (type-specifier-arglist type-specifier))
    (setf (arglist.provided-args arglist) (list type-specifier))
    arglist))


;;; Slimefuns
  
(defslimefun variable-desc-for-echo-area (variable-name)
  "Return a short description of VARIABLE-NAME, or NIL."
    (with-buffer-syntax ()
    (let ((sym (parse-symbol variable-name)))
      (if (and sym (boundp sym))
          (let ((*print-pretty* t) (*print-level* 4)
                (*print-length* 10) (*print-lines* 1)
		(*print-readably* nil))
	    (call/truncated-output-to-string
	     75 (lambda (s)
		  (format s "~A => ~S" sym (symbol-value sym)))))))))

;;; We work on a RAW-FORM, or BUFFER-FORM, which represent the form at
;;; user's point in Emacs. A RAW-FORM looks like
;;;
;;;       ("FOO" ("BAR" ...) "QUUX" ("ZURP" SWANK::%CURSOR-MARKER%))
;;;
;;; The expression before the cursor marker is the expression where
;;; user's cursor points at. An explicit marker is necessary to
;;; disambiguate between
;;;
;;;       ("IF" ("PRED")
;;;             ("F" "X" "Y" %CURSOR-MARKER%))
;;;
;;; and
;;;       ("IF" ("PRED")
;;;             ("F" "X" "Y") %CURSOR-MARKER%)

;;; Notice that for a form like (FOO (BAR |) QUUX), where | denotes
;;; user's point, the following should be sent ("FOO" ("BAR" ""
;;; %CURSOR-MARKER%)). Only the forms up to point should be
;;; considered.

(defslimefun arglist-for-echo-area (raw-form &key print-right-margin print-lines)
  "Return a string representing the arglist for the deepest subform in
RAW-FORM that does have an arglist. The highlighted parameter is
wrapped in ===> X <===."
  (with-buffer-syntax ()
    (multiple-value-bind (form arglist)
        (find-subform-with-arglist (parse-raw-form raw-form))
      (with-available-arglist (arglist) arglist
        (destructuring-bind (operator . args) form
          (decoded-arglist-to-string
           arglist
           :print-right-margin print-right-margin
           :print-lines print-lines
           :operator operator
           :highlight (arglist-path-to-parameter arglist args)))))))

(defslimefun complete-form (raw-form)
    "Read FORM-STRING in the current buffer package, then complete it
  by adding a template for the missing arguments."
    (with-buffer-syntax ()
    (multiple-value-bind (arglist provided-args)
        (find-immediately-containing-arglist (parse-raw-form raw-form))
      (with-available-arglist (arglist) arglist
        (decoded-arglist-to-template-string 
         (delete-given-args arglist 
                            (remove-if #'empty-arg-p provided-args
                                       :from-end t :count 1))
         :prefix "" :suffix "")))))

(defslimefun completions-for-keyword (keyword-string raw-form)
  "Return a list of possible completions for KEYWORD-STRING relative
to the context provided by RAW-FORM."
  (with-buffer-syntax ()
    (with-available-arglist (arglist)
        (find-immediately-containing-arglist (parse-raw-form raw-form))
      ;; It would be possible to complete keywords only if we are in
      ;; a keyword position, but it is not clear if we want that.
      (let* ((keywords
              (append (mapcar #'keyword-arg.keyword
                              (arglist.keyword-args arglist))
                      (remove-if-not #'keywordp (arglist.any-args arglist))))
             (keyword-name
              (tokenize-symbol keyword-string))
             (matching-keywords
              (find-matching-symbols-in-list
               keyword-name keywords (make-compound-prefix-matcher #\-)))
             (converter (completion-output-symbol-converter keyword-string))
             (strings
              (mapcar converter
                      (mapcar #'symbol-name matching-keywords)))
             (completion-set
              (format-completion-set strings nil "")))
        (list completion-set
              (longest-compound-prefix completion-set))))))

(defparameter +cursor-marker+ '%cursor-marker%)

(defun find-subform-with-arglist (form)
  "Returns two values: the appropriate subform of FORM which is
closest to the +CURSOR-MARKER+ and whose operator is valid and has an
arglist. Second value is the arglist. The +CURSOR-MARKER+ is removed
from the subform returned.

This function takes local function and macro definitions appearing in
FORM into account."
  (labels 
      ((yield (form local-ops)
         (let ((form (remove-from-tree +cursor-marker+ form)))
           (values form
                   (let ((entry (assoc (car form) local-ops)))
                     (if entry
                         (decode-arglist (cdr entry))
                         (arglist-from-form form))))))
       (operator-p (operator local-ops)
         (and (symbolp operator)
              (or (valid-operator-symbol-p operator)
                  (assoc operator local-ops :test #'eq))))
       (grovel-form (form local-ops)
         (assert (listp form))
         (destructuring-bind (operator . args) form
           (declare (ignore args))
           ;; N.b. the user's cursor is at the rightmost, deepest
           ;; subform right before +CURSOR-MARKER+.
           (let ((last-subform (car (last form))))
             (cond
               ((eq last-subform +cursor-marker+)
                (if (operator-p operator local-ops)
                    (yield form local-ops)
                    (values nil :not-available)))
               ((not (operator-p operator local-ops))
                (grovel-form last-subform local-ops))
               ;; Make sure to pick up the arglists of local
               ;; function/macro definitions.
               ((memq operator '(cl:flet cl:labels cl:macrolet))
                (multiple-value-or (grovel-form last-subform 
                                                (nconc (extract-local-op-arglists form)
                                                       local-ops))
                                   (yield form local-ops)))
               ;; Some typespecs clash with function names, so we make
               ;; sure to bail out early.
               ((eq operator 'cl:declare)
                (yield form local-ops))
               ;; Mostly uninteresting, hence skip.
               ((memq operator '(cl:quote cl:function))
                nil)
               (t
                (multiple-value-or (grovel-form last-subform local-ops)
                                   (yield form local-ops))))))))
    (grovel-form form '())))

(defun extract-local-op-arglists (form)
  ;; FIXME: Take recursive scope of LABELS into account.
  (if (null (cddr form))
      nil
      (loop for (name arglist . nil) in (second form)
            when arglist
              collect (cons name arglist))))

(defun find-immediately-containing-arglist (form)
  "Returns the arglist closest to +CURSOR-MARKER+ in form. This may be
an implicit, nested arglist; e.g. on (WITH-OPEN-FILE (X))."
  (multiple-value-bind (form arglist) (find-subform-with-arglist form)
    (if (eql arglist :not-available)
        (values :not-available nil)
        (destructuring-bind (operator . args) form
          (declare (ignore operator))
          (let* ((path (arglist-path-to-nested-arglist arglist args))
                 (argl (apply #'arglist-ref arglist path))
                 (args (apply #'provided-arguments-ref args arglist path)))
            (values argl args))))))

(defun arglist-path-to-parameter (arglist provided-args)
  "Returns a path to the arglist parameter that the last argument in
PROVIDED-ARGS would take up on application."
  (let* ((path (arglist-path-to-nested-arglist arglist provided-args))
         (argl (apply #'arglist-ref arglist path))
         (provided-arg (apply #'provided-arguments-ref provided-args arglist path)))
    (nconc path (list (compute-arglist-index argl provided-arg)))))


(defun arglist-path-to-nested-arglist (arglist provided-args)
  "Returns a path to the (nested) arglist that still contains the last
argument in PROVIDED-ARGS."
  (let ((idx (compute-arglist-index arglist provided-args)))
    (when idx
      (let ((arg (arglist-ref arglist idx))
            (provided-arg (provided-arguments-ref provided-args arglist idx)))
        (if (and (arglist-p arg) (listp provided-arg))
            (cons idx (arglist-path-to-nested-arglist arg provided-arg))
            nil)))))

(defun compute-arglist-index (arglist provided-args)
  "Returns the index of ARGLIST pertaining to the last argument in
PROVIDED-ARGUMENTS."
  (let ((arg-index (1- (length provided-args)))
        (positional-args# (positional-args-number arglist)))
    (cond
      ((< arg-index 0) nil)
      ((< arg-index positional-args#) arg-index)        ; required + optional
      ((not (arglist.key-p arglist))  positional-args#) ; rest + body
      (t                                                ; key
       ;; Find last provided &key parameter
       (let ((provided-keys (subseq provided-args positional-args#)))
         (loop for (key nil . rest) on provided-keys by #'cddr
               when (null rest) 
                 return (and (symbolp key) key)))))))

(defun arglist-ref (arglist &rest indices)
  "Returns the parameter in ARGLIST along the INDICIES path. Numbers
represent positional parameters (required, optional), keywords
represent key parameters."
  (flet ((ref-positional-arg (arglist index)
           (check-type index (integer 0 *))
           (with-struct (arglist. provided-args required-args optional-args rest) 
               arglist
             (loop for args in (list provided-args required-args 
                                     (mapcar #'optional-arg.arg-name optional-args))
                   for args# = (length args)
                   if (< index args#)
                     return (nth index args)
                   else
                     do (decf index args#)
                   finally (return (or rest nil)))))
         (ref-keyword-arg (arglist keyword)
           (assert (symbolp keyword) (keyword))
           (do-decoded-arglist arglist
             (&key (kw arg) (when (eq kw keyword)
                              (return-from ref-keyword-arg arg))))
           nil))
    (dolist (index indices)
      (assert (arglist-p arglist))
      (setq arglist (if (numberp index)
                        (ref-positional-arg arglist index)
                        (ref-keyword-arg arglist index))))
    arglist))

(defun provided-arguments-ref (provided-args arglist &rest indices)
  "Returns the argument in PROVIDED-ARGUMENT along the INDICES path
relative to ARGLIST."
  (flet ((ref (provided-args arglist index)
           (if (numberp index)
               (nth index provided-args)
               (let ((provided-keys (subseq provided-args (positional-args-number arglist))))
                 (loop for (key value) on provided-keys
                       when (eq key index)
                         return value)))))
    (dolist (idx indices)
      (setq provided-args (ref provided-args arglist idx))
      (setq arglist (arglist-ref arglist idx)))
    provided-args))

(defun positional-args-number (arglist)
  (+ (length (arglist.provided-args arglist))
     (length (arglist.required-args arglist))
     (length (arglist.optional-args arglist))))

(defun parse-raw-form (raw-form)
  "Parse a RAW-FORM into a Lisp form. I.e. substitute strings by
symbols if already interned. For strings not already interned, use
ARGLIST-DUMMY."
  (unless (null raw-form)
    (loop for element in raw-form 
          collect (etypecase element
                    (string (read-conversatively element))
                    (list   (parse-raw-form element))
                    (symbol (prog1 element
                              ;; Comes after list, so ELEMENT can't be NIL.
                              (assert (eq element +cursor-marker+))))))))

(defun read-conversatively (string)
  "Tries to find the symbol that's represented by STRING.

If it can't, this either means that STRING does not represent a
symbol, or that the symbol behind STRING would have to be freshly
interned. Because this function is supposed to be called from the
automatic arglist display stuff from Slime, interning freshly
symbols is a big no-no.

In such a case (that no symbol could be found), an object of type
ARGLIST-DUMMY is returned instead, which works as a placeholder
datum for subsequent logics to rely on."
  (let* ((string  (string-left-trim '(#\Space #\Tab #\Newline) string))
         (length  (length string))
	 (prefix  (cond ((zerop length) nil)
                        ((eql (aref string 0) #\') :quote)
                        ((search "#'" string :end2 (min length 2)) :sharpquote)
                        (t nil))))
    (multiple-value-bind (symbol found?)
	(parse-symbol (case prefix
                        (:quote      (subseq string 1))
                        (:sharpquote (subseq string 2))
                        (t string)))
      (if found?
          (case prefix
            (:quote      `(quote ,symbol))
            (:sharpquote `(function ,symbol))
            (t symbol))
	  (make-arglist-dummy string)))))
  
(defun test-print-arglist ()
  (flet ((test (arglist string)
           (let* ((*package* (find-package :swank))
                  (actual  (decoded-arglist-to-string (decode-arglist arglist))))
             (unless (string= actual string)
               (warn "Test failed: ~S => ~S~%  Expected: ~S"
                     arglist actual string)))))
    (test '(function cons) "(function cons)")
    (test '(quote cons) "(quote cons)")
    (test '(&key (function #'+)) "(&key (function #'+))")
    (test '(&whole x y z) "(y z)")
    (test '(x &aux y z) "(x)")
    (test '(x &environment env y) "(x y)")
    (test '(&key ((function f))) "(&key ((function ..)))")
    (test '(eval-when (&any :compile-toplevel :load-toplevel :execute) &body body)
	  "(eval-when (&any :compile-toplevel :load-toplevel :execute) &body body)")
    (test '(declare (optimize &any (speed 1) (safety 1)))
	  "(declare (optimize &any (speed 1) (safety 1)))")
    ))

(defun test-arglist-ref ()
  (macrolet ((soft-assert (form)
               `(unless ,form
                  (warn "Assertion failed: ~S~%" ',form))))
    (let ((sample (decode-arglist '(x &key ((:k (y z)))))))
      (soft-assert (eq (arglist-ref sample 0)    'x))
      (soft-assert (eq (arglist-ref sample :k 0) 'y))
      (soft-assert (eq (arglist-ref sample :k 1) 'z))

      (soft-assert (eq (provided-arguments-ref '(a :k (b c)) sample 0)    'a))
      (soft-assert (eq (provided-arguments-ref '(a :k (b c)) sample :k 0) 'b))
      (soft-assert (eq (provided-arguments-ref '(a :k (b c)) sample :k 1) 'c)))))

(test-print-arglist)
(test-arglist-ref)

(provide :swank-arglists)
