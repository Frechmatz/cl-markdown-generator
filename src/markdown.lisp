
(in-package :cl-markdown-generator)

(defparameter *TAB* "    ")


;;
;; Holds the global status of the document
;; - current indentation
;; - if current line is blank
;; - etc.
;; The document-printer exists on compilation time
;;

(defvar *MARKDOWN-OUTPUT* nil)
(defvar *DOCUMENT-PRINTER* nil)


(defclass document-printer ()
  ((indentation :initform '())
   (is-new-line :initform t)
   (is-blank-line :initform t)))

(defmethod push-indentation ((w document-printer) str)
  (push str (slot-value w 'indentation)))

(defmethod pop-indentation ((w document-printer))
  (pop (slot-value w 'indentation)))

(defmethod emit-indentation ((w document-printer))
  (dolist (i (slot-value w 'indentation))
    (princ i *MARKDOWN-OUTPUT*)))

(defmethod emit-text ((w document-printer) str)
  (if (slot-value w 'is-new-line)
      (progn
	(setf (slot-value w 'is-new-line) nil)
	(setf (slot-value w 'is-blank-line) nil)
	(emit-indentation w)))
  (princ str *MARKDOWN-OUTPUT*))

(defmethod emit-linebreak ((w document-printer) &optional (force nil))
  (if (or force (not (slot-value w 'is-new-line)))
      (progn 
	(setf (slot-value w 'is-new-line) t)
	(princ #\newline *MARKDOWN-OUTPUT*))))
		       
(defmethod emit-blank-line ((w document-printer))
  (if (not (slot-value w 'is-blank-line))
      (progn
	(emit-linebreak w)
	(emit-linebreak w t)
	(setf (slot-value w 'is-blank-line) t))))

;;
;; Compiler context class
;; Holds the output of the current markdown macro evaluation
;; Exists during macro-expansion-time and collects calls to
;; the document-printer.
;;

(defclass compiler-context ()
  ((ops :initform nil)))

(defmethod add-code-operation ((ctx compiler-context) operation)
  (push `(:code ,operation) (slot-value ctx 'ops)))

(defmethod add-push-indentation-operation ((ctx compiler-context) indentation)
  (push `(:push-indentation ,indentation) (slot-value ctx 'ops)))

(defmethod add-pop-indentation-operation ((ctx compiler-context))
  (push `(:pop-indentation) (slot-value ctx 'ops)))

(defmethod add-text-operation ((ctx compiler-context) text)
  (push `(:text ,text) (slot-value ctx 'ops)))

(defmethod add-linebreak-operation ((ctx compiler-context))
  (push '(:linebreak) (slot-value ctx 'ops)))

(defmethod add-blank-line-operation ((ctx compiler-context))
  (push '(:blank-line) (slot-value ctx 'ops)))

;;
;; Generate code
;;
(defmethod generate-code ((ctx compiler-context))
  (let ((c '()))
    (dolist (o (slot-value ctx 'ops))
      (cond
	((eql :text (first o)) 
	 (push `(emit-text *DOCUMENT-PRINTER* ,(second o)) c))
	((eql :push-indentation (first o)) 
	 (push `(push-indentation *DOCUMENT-PRINTER* ,(second o)) c))
	((eql :pop-indentation (first o)) 
	 (push `(pop-indentation *DOCUMENT-PRINTER*) c))
	((eql :linebreak (first o))
	 (push `(emit-linebreak *DOCUMENT-PRINTER*) c))
	((eql :blank-line (first o))
	 (push `(emit-blank-line *DOCUMENT-PRINTER*) c))
	((eql :code (first o))
	 (push (second o) c))
	(t (error (format nil "Cannot generate code for operator ~a" o)))))
    `(progn ,@c)))

;;
;; Opcode handling
;; keywords such as :h1 are resolved into a function named opcode-<keyword-name>
;;

(defun lookup-opcode-handler (keyword)
  (let ((name (concatenate 'string "OPCODE-" (symbol-name keyword))))
    (let ((s (find-symbol name :cl-markdown-generator)))
      (if (not s)
	  (error (format nil "No handler function defined for opcode: ~a" keyword))
	  (symbol-function s)))))

(defmacro call-opcode-handler (keyword compiler-context &rest args)
  `(apply (lookup-opcode-handler ,keyword) ,compiler-context ,@args))

;;
;; Form processing
;; parse form and add opcodes to the compiler context
;;

(defun process-form (compiler-context &rest args)
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (dolist (arg args)
    (cond
      ((stringp arg)
       (add-text-operation compiler-context arg))
      ((and (listp arg) (keywordp (first arg)))
       (call-opcode-handler (first arg) compiler-context (rest arg)))
      ;; todo: Handle code and other forms
      ((listp arg)
       (add-code-operation compiler-context arg))
      (t (error (format nil "Markdown error: Cannot handle input: ~a" arg))))))


;;
;; Opcode-Implementation helper macros
;;

(defmacro with-indentation (compiler-context indentation &body body)
  `(progn
     (add-push-indentation-operation ,compiler-context ,indentation)
     ,@body
     (add-pop-indentation-operation ,compiler-context)))

;;
;; Opcodes
;;

(defun opcode-h1 (compiler-context &rest args)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (add-blank-line-operation compiler-context)
  (add-text-operation compiler-context "# ")
  (apply #'process-form compiler-context args)
  (add-blank-line-operation compiler-context))


;; todo: set up list context
(defun opcode-list (compiler-context &rest args)
  "List is an element that doesn't emit any text nor adds indentation levels
   but creates a new item-count and list-style context"
  (apply #'process-form compiler-context args))

(defun opcode-list-item (compiler-context &rest args)
  (add-linebreak-operation compiler-context)
  ;; todo: format according to item-count/list-style context
  (add-text-operation compiler-context "* ")
  (with-indentation compiler-context *TAB* (apply #'process-form compiler-context args))
  (add-linebreak-operation compiler-context))

;;
;; Tree-Walker
;;

(defun walk-tree (&rest args)
  "Creates a new compiler context and processes the argument forms"
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (let ((compiler-ctx (make-instance 'compiler-context)))
    ;; parse tree and add opcodes to compiler context
    (apply #'process-form compiler-ctx args)
    ;; todo: Optimize generated code
    (generate-code compiler-ctx)))

;;
;; The API
;;

(defmacro with-markdown-output ((stream) &rest body)
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  `(let ((*MARKDOWN-OUTPUT* ,stream))
     ;; create a document-printer. the generated code consists
     ;; of calls against the document printer
     (let ((*DOCUMENT-PRINTER* (make-instance 'document-printer)))
       (macrolet ((markdown (&rest body)
		    ;;(declare (optimize (debug 3) (speed 0) (space 0)))
		    (apply #'walk-tree body))
		  (str (&rest body)
		    `(emit-text *DOCUMENT-PRINTER* ,@body))
		  (fmt (control-string &rest body)
		    `(emit-text *DOCUMENT-PRINTER* (format nil ,control-string ,@body)))
		  )
	 ;; generate code and insert it here
	 ,(apply #'walk-tree body)))))

(defmacro with-markdown-output-to-string ((var &optional string-form) &rest body)
  `(with-output-to-string (,var ,string-form)
     (with-markdown-output (,var) ,@body)))

#|
(defun test()
  (with-markdown-output (*STANDARD-OUTPUT*)
    (:h1 "Chapter 1")
    (:list (:list-item "1.1")
	   (:list-item "1.2"))
	   (dotimes (i 10) (markdown (:h1 (str "Next one"))))
	   ))

(test)
|#



