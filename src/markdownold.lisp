
;;
;; Todos:
;; - escaping of number-period-space sequence at start of line ('1986. What a great season')
;;

(in-package :cl-markdown-generator)

(defparameter *INDENT* "    ")

(defclass markdown-writer ()
  (
   (indentation :initform '())
   (is-new-line :initform t)
   (is-blank-line :initform t)
   ))

(defmethod emit-text ((w markdown-writer) str)
  (if (slot-value w 'is-new-line)
      (progn
	(setf (slot-value w 'is-new-line) nil)
	(setf (slot-value w 'is-blank-line) nil)
	(princ (get-indentation w))))
  (princ str))

(defmethod emit-linebreak ((w markdown-writer))
  (if (not (slot-value w 'is-new-line))
      (progn 
	(setf (slot-value w 'is-new-line) t)
	(princ #\Newline))))

(defmethod emit-blank-line ((w markdown-writer))
  (if (not (slot-value w 'is-blank-line))
      (progn 
	(emit-linebreak w)
	(emit-text w "")
	(emit-linebreak w)
	(setf (slot-value w 'is-blank-line) t)
	)))

(defmethod push-indentation ((w markdown-writer) str)
  (push str (slot-value w 'indentation)))

(defmethod pop-indentation ((w markdown-writer))
  (pop (slot-value w 'indentation)))

(defmethod get-indentation ((w markdown-writer))
  (apply #'concatenate 'string (reverse (slot-value w 'indentation))))

(defmacro with-indentation (indentation &body body)
  `(progn
     (push-indentation writer ,indentation)
     ,@body
     (pop-indentation writer)))

(defmacro with-blank-lines (&body body)
  `(progn
     (emit-blank-line writer)
     ,@body
     (emit-blank-line writer)))

(defmacro with-blank-line (&body body)
  `(progn
     (emit-blank-line writer)
     ,@body))

(defun get-opcode-handler (keyword)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (let ((name (concatenate 'string "OPCODE-" (symbol-name keyword))))
    ;;(format t "~%Looking up symbol ~a" name)
    (let ((s (find-symbol name)))
      ;;(format t "~%Symbol ~a found in package" name)
      (if (not s)
	  (error (format nil "No handler function defined for opcode: ~a" keyword))
	  (symbol-function s)))))

(defun markdown-sexp-p (o)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (cond
    ((not (listp o))
     nil)
    (t (keywordp (first o)))))

(defun process-form (writer &rest args)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (dolist (arg args)
    (cond
      ((stringp arg)
       (emit-text writer arg))
      ((markdown-sexp-p arg)
       (let ((handler (get-opcode-handler (first arg))))
	 (apply handler writer (rest arg))))
      ((listp arg)
       (apply #'process-form writer arg))
      (t (error (format nil "Cannot handle input: ~a" arg))))))


(defun opcode-paragraph (writer &rest args)
  (with-blank-lines (process-form writer args)))

(defun opcode-h1 (writer form)
  (with-blank-lines 
    (emit-text writer "# ")
    (process-form writer form)))


;; todo: set up list context
(defun opcode-list (writer &rest args)
  "List is an element that doesnt emit any text nor adds indentation levels
but creates a new item-count and list-style context"
  (process-form writer args))

(defun opcode-list-item (writer &rest args)
  (emit-linebreak writer)
  ;; todo: format according to item-count/list-style context
  (emit-text writer "* ")
  (with-indentation *INDENT* (process-form writer args)))

(defun opcode-code (writer &rest args)
  (emit-blank-line writer)
  (with-indentation *INDENT* (process-form writer args))
  (emit-linebreak writer))

(defun opcode-blockquote (writer &rest args)
  (emit-blank-line writer)
  (with-indentation "> " (process-form writer args)))

(defun opcode-em (writer form)
  (emit-text writer "*")
  (process-form writer form)
  (emit-text writer "*"))
  
(defun opcode-strong (writer form)
  (emit-text writer "**")
  (process-form writer form)
  (emit-text writer "**"))


(defun emit (writer &rest args)
  (format t "~%")
  (apply #'process-form writer args)
  (format t "~%"))

(defun test ()
  (emit
   (make-instance 'markdown-writer)
   '((:paragraph ("Paragraph 1")
      (:code "<Example code here>"))
     (:paragraph "Paragraph 2")
     (:h1 "Header 1")
     (:list
      (:list-item "Item 1 " (:strong "Strong")
       (:paragraph "First paragraph of Item 1")
       (:code "<Example code here>")
       (:paragraph "Second paragraph of Item 1")
       )
      (:list-item "Item 2"
       (:list
	(:list-item "Item 2.1" (:paragraph "First paragraph of 2.1") (:paragraph "Second paragraph of 2.1"))
	(:list-item "Item 2.2")))
      (:list-item "Item 3"
       (:code "<Example code here>")
       "Item 3 continued"
       )
      (:list-item "Item 4"
       (:code "<Example code here>"))
      (:list-item "Item 5" (:list (:list-item "4.1") (:list-item "4.2") (:list-item "4.3")))
      (:list-item "Item 6")
      (:list-item "Item 7"))
   (:h1 "Blockquotes")
   (:paragraph "Here some blockquote tests") 
   (:blockquote "Quote 1")
   )))

(defun simple-test ()
  (emit
   (make-instance 'markdown-writer)
   '(:paragraph ("Paragraph 1") (:paragraph "Paragraph 2"))))

;;(simple-test)
(test)


#|
(defmacro ollis-dispatch (opcodeName &rest args)
  `(,(intern opcodeName) ,@ args))

(ollis-dispatch "OPCODE-H1" )
|#



