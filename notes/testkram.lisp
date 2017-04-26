(in-package :cl-markdown-generator)


#|
(defun test-88 ()
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (let ((result (with-markdown-output-to-string (s) (:h1 "Chapter 0") (:h1 "Chapter 1"))))
    (format t "~%DONE: Is-String: ~a Value: ~a~%" (stringp result) result)))

(test-88)
|#

  
;;
;; Status: Wir sind jetzt soweit, das (:h1 "Chapter 0") ausgeführt wird
;; TODO: bei Aufruf mit ("string1" "string2") steigt der Compiler aus,
;; da eine Liste die als erstes Element einen String enthält von
;; process-form als Code interpretiert wird.
;; die Frage ist: Ist das gültige Syntax oder nicht?
;; -> über constantp nachdenken
;; 
;; weitere Todos: Basierend auf diesem Stand
;; - with-markdown-output-to-string
;; - Tests schreiben
;; - Prüfen, ob Document-Contexts separiert sind
;;
#|
(defun test-77 ()
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (let ((result
	 (with-output-to-string (s)
	   (with-markdown-output (s nil) (:h1 "Chapter 0") (:h1 "Chapter 1")))))
    ;;(break)
    (format t "~%DONE: Is-String: ~a Value: ~a~%" (stringp result) result)))

(test-77)
|#


;;
;;
;;

#|

;;(with-markdown-output *STANDARD-OUTPUT* "Plain text")
(defun to-string-0-1 ()
  (let ((str
	 (with-output-to-string (str)
	   (with-markdown-output str (:h1 "Dicker Bauch")))))
    (format t "~%The returned value is ~a~%" str)))
(to-string-0-1)


;;(with-markdown-output *STANDARD-OUTPUT* "Plain text")
(defun to-string ()
  (let ((str
	 (with-output-to-string (str)
	   (with-markdown-output str "Dicker Bauch"))))
    (format t "~%The returned value is ~a~%" str)))
(to-string)


(defun to-string-2 ()
  (let ((str
	 (with-markdown-output-to-string "Dicker Bauch")))
    (format t "~%The returned value is ~a~%" str)))
(to-string-2)

(defun to-string-3 ()
  (let ((str
	 (with-markdown-output-to-string (:h1 "Chapter 1"))))
    (format t "~%The returned value is ~a~%" str)))
(to-string-3)

     
|#
  

#|
(defmacro markdown-2 (&rest args)
  "Sets up a new compiler context and processes the argument forms"
  (let ((*compiler-context* (make-instance 'compiler-context)))
    (dolist (arg args)
      (process-form arg))
    (generate-code *compiler-context*)))


(defmacro with-markdown-output-2 (stream &rest body)
  (let ((*writer* (make-instance 'markdown-writer)))
    `(let ((*MARKDOWN-OUTPUT* ,stream))
	  (markdown ,@body))))
|#

#|
(defun with-markdown-output-1 ()
  (with-markdown-output *STANDARD-OUTPUT* (:h1 "Chapter 1")))

|#

;; (with-markdown-output-1)


;;
;; Debug and test stuff
;;




#|

;; Status: OK
(defun test-plain-string ()
  (markdown "Olli"))

|#


#|

;; Status: OK
(defun test-multiple-plain-strings ()
  (markdown "Olli" " hat" " einen dicken Bauch"))

;; Status: OK
(defun test-header ()
  (markdown (:h1 "Chapter 1")))
(test-header)


;; Status: OK
(defun test-header-with-multiple-content ()
  (markdown (:h1 "Chapter " "1")))
;;(test-header-with-multiple-content)

;; Status: OK
(defun test-3-headers ()
  (markdown (:h1 "Chapter 1") (:h1 "Chapter 2") (:h1 "Chapter 3")))
(test-3-headers)

;; Status: OK
(defun test-markdown-list ()
  (markdown (:list (:list-item "Item 1") (:list-item "Item 2"))))
;;(test-markdown-list)

;; Status: OK
(defun test-markdown-nested-list ()
  (markdown
   (:list
    (:list-item "Item 1")
    (:list-item "Item 2" 
		(:list
		 (:list-item "Item 2.1")
		 (:list-item "Item 2.2"
			     (:list
			      (:list-item "Item 2.2.1")
			      (:list-item "Item 2.2.2")))))
    (:list-item "Item 3"))))
;;(test-markdown-nested-list)

;; Status: OK
(defun test-simple-lisp ()
  (markdown (:h1 "Chapter 1") (let ((a 1)) (markdown (:h1 "Chapter 2")))))
;;(test-simple-lisp)

;; Status: OK
(defun test-loop-lisp ()
  (markdown (:h1 "Chapter 1") (dotimes (a 3) (markdown (:h1 "Chapter n")))))
;;(test-loop-lisp)

;; Status: OK
(defun test-lisp-list-with-3-items ()
  (markdown (:list (dotimes (b 3) (markdown (:list-item "Item"))))))
(test-lisp-list-with-3-items)

;; Status: OK
(defun test-lisp-3-lists-with-4-items ()
  (markdown (dotimes (a 3) (markdown (:h1 "List") (:list (dotimes (b 4) (markdown (:list-item "Item"))))))))
(test-lisp-3-lists-with-4-items)


|#










