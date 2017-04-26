(in-package :cl-markdown-generator-test)


(define-test test-static ()
	     ""
	     (assert-equal
	      '("# Header 1"
		""
		"# Header X"
		""
		"# Header X"
		"")
		(string-to-string-list
		 (cl-markdown-generator:with-markdown-output-to-string (s)
		   (:h1 "Header 1")
		   (dotimes (i 2)
		     (cl-markdown-generator:markdown (:h1 "Header X")))))))

(define-test test-str-1 ()
	     ""
	     (assert-equal
	      '("# Header 1"
		""
		"# Header 2"
		""
		"# Header 3"
		"")
		(string-to-string-list
		 (cl-markdown-generator:with-markdown-output-to-string (s)
		   (:h1 "Header 1")
		   (dotimes (i 2)
		     (cl-markdown-generator:markdown (:h1 "Header " (cl-markdown-generator:str (+ i 2)))))))))

(define-test test-fmt-1 ()
	     ""
	     (assert-equal
	      '("# Header 01"
		""
		"# Header 02"
		""
		"# Header 03"
		"")
		(string-to-string-list
		 (cl-markdown-generator:with-markdown-output-to-string (s)
		   (:h1 "Header 01")
		   (dotimes (i 2)
		     (cl-markdown-generator:markdown (:h1 "Header " (cl-markdown-generator:fmt "~2,'0d" (+ i 2)))))))))
