;;
;; Tests static markdown
;;

(in-package :cl-markdown-generator-test)


(define-test test-plain-string ()
	     ""
	     (assert-equal 
	      '("Plain text")
	      (string-to-string-list
	       (cl-markdown-generator:with-markdown-output-to-string
		   (s)
		   "Plain text"))))

(define-test test-plain-string-2 ()
	     ""
	     (assert-equal 
	      '("Plain text Plain text2")
	      (string-to-string-list
	       (cl-markdown-generator:with-markdown-output-to-string
		   (s)
		   "Plain text " "Plain text2"))))

(define-test test-header-one-header ()
	     ""
	     (assert-equal
	      '("# Header 1" "")
	      (string-to-string-list
	       (cl-markdown-generator:with-markdown-output-to-string (s)
		   (:h1 "Header 1")))))

(define-test test-header-two-headers ()
	     ""
	     (assert-equal
	      '("# Header 1" "" "# Header 2" "")
	      (string-to-string-list
	       (cl-markdown-generator:with-markdown-output-to-string (s)
		   (:h1 "Header 1") (:h1 "Header 2")))))

(define-test test-list ()
	     ""
	     (assert-equal
	      '("# Header 1" "" "* Item 1" "* Item 2")
		(string-to-string-list
		 (cl-markdown-generator:with-markdown-output-to-string (s)
		   (:h1 "Header 1") (:list (:list-item "Item 1") (:list-item "Item 2"))))))

(define-test test-nested-list ()
	     ""
	     (assert-equal
	      '("# Header 1"
		""
		"* Item 1"
		"    * Item 1.1"
		"    * Item 1.2"
		"* Item 2")
		(string-to-string-list
		 (cl-markdown-generator:with-markdown-output-to-string (s)
		   (:h1 "Header 1")
		   (:list (:list-item "Item 1"
				      (:list (:list-item "Item 1.1")
					     (:list-item "Item 1.2")))
			  (:list-item "Item 2"))))))


(define-test test-em ()
	     ""
	     (assert-equal 
	      '("Text *em text*")
	      (string-to-string-list
	       (cl-markdown-generator:with-markdown-output-to-string
		   (s)
		 "Text "
		 (:em "em text")))))


(define-test test-strong ()
	     ""
	     (assert-equal 
	      '("**Plain text**")
	      (string-to-string-list
	       (cl-markdown-generator:with-markdown-output-to-string
		   (s)
		   (:strong "Plain text")))))


(define-test test-codeblock ()
	     "Test codeblock"
	     (assert-equal 
	      '("# Header"
		""
		"    cd /"
		"")
	      (string-to-string-list
	       (cl-markdown-generator:with-markdown-output-to-string
		   (s)
		 (:h1 "Header") (:codeblock "cd /")))))

(define-test test-blockquote-simple ()
	     ""
	     (assert-equal 
	      '("# Header"
		""
		"> Block")
	      (string-to-string-list
	       (cl-markdown-generator:with-markdown-output-to-string
		   (s)
		 (:h1 "Header") (:blockquote "Block")))))

(define-test test-blockquote-nested ()
	     ""
	     (assert-equal 
	      '("# Header"
		""
		"> Block"
		""
		"> > Inner Block"
		)
	      (string-to-string-list
	       (cl-markdown-generator:with-markdown-output-to-string
		   (s)
		 (:h1 "Header") (:blockquote "Block" (:blockquote "Inner Block"))))))

(define-test test-linebreak-1 ()
	     ""
	     (assert-equal
	      '("# Header 1"
		"Header 1 continued"
		"")
	      (string-to-string-list
	       (cl-markdown-generator:with-markdown-output-to-string (s)
		   (:h1 "Header 1
Header 1 continued")))))

(define-test test-linebreak-2 ()
	     ""
	     (assert-equal
	      '("> Quote"
		"> Quote continued")
	      (string-to-string-list
	       (cl-markdown-generator:with-markdown-output-to-string (s)
		   (:blockquote "Quote
Quote continued")))))
