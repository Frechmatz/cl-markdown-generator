(in-package :cl-markdown-generator-test)


(define-test test-plain-string ()
	     ""
	     (assert-equal 
	      '("Plain text")
	      (string-to-string-list
	       (cl-markdown-generator:with-markdown-output-to-string
		   (s)
		   "Plain text"))))

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


