(defpackage :cl-threadpool-doc
  (:use :cl))

(in-package :cl-threadpool-doc)


(asdf:load-system "docparser")
(asdf:load-system "cl-markdown-generator")

;;
;; Failed attempt to generate markdown documentation
;; using the markdown generator.
;; From my point of view it's easier to just write a markdown file
;; or to format documentation strings in markdown format and just
;; dump them into a file (using docparser)
;;
(defun generate ()
    (with-open-file (s "/Users/olli/src/lisp/cl-markdown-generator/doc.md" :direction :output :if-exists :supersede)
      (md:with-markdown-output (s)
	(md:markdown
	 (:h1 "cl-markdown-generator")
	 (:paragraph "A markdown generator written in Common Lisp")
	 (:paragraph "Status: Work in progress")
	 (:h2 "Examples")
	 (:paragraph "A couple of code snippets and the resulting markdown outputs.")
	 ;; we need a code formatter here
	 (:codeblock
	  "(cl-markdown-generator:with-markdown-output-to-string (s)
(:h1 \"Header\") 
(:list 
   (:list-item \"Item 1\") 
   (:list-item \"Item 2\")))")
	 ;; dummy paragraph to avoid that the two code blocks will be merged into one
	 (:paragraph "Generates")
	 (:codeblock
	  "# Header
           
* Item 1
* Item 2")
      ))))
      

(generate)


