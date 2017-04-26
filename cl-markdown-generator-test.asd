(defsystem :cl-markdown-generator-test
  :serial t
  :version "0.0.1"
  :description "Tests of the markdown generator"
  :depends-on (
	       :lisp-unit)
  :components (
	       (:module "src"
			:serial t
			:components ((:file "packages")
				     (:file "markdown")))
	       (:module "test"
			:serial t
			:components ((:file "packages")
				     (:file "util")
				     (:file "static-markdown")
				     (:file "embedded-markdown"))
			)
					    
	       ))

    

