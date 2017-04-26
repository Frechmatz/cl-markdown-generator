(defsystem :cl-markdown-generator
  :serial t
  :version "0.0.1"
  :description "Implementation of a markdown generator"
  :long-description "Implementation of a markdown generator"
  :depends-on ()
  :components (
	       (:module "src"
			:serial t
			:components ((:file "packages")
				     (:file "markdown")))
	       ))

    

