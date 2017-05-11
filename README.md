# cl-markdown-generator
A markdown generator written in Common Lisp

Status: Work in progress

## Examples

    (cl-markdown-generator:with-markdown-output-to-string (s)
        (:h1 "Header 1") 
        (:list 
            (:list-item "Item 1") 
            (:list-item "Item 2")))
       
    (cl-markdown-generator:with-markdown-output-to-string (s)
        (:h1 "Header 1")
        (:list 
            (:list-item "Item 1"
            (:list 
                (:list-item "Item 1.1")
                (:list-item "Item 1.2")))
            (:list-item "Item 2")))
       
    (cl-markdown-generator:with-markdown-output-to-string (s)
        (:h1 "Header 1")
        (dotimes (i 2)
            (cl-markdown-generator:markdown 
	        (:h1 "Header " (cl-markdown-generator:str (+ i 2))))))

    (cl-markdown-generator:with-markdown-output-to-string (s)
        (:h1 "Header 01")
        (dotimes (i 2)
            (cl-markdown-generator:markdown 
	        (:h1 "Header " (cl-markdown-generator:fmt "~2,'0d" (+ i 2))))))
