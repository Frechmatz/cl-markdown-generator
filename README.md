# cl-markdown-generator

A markdown generator written in Common Lisp

Status: **Cancelled**

The examples are working and all tests pass but there will be no further
development of this project.

## Examples

### Headers and Paragraphs

    (cl-markdown-generator:with-markdown-output (*STANDARD-OUTPUT*)
        (:h1 "Chapter 1") 
        (:paragraph "Lorem ipsum dolor sit amet, consectetuer adipiscing elit.")
        (:h1 "Chapter 2")
        (:paragraph "Lorem ipsum dolor sit amet, consectetuer adipiscing elit."))
	
Generates

    # Chapter 1

    Lorem ipsum dolor sit amet, consectetuer adipiscing elit.

    # Chapter 2

    Lorem ipsum dolor sit amet, consectetuer adipiscing elit.

### Lists

    (cl-markdown-generator:with-markdown-output (*STANDARD-OUTPUT*)
        (:h1 "Header 1") 
        (:list 
            (:list-item "Item 1") 
            (:list-item "Item 2")))

Generates

    # Header 1

    * Item 1
    * Item 2

### Nested Lists

    (cl-markdown-generator:with-markdown-output (*STANDARD-OUTPUT*)
        (:h1 "Header 1")
        (:list 
            (:list-item "Item 1"
            (:list 
                (:list-item "Item 1.1")
                (:list-item "Item 1.2")))
            (:list-item "Item 2")))

Generates

    # Header 1

    * Item 1
        * Item 1.1
        * Item 1.2
    * Item 2


### Blockquotes

    (cl-markdown-generator:with-markdown-output (*STANDARD-OUTPUT*)
        (:blockquote
            (:h1 "Header 1")
            (:list 
                (:list-item "Item 1"
                (:list 
                    (:list-item "Item 1.1")
                    (:list-item "Item 1.2")))
                (:list-item "Item 2"))))

Generates

    > # Header 1
    > 
    > * Item 1
    >     * Item 1.1
    >     * Item 1.2
    > * Item 2


### Nested Blockquotes

    (cl-markdown-generator:with-markdown-output (*STANDARD-OUTPUT*)
        (:blockquote
            (:blockquote
                (:h1 "Header 1")
                (:list 
                    (:list-item "Item 1"
                    (:list 
                        (:list-item "Item 1.1")
                        (:list-item "Item 1.2")))
                    (:list-item "Item 2")))))

Generates

    > > # Header 1
    > > 
    > > * Item 1
    > >     * Item 1.1
    > >     * Item 1.2
    > > * Item 2

### Embedded Lisp 1

    (cl-markdown-generator:with-markdown-output (*STANDARD-OUTPUT*)
        (:h1 "Header 1")
        (dotimes (i 2)
            (cl-markdown-generator:markdown 
	        (:h1 "Header " (cl-markdown-generator:str (+ i 2))))))

Generates

    # Header 1

    # Header 2

    # Header 3


### Embedded Lisp 2

    (cl-markdown-generator:with-markdown-output (*STANDARD-OUTPUT*)
        (:h1 "Header 01")
        (dotimes (i 2)
            (cl-markdown-generator:markdown 
	        (:h1 "Header " (cl-markdown-generator:fmt "~2,'0d" (+ i 2))))))

Generates

    # Header 01

    # Header 02

    # Header 03
