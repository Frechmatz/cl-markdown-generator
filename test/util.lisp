
(in-package :cl-markdown-generator-test)

(defun string-to-string-list (str)
  (let ((l '()))
    (with-input-from-string (s str)
      (loop
	 (let ((line (read-line s nil t)))
	   (cond
	     ((stringp line)
	      (push line l))
	     (t (return))))))
    (reverse l)))

    
