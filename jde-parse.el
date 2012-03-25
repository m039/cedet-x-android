
(defun jde-parse-get-package-from-name (class-name)
  "Gets the package portion of a qualified class name."
  (substring
   class-name 0
   (let ((pos  (position ?. class-name :from-end t)))
     (if pos
	 pos
       0))))

(defun jde-parse-get-unqualified-name (name)
  "Gets the last name in a qualified name."
  (let ((unqualified-name (substring name (string-match "[^.]+$" name))))
    (if unqualified-name unqualified-name name)))

(provide 'jde-parse)
