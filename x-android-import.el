(require 'cedet-java)
(require 'cedet-android)

(require 'jde-parse)

;;
;; This code is almost copy from jde and changing first part with
;; x-android.
;; 

(defun jde-import-get-imports ()
  "Returns a list containing all imported classes."
  (let* (imports
		 (tags  (semantic-fetch-tags))
		 (import-tags (semantic-brute-find-tag-by-class 'include tags)))
    (dolist (import-tag import-tags)
      (setq imports
			(cons
			 (semantic-tag-name import-tag)
			 imports)))
    (nreverse imports)))

(defun jde-import-already-imports-class (class-name existing-imports)
  "Determine if a class is already being imported."
  (find
   class-name
   existing-imports
   :test (lambda (new existing)
		   (let ((new-package (jde-parse-get-package-from-name new))
				 (new-class (jde-parse-get-unqualified-name new))
				 (existing-package (jde-parse-get-package-from-name existing))
				 (existing-class (jde-parse-get-unqualified-name existing)))
		     (and
		      (string= new-package existing-package)
		      (or
		       (string= new-class existing-class)
		       (string= existing-class "*")))))))


(defun jde-import-get-import-insertion-point ()
  "Determine where to insert an import statement.
If the buffer contains an import statement, return
the beginning of the next line; otherwise, if
the buffer contains a package statement, insert
three empty lines and return the beginning of
the second empty line; otherwise, if the buffer
contains a class definition, return the beginning
of the line before the class definition; otherwise,
return the beginning of the buffer."
  (let* ((tags (semantic-fetch-tags))
		 (import-tag
		  (car (last (semantic-brute-find-tag-by-class
					  'include tags))))
		 (package-tag (car (semantic-brute-find-tag-by-class
							'package tags)))
		 (class-tag (car (semantic-brute-find-tag-by-class
						  'type tags)))
		 insertion-point)
	(cond (import-tag
		   (setq insertion-point (+ (semantic-tag-end import-tag) 1)))
		  (package-tag
		   (save-excursion
			 (goto-char (semantic-tag-end package-tag))
			 (forward-line)
			 (insert "\n")
			 (setq insertion-point (point))))
		  (class-tag
		   (setq insertion-point
				 (let ((comment-token (semantic-documentation-for-tag
									   class-tag 'lex)))
				   (if comment-token
					   (semantic-lex-token-start comment-token)
					 (semantic-tag-start class-tag)))))
		  (t
		   (setq insertion-point 1)))
	(save-excursion
	  (goto-char insertion-point)
	  (unless (and (bolp) (eolp)) (insert "\n")))
	insertion-point))

(defun jde-import-insert-imports-into-buffer (new-imports &optional exclude)
  "Inserts imports into the correct place in the buffer."
  (save-excursion
    (goto-char (jde-import-get-import-insertion-point))
	(deactivate-mark)
    ;; (if exclude
	;; 	(setq new-imports (jde-import-exclude-imports new-imports)))
    (loop for new-import in new-imports do
		  (when (> (length new-import) 0) ;; added to avoid insert empty import statements.
			(insert
			 (concat "import " new-import ";\n"))
			(message "Imported %s" new-import)))
    ;; (if jde-import-auto-collapse-imports
	;; 	(let (jde-import-auto-collapse-imports) ;; setting this to avoid infinite recursion
	;; 	  (jde-import-collapse-imports)))
    ;; (if jde-import-auto-sort
	;; 	(funcall jde-import-auto-sort-function))
	))

(defun jde-import-find-and-import (class &optional no-errors no-exclude qualifiedp)
  "*Insert an import statement for a class in the current buffer.
CLASS is an unqualified class name. This function searches
the classpath for a class (or classes) that match CLASS. If it
finds only one, it inserts an import statements for the class at the
head of the current buffer. If it finds more than one class that matches
CLASS, it prompts you to select which class to import. You can customize
the variable `jde-import-excluded-classes' to prevent specified classes
from being imported or considered for import. If the prefix argument NO-EXCLUDE
is non-nil, jde-import-excluded-classes will be ignored.
This command uses the JDE's BeanShell interpreter. It starts the interpreter
if it is not already running so there may be a short delay generating the first
import statement in the session. Note that you must explicitly include
any directories or jars that you want the command to search in your
classpath, except jars implicitly included by the jvm, e.g.,
rt.jar. The NO-ERRORS is used to avoid showing erros to the user."
  (interactive
   (flet ((vfn
		   (class)
		   (let ((existing-import (jde-import-get-import (third class))))
			 (if (null existing-import)
				 class
			   (message "Skipping: already imported %s" existing-import)
			   'pass))))
     (list (jde-read-class nil nil nil nil nil 'vfn) nil current-prefix-arg t)))
  (if qualifiedp
      (unless (eq class 'pass)
		(jde-parse-class-exists "java.util.List")
		(jde-import-insert-import (list class) (not no-exclude)))
    (let (existing-import)
      (setq existing-import (jde-import-get-import class))
      (if (not (null existing-import))
		  (message "Skipping: already imported %s" existing-import)
		(let ((imports (jde-import-get-qualified-names class)))
		  (setq imports (remove-duplicates imports :test 'equal))
		  (if imports
			  (jde-import-insert-import imports (not no-exclude))
			(if (not no-errors)
				(message "Error: could not find %s." class))))))))

;; jde-import-one-class
(defun x-android-import-one-class (class)
  "Insert an import into the buffer if not already there."
  (interactive "s")
  (if (not (jde-import-already-imports-class class (jde-import-get-imports)))
      (jde-import-insert-imports-into-buffer (list class))))

(provide 'x-android-import)
