(require 'jde-parse)

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
    ;;  (setq new-imports (jde-import-exclude-imports new-imports)))
    (loop for new-import in new-imports do
          (when (> (length new-import) 0) ;; added to avoid insert empty import statements.
            (insert
             (concat "import " new-import ";\n"))
            (message "Imported %s" new-import)))
    ;; (if jde-import-auto-collapse-imports
    ;;  (let (jde-import-auto-collapse-imports) ;; setting this to avoid infinite recursion
    ;;    (jde-import-collapse-imports)))
    ;; (if jde-import-auto-sort
    ;;  (funcall jde-import-auto-sort-function))
    ))

(defun jde-import-get-import (unqualified-class)
  "Get imported name for unqualified name UNQUALIFIED-CLASS.
This name may have the form \"package.*\". Returns nil,
if there is no import statement for UNQUALIFIED-CLASS."
  (let (import
    (imports (jde-import-get-imports))
    (qualified-names (jde-import-get-qualified-names unqualified-class)))
    (catch 'found
      (dolist (class qualified-names)
    (if (setq import (jde-import-already-imports-class class imports))
        (throw 'found import))))))

(provide 'jde-import)