(require 'cedet-java)
(require 'cedet-android)

(require 'cl)

(require 'jde-import)
(require 'efc)

(defun x-android-import-get-java-jar-databases ()
  "Get all java-jar-databases in the current buffer. Works only
in java-mode."
  (let (ans)
    (dolist (o (semanticdb-javap-classpath-objects (current-buffer)))
      (when (semanticdb-java-jar-database-p o)
        (push o ans)))
    (nreverse ans)))

(defun x-android-import-find-files-from-database (dbc file-name)
  "Return list of the full file names from DBC that match FILE-NAME.
 For example '(android/preference/PreferenceActivity$Header.class)"
  (let (ans)
    (dolist (F (oref dbc jarfilecache))
      (when (string-match (concat "[/$]" (regexp-quote file-name) "\\.class$") F)
        (push F ans)))
    (nreverse ans)))

(defun x-android-import-find-files (file-name)
  "Return list of the full file names finded in all databases in
the current buffer."
  (apply 'append (mapcar (lambda (db)
                           (x-android-import-find-files-from-database db file-name))
                         (x-android-import-get-java-jar-databases))))

(defun x-android-import-one-class (class)
  "Insert an import into the buffer if not already there."
  (if (not (jde-import-already-imports-class class (jde-import-get-imports)))
      (jde-import-insert-imports-into-buffer (list class))))

(defun x-android-import-class (class-name)
  (interactive "sClass name: ")
  (let* ((imports (mapcar (lambda (f)
                            (let ((no-class (substring f 0 (string-match "\\.class" f))))
                              (replace-regexp-in-string "\\(\\$\\|/\\)" "." no-class)))
                          (x-android-import-find-files class-name)))
         
         (imports (remove-duplicates imports :test 'string=))
         
         (selected-class (if (<= (length imports) 1)
                             (car imports)
                           (efc-query-options imports "Select class")))
         )
    (if (null selected-class)
        (message "Nothing is imported.")
      (x-android-import-one-class selected-class))))

(defun x-android-import-class-under-point ()
  (interactive)
  (let ((cur-word (current-word)))
    (when (and cur-word  (> (length cur-word) 0))
      (when (string-match "[^a-zA-Z0-9_]\\([a-zA-Z0-9_]+\\)$" cur-word)
        (setq cur-word (match-string-no-properties 1  cur-word)))
      (when (string-match "^\\([a-zA-Z0-9_]+\\)[^a-zA-Z0-9_]" cur-word)
        (setq cur-word (match-string-no-properties 1  cur-word)))
      (x-android-import-class cur-word))))

(defun x-android-import-all-classes ()
  (interactive)
  )

(provide 'x-android-import)
