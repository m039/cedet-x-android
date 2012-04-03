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

(defun x-android-import-find-files-in-databases (file-name)
  "Return list of the full file names finded in all databases in
the current buffer."
  (remove-duplicates (apply 'append (mapcar (lambda (db)
                                              (x-android-import-find-files-from-database db file-name))
                                            (x-android-import-get-java-jar-databases))) :test 'string=))

(defun x-android-import-find-files-in-directory (directory match &optional cut-directory-part)
  "Find all files matched MATCH in DIR "
  (let (ans)
    (flet ((dir-directories (dir)
                            (let (dirs)
                              (dolist (f (directory-files dir))
                                (let ((f-full-name (expand-file-name f dir)))
                                  (when (and (file-directory-p f-full-name)
                                             (not (member f '("." ".."))))
                                    (push (file-name-as-directory f-full-name) dirs))))
                              (nreverse dirs)))
           
           (dir-files (dir)
                      (dolist (f (directory-files dir t match))
                        (push f ans)))

           (dir-recursive (dir)
                          (dir-files dir)
                          (dolist (d (dir-directories dir))
                            (dir-recursive d))))
      
      (dir-recursive directory)

      (let ((res (nreverse ans)))
        (if cut-directory-part
            (let* ((full-name (expand-file-name directory))
                   (full-name-length (length full-name)))
              (mapcar (lambda (f) (substring f full-name-length)) res))
          res)))))

(defun x-android-import-find-files-in-bin/classes-directory (file-name)
  "Find all *.class files in DIR "
  (unless x-android-import-bin/classes-files
      (x-android-import-update-bin/classes-files))
  (let (ans)
    (dolist (f x-android-import-bin/classes-files)
      (when (string-match (concat "[/$]" (regexp-quote file-name) "\\.class$") f)
        (push f ans)))
    (nreverse ans)))

(defun x-android-import-one-class (class)
  "Insert an import into the buffer if not already there."
  (if (not (jde-import-already-imports-class class (jde-import-get-imports)))
      (jde-import-insert-imports-into-buffer (list class))))

(defvar x-android-import-bin/classes-files nil
  "This variable as a cache holds list of *.class file names in
  bin/classes.")

(defun x-android-import-update-bin/classes-files ()
  (let* ((root (ede-project-root-directory (ede-current-project)))
         (files (x-android-find-bin/classes-recursively root)))
    (when files
      (setq x-android-import-bin/classes-files files))))

;; todo add before compilation hook

(add-to-list 'compilation-finish-functions
             (lambda (buffer error) (x-android-import-update-bin/classes-files)))

(defun x-android-import-class (class-name)
  (interactive "sClass name: ")
  (let* ((imports (mapcar (lambda (f)
                            (let ((no-class (substring f 0 (string-match "\\.class" f))))
                              (replace-regexp-in-string "\\(\\$\\|/\\)" "." no-class)))
                          (append (x-android-import-find-files-in-databases class-name)
                                  (x-android-import-find-files-in-bin/classes-directory class-name))))
         
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
