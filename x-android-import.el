(require 'cedet-java)
(require 'cedet-android)


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

(provide 'x-android-import)