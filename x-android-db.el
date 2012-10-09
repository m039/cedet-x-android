(defvar x-android-db/sqlite-program "sqlite"
  "Full path name of the SQLite executable.")

(require 'comint)

;;; Commentary:

;; All you need from this file is this functions:
;; - x-android-db/start-sqlite-process
;; - x-android-db/send-command

(defconst x-android-db/sqlite-program "/usr/bin/sqlite3")

(defconst x-android-db/sqlite-process-name "x-android/sqlite-process")
(defconst x-android-db/sqlite-process-buffer (format "*%s*" x-android-db/sqlite-process-name))
(defconst x-android-db/sqlite-output-buffer "*x-android/sqlite-output-buffer*")

(defconst x-android-db/result-type "html")

(defun x-android-db/start-sqlite-process ()
  "Spawn the sqlite process in the buffer if there is no such
process."
  (let ((comint-prompt-regexp "^sqlite>+ *")
        )

    (make-comint x-android-db/sqlite-process-name
                 x-android-db/sqlite-program nil (format "-%s" x-android-db/result-type))
    
    ))

(defun x-android-db/send-command ( command )
  "Sends the command to the SQLITE process. Returns nil if error
occurs otherwise list of the results."
  (with-current-buffer (get-buffer-create x-android-db/sqlite-output-buffer)
    (erase-buffer)
    
    (comint-redirect-send-command-to-process
     command
     x-android-db/sqlite-output-buffer
     (get-buffer-process x-android-db/sqlite-process-buffer)
     nil
     t)

    ;; Wait for the process to complete
    (with-current-buffer x-android-db/sqlite-process-buffer
      (while (null comint-redirect-completed)
        (accept-process-output nil)))
    
    (let ((error-string "Error")
          (result nil))
      (if (and (not (= (point-min) (point-max)))
               (equal error-string
                      (buffer-substring (point-min) (+ (point-min) (length error-string)))))
          nil

        (x-android-db/read-result)
        ))))

(defun x-android-db/read-result ()
  (goto-char (point-min))
  (let (result
        entity)
    
    (when (string= x-android-db/result-type "html")
      (while (setq entity (x-android-db/read-html-entity))
        (push entity result))
      (nreverse result))))

(defun x-android-db/read-html-entity ()
  (let ((case-fold-search t)
        result)
    (let* ((openning-tr "<tr>")
           (closing-tr "</tr>")
           (openning-td "<td>")
           (closing-td "</td>")

         (left (search-forward openning-tr nil t))
         (right (search-forward closing-tr nil t)))

      (when (and left
                 right)
        (let ((tr (buffer-substring left (search-backward closing-tr))))

          ;; `TODO' optimization: rewrite this to walk from top to
          ;; bottom (that is don't need the nreverse)
          
          (with-temp-buffer
            (insert tr)
            (goto-char (point-min))

            (while (search-forward openning-td nil t)
              (setq left (point))
              (setq right (search-forward closing-td nil t))
              
              (when (and left right)
                (push (buffer-substring left (search-backward closing-td)) result)))

            (nreverse result)))))))

(defun x-android-db/command/mode-html ()
  (setq x-android-db/result-type "html")
  (format ".mode %s" x-android-db/result-type))

(defun x-android-db/command/select-database ( path-to-sqlite-database )
  (format ".restore \"%s\""  (expand-file-name path-to-sqlite-database)))

(defun x-android-db/command/query ( query )
  (format "%s;" query))

(provide 'x-android-db)

;;
;; Example of usage
;; 

;; (x-android-db/start-sqlite-process)

;; (x-android-db/send-command (x-android-db/command/select-database "~/.x-android/android-15.db"))
;; (x-android-db/send-command (x-android-db/command/select-database "~/.x-android/test.db"))

;; (length (x-android-db/send-command (x-android-db/command/query "select name,simple_name,methods from class")))
;; (length (x-android-db/send-command (x-android-db/command/query "select * from t1")))
