(defvar *x-android-mode-line/string* "")

(defun x-android-mode-line/inject-into-global-mode-string ( &optional remove )
  (if remove
      (setq global-mode-string
            (remove '*x-android-mode-line/string* global-mode-string))
    (when (not (member '*x-android-mode-line/string* global-mode-string))
      (setq global-mode-string
            (append global-mode-string
                    '(*x-android-mode-line/string*)))))
  
  global-mode-string)

(defun x-android-mode-line/clear-mode-line ()
  (setq *x-android-mode-line/string* ""))

(defun x-android-mode-line/update-mode-line (text)
  (setq *x-android-mode-line/string* (format "x-android: %s" text)))

;;
;; Helpful functions
;; 

(defalias 'x-android-mode-line/inject
  'x-android-mode-line/inject-into-global-mode-string)

(defalias 'x-android-mode-line/update
  'x-android-mode-line/update-mode-line)

(defun x-android-mode-line/inject-and-update ( text )
  (x-android-mode-line/update-mode-line text)
  (x-android-mode-line/inject-into-global-mode-string))

(provide 'x-android-mode-line)

;; (x-android-mode-line/inject-into-global-mode-string)
;; (x-android-mode-line/clear-mode-line)
;; (x-android-mode-line/update-mode-line "2")

