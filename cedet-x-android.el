(require 'cedet-java)
(require 'cedet-android)

;;; Nine Patch
;;

(defvar cedet-x-android-sdk-draw9patch (expand-file-name "tools/draw9patch" cedet-android-sdk-root)
  "Location of the android draw9patch program.")

(defun cedet-x-android-start-nine-patch-editor ()
  "Start Android's (non official) nine-patch editor"
  (interactive)
  (let ((editor-file (expand-file-name
                      "data/NinePatchEditor.jar"
                      (file-name-directory (locate-library "cedet-x-android")))))
    (if (file-exists-p editor-file)
        (apply 'start-process
               "nine-patch-editor"
               nil                      ; buffer
               cedet-java-command
               (list "-jar"
                     editor-file))
      (apply 'start-process
               "nine-patch-editor"
               nil                      ; buffer
               cedet-x-android-sdk-draw9patch
               nil))))

(defun cedet-x-android-start-gui-builder ()
  "Start Android's (non official) gui builder"
  (interactive)
  (let ((builder-file (expand-file-name
                       "data/droiddraw.jar"
                       (file-name-directory (locate-library "cedet-x-android")))))
    (when (file-exists-p builder-file)
      (apply 'start-process
             "gui-builder"
             nil                        ; buffer
             cedet-java-command
             (list "-jar"
                   builder-file)))))

(defun cedet-x-android-support-jars ()
  "Return a list of android-support-*.jar in the
  <sdk-root>/extras/android/support."
  (flet ((dirs-in-support ()
                          (let (ans)
                            (dolist (f (directory-files
                                        (expand-file-name "extras/android/support" cedet-android-sdk-root) t))
                              (when (file-directory-p f)
                                  (push f ans)))
                            (nreverse ans)))
         (support-jars-in-dir (dir)
                          (directory-files dir t ".+support.+\\.jar$")))
    (let (ans)
      (dolist (dir (dirs-in-support))
        (dolist (jar (support-jars-in-dir dir))
          (push jar ans)))
      (nreverse ans))))

(provide 'cedet-x-android)