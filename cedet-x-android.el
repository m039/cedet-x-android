(require 'cedet-java)

;;; Nine Patch
;;

(defun cedet-x-android-start-nine-patch-editor ()
  "Start Android's (non official) nine-patch editor"
  (interactive)
  (apply 'start-process
		  "nine-patch-editor"
		  nil							; buffer
		  cedet-java-command
		  (list "-jar"
				(expand-file-name
				 "NinePatchEditor.jar"
				 (file-name-directory (locate-library "cedet-x-android"))))))

(provide 'cedet-x-android)