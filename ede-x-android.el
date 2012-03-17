(require 'cedet-android)

(setq old-ede-files ede-project-class-files)
(setq ede-project-class-files (list
							   (ede-project-autoload "edeproject-makefile"
													 :name "Make" :file 'ede-proj
													 :proj-file "Project.ede"
													 :load-type 'ede-proj-load
													 :class-sym 'ede-proj-project
													 :safe-p nil)
							   (ede-project-autoload "edeproject-automake"
													 :name "Automake" :file 'ede-proj
													 :proj-file "Project.ede"
													 :initializers '(:makefile-type Makefile.am)
													 :load-type 'ede-proj-load
													 :class-sym 'ede-proj-project
													 :safe-p nil)
							   (ede-project-autoload "automake"
													 :name "automake" :file 'project-am
													 :proj-file "Makefile.am"
													 :load-type 'project-am-load
													 :class-sym 'project-am-makefile
													 :new-p nil
													 :safe-p t)
							   ))

(.add-to-load-path "~/Dropbox/Sync/android/")

(defvar ede-x-android-project-list nil
  "List of projects created by option `ede-x-android-project'.")

;; (NAME VERSION PACKAGE)

(defun ede-x-android-load (dir &optional rootproj)
  (message "dir %s, rootproj %s" dir rootproj)
  (ede-x-android-project
   "m039"
   :name "m039"
   :version "0.1"
   :directory (file-name-as-directory dir)
   :file (expand-file-name "AM.xml"
						   dir)))

(defmethod initialize-instance ((this ede-x-android-project)
				&rest fields)
  "Make sure the targets slot is bound."
  (call-next-method)
  
  (unless (slot-boundp this 'targets)
    ;; @TODO - All android projects are the same, so we can probably
    ;; prepopulate this whole thing right off.
    (oset this :targets nil))
  ;; In case the defaults change, force the known configurations
  ;; of android to be setup here.
  (oset this configurations '("debug" "install" "release"))
  (oset this configuration-default "debug"))

(defclass ede-x-android-project (ede-project eieio-instance-tracker)
  ((tracking-symbol :initform 'ede-x-android-project-list)
   ;; (keybindings :initform (("S" . ede-android-visit-strings)))
   ;; (menu :initform
   ;; 	 (
   ;; 	  [ "Visit strings.xml" ede-android-visit-strings ]
   ;; 	  [ "Edit Projectfile" ede-edit-file-target
   ;; 	    (ede-buffer-belongs-to-project-p) ]
   ;; 	  [ "Start Debug Proxy (DDMS)" cedet-android-start-ddms ]
   ;; 	  "--"
   ;; 	  [ "Update Version" ede-update-version ede-object ]
   ;; 	  [ "Version Control Status" ede-vc-project-directory ede-object ]
   ;; 	  [ "Android Shell" cedet-android-adb-shell ede-object ]
   ;; 	  [ "Layout Optimizer" ede-android-layoutopt ede-object ]
   ;; 	  "--"
   ;; 	  [ "Rescan Project Files" ede-rescan-toplevel t ]
   ;; 	  ))
   (package :initarg :package
	    :initform "com"
	    :type string
	    :documentation "The package extracted from the Manifest.")
   )
  "Project for Android applications.")

(ede-add-project-autoload
 (ede-project-autoload "x-android"
		       :name "X ANDROID ROOT"
			   :file 'ede-x-android			   
		       :proj-file "AM.xml"
		       :load-type 'ede-x-android-load
		       :class-sym 'ede-x-android-project
		       :new-p t
		       :safe-p t))

(provide 'ede-x-android)

;; debugging

;; (setq ede-projects nil)
