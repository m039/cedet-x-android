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
  (let* ((pd (ede-x-android-project-data dir)))
	(ede-x-android-project
	 (cdr (assoc 'name pd))
	 :name (cdr (assoc 'name pd))
	 :version (cdr (assoc 'version pd))
	 :directory (file-name-as-directory dir)
	 :file (expand-file-name "AndroidManifest.xml" dir)
	 :package (cdr (assoc 'package pd)))))

(defun ede-x-android-project-data (dir)
  (let ((name		(ede-x-android-project-data-name dir))
		(version	(ede-x-android-project-data-version dir))
		(package	(ede-x-android-project-data-package dir)))
	(list (cons 'name name)
		  (cons 'version version)
		  (cons 'package package))))

(defun ede-x-android-project-data-version (dir)
  (let* ((root (car (xml-parse-file (expand-file-name "AndroidManifest.xml" dir))))
		 (version-text (xml-get-attribute-or-nil root 'android:versionName)))
		 version-text))

(defun ede-x-android-project-data-package (dir)
  (let* ((root (car (xml-parse-file (expand-file-name "AndroidManifest.xml" dir))))
		 (version-text (xml-get-attribute-or-nil root 'package)))
		 version-text))

(defun ede-x-android-project-data-name (dir)
  (flet ((find-name-in-build-xml ()
								 (let (name)
								   (if (not (file-exists-p (expand-file-name "build.xml")))
									   (setq name nil)
									 (let* ((root (car (xml-parse-file (expand-file-name "build.xml"))))
											(name-text (xml-get-attribute-or-nil root 'name)))
									   (setq name name-text))
									 name)))
		 (find-name-in-pom-xml ()
							   (let (name)
								 (if (not (file-exists-p (expand-file-name "pom.xml")))
									 (setq name nil)
								   (let* ((root (car (xml-parse-file (expand-file-name "pom.xml"))))
										  (name-child (car (xml-get-children root 'name)))
										  (name-text (car (xml-node-children name-child))))
									 (setq name name-text)
									 name))))
		 (find-name-in-mainfest-xml ()
									;; TODO
									nil))
	(with-temp-buffer
	  (setq default-directory (file-name-as-directory dir))
	  (dolist (f
			   '(find-name-in-build-xml
				 find-name-in-pom-xml))
		(let ((name (funcall f)))
		  (unless (null name)
			(return name)))))))

(setq test-dir "/home/m039/Trash/Android-ViewPagerIndicator")
(setq test-dir "/home/m039/Trash/Android-Examples/WE1")

(ede-x-android-project-data test-dir)

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
   ;; 		(ede-buffer-belongs-to-project-p) ]
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
			   :proj-file "AndroidManifest.xml"
			   :load-type 'ede-x-android-load
			   :class-sym 'ede-x-android-project
			   :new-p t
			   :safe-p t))

(provide 'ede-x-android)

;; debugging

;; (setq ede-projects nil)
