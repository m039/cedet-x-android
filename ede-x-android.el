(require 'cedet-android)
(require 'cedet-x-android)

(require 'x-android)
(require 'x-android-import)
(require 'x-android-cache)
(require 'x-android-db-parser)

(defun ede-x-android-clean-all ()
  "Only for debugging purpose! Sets all project-lists to nil"
  (interactive)
  (setq ede-x-android-project-list nil
        ede-projects nil))

(defvar ede-x-android-project-list nil
  "List of projects created by option `ede-x-android-project'.")

(defun ede-x-android-project-existing (dir)
  "Find an Android project in the list of Android projects.
DIR is the directory to search from."
  (let ((projs ede-x-android-project-list)
        (ans nil))
    (while (and projs
                (not ans))
      (let ((root (ede-project-root-directory (car projs))))
        (when (string-match (concat "^" (regexp-quote root)) dir)
          (setq ans (car projs))))
      (setq projs (cdr projs)))
    ;; (message "find project in %s dir" dir)
    ans))

(defun ede-x-android-proj-root (&optional file)
  (when (not file) (setq file default-directory))
  (let (ans)
    (while (and file
                (not ans))
      (when (file-exists-p (expand-file-name "AndroidManifest.xml" file))
        (setq ans file))
      (setq file (ede-up-directory file)))
    ans))

(defun ede-x-android-load (dir &optional rootproj)
  (or (ede-x-android-project-existing dir)
      (let* ((pd (ede-x-android-project-data dir))
             (proj (ede-x-android-project
                    (cdr (assoc 'name pd))
                    :name (cdr (assoc 'name pd))
                    :version (cdr (assoc 'version pd))
                    :directory (file-name-as-directory dir)
                    :file (expand-file-name "AndroidManifest.xml" dir)
                    :package (cdr (assoc 'package pd)))))
        (ede-add-project-to-global-list proj))))

;;;###autoload
(ede-add-project-autoload
 (ede-project-autoload "x-android"
               :name "X ANDROID ROOT"
               :file 'ede-x-android
               :proj-file "AndroidManifest.xml"
               :proj-root 'ede-x-android-proj-root
               :load-type 'ede-x-android-load
               :class-sym 'ede-x-android-project
               :new-p t
               :safe-p t)
 'unique)

(defun ede-x-android-project-data (dir)
  (let ((name       (ede-x-android-project-data-name dir))
        (version    (ede-x-android-project-data-version dir))
        (package    (ede-x-android-project-data-package dir)))
    (list (cons 'name name)
          (cons 'version version)
          (cons 'package package))))

;;
;; WARN: in api-demos didn't find the version
;;
(defun ede-x-android-project-data-version (dir)
  (let* ((root (car (xml-parse-file (expand-file-name "AndroidManifest.xml" dir))))
         (version-text (xml-get-attribute-or-nil root 'android:versionName)))
         (if version-text
             version-text
           "0.00")))

(defun ede-x-android-project-data-package (dir)
  (let* ((root (car (xml-parse-file (expand-file-name "AndroidManifest.xml" dir))))
         (version-text (xml-get-attribute-or-nil root 'package)))
         version-text))

(defun ede-x-android-project-data-name (dir)
  (flet ((find-name-in-build-xml ()
                                 (if (not (file-exists-p (expand-file-name "build.xml")))
                                     nil
                                   (let* ((root (car (xml-parse-file (expand-file-name "build.xml"))))
                                          (name-text (xml-get-attribute-or-nil root 'name)))
                                     name-text)))
         (find-name-in-pom-xml ()
                               (if (not (file-exists-p (expand-file-name "pom.xml")))
                                   nil
                                 (let* ((root (car (xml-parse-file (expand-file-name "pom.xml"))))
                                          (name-child (car (xml-get-children root 'name)))
                                          (name-text (car (xml-node-children name-child))))
                                     name-text)))
         (find-name-in-eclipse-xml ()
                                 (if (not (file-exists-p (expand-file-name ".project")))
                                     nil
                                   (let* ((root (car (xml-parse-file (expand-file-name ".project"))))
                                          (name-child (car (xml-get-children root 'name)))
                                          (name-text (car (xml-node-children name-child))))
                                     name-text)))

         (find-application-name-in-mainfest-xml ()
                                    (if (not (file-exists-p (expand-file-name "AndroidManifest.xml")))
                                        nil
                                      (let* ((root (car (xml-parse-file (expand-file-name "AndroidManifest.xml"))))
                                             (name-child (car (xml-get-children root 'application)))
                                             (name-text (xml-get-attribute-or-nil name-child 'android:name)))
                                        name-text)))

         (find-name-default ()
           "Can't find the project name"))
    (with-temp-buffer
      (setq default-directory (file-name-as-directory dir))
      (dolist (f
               '(find-name-in-build-xml
                 find-name-in-pom-xml
                 find-name-in-eclipse-xml
                 find-application-name-in-mainfest-xml
                 find-name-default))
        (let ((name (funcall f)))
          (unless (null name)
            (return name)))))))

;; CLASSES
;; -------

;;;###autoload
(defclass ede-x-android-project (ede-project eieio-instance-tracker)
  ((tracking-symbol :initform 'ede-x-android-project-list)
   (menu :initform
     (
      [ "Start nine-patch editor"   cedet-x-android-start-nine-patch-editor ]
      [ "Start gui builder"         cedet-x-android-start-gui-builder ]
      ))
   (targets :initform nil)
   (configurations :initform ("debug install"
                              "clean debug"
                              "clean debug install"
                              "release install"
                              "clean release"
                              "clean release install") :type list)

   (configuration-default :initform "clean debug install")
   (package :initarg :package
            :initform "com"
            :type string
            :documentation "The package extracted from the Manifest."))
  "Project for Android applications.")

(defclass ede-x-android-target-misc (ede-target)
  ()
  "EDE X-Android Project target for Misc files.
All directories with files should have at least one target.")

(defclass ede-x-android-target-java (ede-target)
  ((keybindings :initform (("i" . x-android-import-class-under-point)
                           ("I" . x-android-import-class)
                           )))
  "EDE X-Android Project target for .java files.")

(defclass ede-x-android-target-xml (ede-target)
  ()
  "EDE X-Android Project target for .xml files.")

(defmethod project-rescan ((this ede-x-android-project))
  (let ((pd (ede-x-android-project-data (file-name-directory (oref this file)))))
    (oset this name (cdr (assoc 'name pd)))
    (oset this version (cdr (assoc 'version pd)))
    (oset this package (cdr (assoc 'package pd)))))

;;; TARGET
;;  ------

(defun ede-x-android-find-matching-target (class dir targets)
  "Find a target that is a CLASS and is in DIR in the list of TARGETS."
  (let ((match nil))
    (dolist (T targets)
      (when (and (object-of-class-p T class)
                 (string= (oref T :path) dir))
        (setq match T)
        ))
    match))

(defmethod ede-find-target ((proj ede-x-android-project) buffer)
  "Find an EDE target in PROJ for BUFFER.
If one doesn't exist, create a new one for this directory."
  (let* ((ext (file-name-extension (buffer-file-name buffer)))
         (cls (cond ((string-match "java" ext)
                     'ede-x-android-target-java)
                    ((string-match "xml" ext)
                     'ede-x-android-target-xml)
                    (t 'ede-x-android-target-misc)))
         (targets (oref proj targets))
         (dir default-directory)
         (ans (ede-x-android-find-matching-target cls dir targets))

         ;; adding ext to name only for debugging purpose
         (name (concat (file-name-nondirectory
                        (directory-file-name dir))
                       (unless (null ext)
                         (format "-%s" ext))))
         )
    (when (not ans)
      (setq ans (make-instance
                 cls
                 :name name
                 :path dir
                 :source nil))
      (object-add-to-list proj :targets ans))
    ans))

;;; File Stuff
;;  ---- -----

(defmethod ede-project-root-directory ((this ede-x-android-project)
                                       &optional file)
  (file-name-directory (oref this file)))

(defmethod ede-project-root ((this ede-x-android-project))
  this)

(defmethod ede-find-subproject-for-directory ((this ede-x-android-project)
                                              dir)
  this)

;;; Include paths
;;  ------- -----

(defmethod ede-system-include-path ((this ede-x-android-target-java))
  "Get the system include path used by target THIS."
  (let ((root-dir (ede-project-root-directory (ede-current-project))))
    (append (x-android-find-source-path-recursively root-dir  'java-mode)
            (list (x-android-find-android-sources root-dir)))))

(defmethod ede-java-classpath ((this ede-x-android-project))
  (x-android-find-classpath-recursively (ede-project-root-directory this)))

(defmethod ede-source-paths ((this ede-x-android-project) mode)
  (x-android-find-source-path-recursively (ede-project-root-directory this) mode))

(defun ede-x-android-fname-if-exists (name)
  "Return the file NAME if it exists as a file."
  (if (file-exists-p name) name))

(defmethod ede-expand-filename-impl ((proj ede-x-android-project) name)
  "Within this android project, expand filename NAME."
  (let ((ans (call-next-method))        ; locate feature
        )
    (unless ans
      (let ((pr (ede-project-root-directory proj))
            (ext (file-name-extension name)))
        (setq ans
              (or
               (ede-x-android-fname-if-exists (expand-file-name name))
               (when (string= ext "java")
                 (or
                  (ede-x-android-fname-if-exists (expand-file-name name (expand-file-name "src" pr)))
                  (ede-x-android-fname-if-exists (expand-file-name name (expand-file-name "gen" pr)))
                  ;; @TODO Look in all subdirs of src and gen if not fully qualified.
                  ))
               (when (string= ext "xml")
                 (or
                  (ede-android-fname-if-exists (expand-file-name name (expand-file-name "res" pr)))
                  nil
                  ;; @TODO Look in all subdirs of res if not fully qualified.
                  ))
               (when (not ext)
                 ;; No extension, perhaps a directory substruction??
                 ;; Lets expand it as if a java package name.
                 (or
                  (ede-x-android-fname-if-exists (expand-file-name name (expand-file-name "src" pr)))
                  (ede-x-android-fname-if-exists (expand-file-name name (expand-file-name "gen" pr))))
                 )
               ))
        ))
    ans))

;;; Compile/Debug commands
;; -------------- --------

(defun ede-x-android-compile-ant (target &optional dir)
  "Function to compile the Android project using ant.
Argument TARGET is ant's target like \"debug\". DIR is the root
directory of the project"
  (let ((default-directory dir))
    (when (file-exists-p "build.xml")
      (compile (concat "ant " target)))))

(defmethod project-compile-project ((proj ede-x-android-project) &optional command)
  "Compile the Android project with ant.
Argument COMMAND is the command to use when compiling."
  (when (null (ede-x-android-compile-ant (oref proj configuration-default)
                                         (ede-project-root-directory proj)))
    (message "Sorry, only 'ant' project supported.")))

(defmethod project-compile-target ((proj ede-x-android-target-java) &optional command)
  (project-compile-project (ede-current-project) command))

(defmethod project-compile-target ((proj ede-x-android-target-xml) &optional command)
  (project-compile-project (ede-current-project) command))

(defun ede-android-debug-project (startdir)
  "Start the android JDB debugger in a buffer.
STARTDIR is the directory to start jdb in.
Depends on `android.el' that comes with the SDK to get going."
  ;; Step one, make sure ddms is running.
  (when (not (cedet-android-ddms-active-p))
    (if (y-or-n-p "No DDMS process running in Emacs.  Start it? ")
        (progn
          (cedet-android-start-ddms)
          ;; Give it a little time.
          (message "Starting DDMS ...")
          (sit-for 10))
      (when (not (y-or-n-p "Start Debugger anyway? " ))
        (signal 'quit nil))))
  ;; Step two, start jdb.

  (unless (featurep 'android)
    (add-to-list 'load-path (expand-file-name "tools/lib/" cedet-android-sdk-root))
    (require 'android)) ;; comes with SDK.

  ;; @TODO - the port should be selectable.
  (android-jdb (car android-jdb-port-history) startdir))

(defmethod project-debug-target ((targ ede-x-android-target-java))
  (ede-android-debug-project (ede-project-root-directory (ede-current-project))))

(defmethod project-debug-target ((targ ede-x-android-target-xml))
  (ede-android-debug-project (ede-project-root-directory (ede-current-project))))

(provide 'ede-x-android)

;; debugging


(defun x-android-project-info ()
  "This function is help to remember the necessary functions."
  (interactive)
  (flet ((insert-list (list)
                      (dolist (l list)
                        (insert (format "  %s\n" l)))))

    (let* ((project (ede-current-project))
           (root (ede-project-root-directory project)))
      (with-current-buffer (get-buffer-create "*x-android-project-info*")
        (erase-buffer)

        (insert (format "Name:\n  %s\n" (oref project name)))
        (insert (format "Version:\n  %s\n" (oref project version)))
        (insert (format "Package:\n  %s\n" (oref project package)))
        (insert (format "Main file:\n  %s\n" (oref project file)))
        (insert (format "Root:\n  %s\n" root))
        (insert (format "Cache directory:\n  %s\n" (x-android-cache/find-cache-directory project)))
        (insert (format "Parsed jars file (SQLite database):\n  %s\n" (x-android-db-parser/find-database project)))

        (insert "Libs:\n")
        (insert-list (x-android-find-libs root))

        (insert "Jars:\n")
        (insert-list (x-android/find-jars root))

        (switch-to-buffer-other-window (current-buffer))))))
