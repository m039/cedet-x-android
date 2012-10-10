;;
;; This file contains utils to store the parsed data.
;;

(defconst x-android-cache-directory (expand-file-name "~/.x-android"))

(defun x-android-cache/mkdir ( dir )
  (unless (file-exists-p dir)
    (mkdir dir)))

(defun x-android-cache/rmdir ( dir )
  "Remove the `dir' recursively."
  (when (file-exists-p dir)
    (delete-directory dir t)))

(defun x-android-cache/rmdir-in-cache-directory ( relative-path )
  (x-android-cache/rmdir (concat
                          (file-name-as-directory x-android-cache-directory)
                          relative-path)))

(defun x-android-cache/mkdir-in-cache-directory ( relative-path )
  (x-android-cache/mkdir (concat
                          (file-name-as-directory x-android-cache-directory)
                          relative-path)))


(defun x-android-cache/find-project-directory-by-name ( project-name )
  (let ((project-directory (concat
                            (file-name-as-directory x-android-cache-directory)
                            "Projects/"
                            project-name)))

    (x-android-cache/mkdir x-android-cache-directory)
    (x-android-cache/mkdir-in-cache-directory "Projects")
    (x-android-cache/mkdir project-directory)

    project-directory))

(defun x-android-cache/find-cache-directory ( &optional project )
  (let* ((project (if project project e(ede-current-project)))
         (name (concat (oref project package)
                       "."
                       (oref project name))))
    (file-name-as-directory (x-android-cache/find-project-directory-by-name name))))
