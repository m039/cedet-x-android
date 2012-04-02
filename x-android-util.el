;; This file contains functions with no dependency on othe files.

(require 'cl)

(defun x-android-find-files-in-directory (directory match &optional cut-directory-part)
  "Find all files matched MATCH in DIR, recursively "
  (when (file-directory-p directory)
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
            res))))))

(defun x-android-parse-project.properties (root-dir)
  "Return the alist where:

'target => pointing to android target

'libs => pointing to the list of linked libraries
"
  (let ((ans)
        (project.properties (expand-file-name "project.properties" root-dir)))
    (flet ((find-target ()
                        (goto-char (point-min))
                        (if (re-search-forward "\s*target\s*=\s*\\(.*\\)\s*$" nil t)
                            (push (cons 'target (match-string 1)) ans)
                          (push (cons 'target nil) ans)))
           (find-libs ()
                      (let (libs)
                        (goto-char (point-min))
                        (while (re-search-forward
                                "^\s*android\\.library\\.reference.*\s*=\s*\\(.*\\)\s*$" nil t)
                          (push (expand-file-name (match-string 1) root-dir) libs))
                        (push (cons 'libs libs) ans))))
      (if (file-exists-p project.properties)
          (with-temp-buffer
            (insert-file-contents project.properties)
            (find-target)
            (find-libs)
            ans))
      ans)))

(defun x-android-parse-local.properties (root-dir)
  "Return the alist where:

'sdk.dir => file path to the android sdk"
  (let ((ans)
        (local.properties (expand-file-name "local.properties" root-dir)))
    (flet ((find-sdk.dir ()
                        (goto-char (point-min))
                        (if (re-search-forward "\s*sdk\\.dir\s*=\s*\\(.*\\)\s*$" nil t)
                            (push (cons 'sdk.dir (match-string 1)) ans)
                          (push (cons 'sdk.dir nil) ans)))
           )
      (if (file-exists-p local.properties)
          (with-temp-buffer
            (insert-file-contents local.properties)
            (find-sdk.dir)
            ans))
      ans)))

(defun x-android-find-android.jar (root-dir)
  "Return the path to the appropriate android.jar"
  (let* ((lp (x-android-parse-local.properties root-dir))
         (sdk.dir (cdr (assoc 'sdk.dir lp)))
         (pp (x-android-parse-project.properties root-dir))
         (target (cdr (assoc 'target pp))))

    (let ((android.jar (and sdk.dir
                            target
                            (expand-file-name (concat "platforms/" target "/android.jar")
                                              sdk.dir))))
      (if (and android.jar
               (file-exists-p android.jar))
          android.jar
          nil))))

(defun x-android-find-support-*.jar (root-dir)
  "Return a list of android-support-*.jar in the
<sdk.dir>/extras/android/support."
  (let* ((lp (x-android-parse-local.properties root-dir))
         (sdk.dir (cdr (assoc 'sdk.dir lp))))
    (when sdk.dir
      (flet ((dirs-in-support ()
                              (let (ans)
                                (dolist (f (directory-files
                                            (expand-file-name "extras/android/support" sdk.dir) t))
                                  (when (file-directory-p f)
                                    (push f ans)))
                                (nreverse ans)))
             (support-jars-in-dir (dir)
                                  (directory-files dir t ".+support.+\\.jar$")))
        (let (ans)
          (dolist (dir (dirs-in-support))
            (dolist (jar (support-jars-in-dir dir))
              (push jar ans)))
          (nreverse ans))))))

(defun x-android-find-classpath (root-dir)
  (append (x-android-find-files-in-directory (expand-file-name "libs/" root-dir) "\\.jar$")
          (list (x-android-find-android.jar root-dir))
          (x-android-find-support-*.jar root-dir)))

(defun x-android-find-libs (root-dir)
  "Helpful function to find all libraries in this
project (pointed by the root-dir)"
  (let* (all-libs)
    (flet ((find-libs (dir)
                      (let ((pp (x-android-parse-project.properties dir)))
                        (cdr (assoc 'libs pp))))

           (push-libs (libs)
                      (dolist (l libs)
                        (push l all-libs)))

           (find-libs-r (dir)
                        (let ((libs (find-libs dir)))
                          (push-libs libs)
                          (dolist (l libs)
                            (find-libs-r l)))))
      (find-libs-r root-dir)
      all-libs)))

(defun x-android-find-classpath-recursively (root-dir)
  (let* ((libs (x-android-find-libs root-dir))
         all-classpath)
    (flet ((push-classpath (dir)
                           (dolist (c (x-android-find-classpath dir))
                             (when c
                               (push c all-classpath))))

           (push-current-classpath ()
                                   (push-classpath root-dir))

           (push-all-classpath ()
                               (dolist (l all-libs)
                                 (push-classpath l))))

      (push-current-classpath)
      (push-all-classpath))

    (remove-duplicates all-classpath :test (lambda (a b)
                                             (and (stringp a)
                                                  (stringp b)
                                                  (let ((af (file-name-nondirectory a))
                                                        (bf (file-name-nondirectory b)))
                                                    (and (not (string= af ""))
                                                         (not (string= bf ""))
                                                         (string= af bf))))))))

(defun x-android-fname-if-exists (name)
  "Return the file NAME if it exists as a file."
  (if (file-exists-p name) name))

(defun x-android-find-source-path (root-dir mode)
  (cond ((eq mode 'java-mode)
         (list
          (x-android-fname-if-exists (expand-file-name "src" root-dir))
          (x-android-fname-if-exists (expand-file-name "gen" root-dir))))
        ((or (eq mode 'nxml-mode)                        ;; emacs 23
             (and (eq mode 'sgml-mode) sgml-xml-mode))   ;; emacs 22
         (list
          (x-android-fname-if-exists (expand-file-name "res" root-dir))))
        (t nil)))

(defun x-android-find-source-path-recursively (root-dir mode)
  "Find source-paths in the library projects either."
  (let (ans)
    (dolist (l (nreverse (cons root-dir (x-android-find-libs root-dir))))
      (dolist (p (x-android-find-source-path l mode))
        (when p
          (push p ans))))
    ans))

;; (x-android-find-source-path-recursively rdir 'java-mode)

;; (x-android-find-libs rdir)
;; (x-android-find-source-path rdir 'java-mode)
;; (setq rdir "~/Projects/douchebag/")

;; (x-android-parse-src rdir)
;; (x-android-parse-project.properties rdir)
;; (cdr (assoc 'sdk.dir (x-android-parse-local.properties rdir)

(provide 'x-android-util)
