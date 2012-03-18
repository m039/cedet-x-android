;; defclass

(defclass boy ()
  ((name :initform "m039" :type string)))

(defclass punk (boy)
  ((toy :initform "gun" :type string)))

(defclass teen (punk)
  ((money :initform 2 :type number)
   (books)
   (lives :type object)))

(setq local-punk (teen "teen"))

(oset local-punk name "mozg")
(oset-default boy name "m0z9")

(oref local-punk name)
(oref-default local-punk name )

(object-add-to-list local-punk 'books "Potter")
(object-add-to-list local-punk 'books "Tolstoy")

(with-slots ((bs books)) local-punk
  bs)

(defmethod yeild ((obj boy))
  (message "I'm %s!" (oref obj name)))

(defmethod yeild ((obj punk))
  (message "I'm punk-%s!" (oref obj name))
  (call-next-method))

(defmethod suck (obj)
  (message "It is really suck"))

(suck nil)

(yeild local-punk)

;; utilities
(find-class boy)
(class-p 'boy)
(class-name 'boy)
(class-option 'boy)
(object-of-class-p local-punk teen)

;; 9
(object-assoc "m0z9" 'name (list local-punk))
(object-assoc-list 'name (list local-punk))
(eieio-build-class-list 'punk)

;; 10
(eieio-customize-object local-punk)
(eieio-custom-widget-insert local-punk)

;; 11 
(object-slots local-punk)
(class-slot-initarg boy name)

;; 12

(defvar house-list nil)

(defclass house (eieio-instance-tracker)
  ((size :initform "big" :type string)
   (tracking-symbol :initform house-list)))

(setq h (house "hh"))

(eieio-instance-tracker-find "big" 'size 'house-list)

(eieio-describe-class 'house)

(delete-instance h)


;; debugging
(defmethod semanticdb-find-tags-by-name-regexp-method ((table semanticdb-table-jar-directory) regexp &optional tags)
  "In TABLE, find all occurrences of tags matching REGEXP.
Optional argument TAGS is a list of tags to search.
Returns a list of all files in this table's directory that matches REGEXP."
  (if tags (call-next-method)
    ;; Look for a file matching NAME.
    (let* ((dir (oref table directory))
	   (files (oref table filenamecache))
	   (tags nil))
      (dolist (F files)
	(when (string-match regexp F)
	  (setq tags (cons (semanticdb-javap-file-to-tag F table) tags))))
      tags))
  (message "m039: find-tags-completion-regexp %s" tags))

(defmethod semanticdb-find-tags-by-name-method ((table semanticdb-table-java-directory) name &optional tags)
  "In TABLE, find all occurrences of tags with NAME.
Optional argument TAGS is a list of tags to search.
Returns a list of all files in this table's directory that matches NAME."
  (if tags (call-next-method)
    ;; Look for a file matching NAME.
    (let* ((dir (oref table directory))
	   (files (directory-files dir t (concat "^" (regexp-quote name) "\\.java")))
	   (tags nil))
      (dolist (F files)
	(setq tags (cons (semanticdb-javap-file-to-tag F table) tags)))
      tags))
  (message "m039: find-tags-completion-name %s" tags))

(defmethod semanticdb-find-tags-for-completion-method ((table semanticdb-table-jar-directory) prefix &optional tags)
  "In TABLE, find all occurrences of tags starting with PREFIX.
Optional argument TAGS is a list of tags to search.
Returns a list of all files in this table's directory that matches REGEXP."
  (if tags (call-next-method)
    ;; Look for a file matching NAME.
    (let* ((dir (oref table directory))
	   (files (oref table filenamecache))
	   (regexp (concat "^" (regexp-quote prefix)))
	   (tags nil))
      (dolist (F files)
	(when (string-match regexp F)
	  (setq tags (cons (semanticdb-javap-file-to-tag F table) tags))))
      tags))
  (message "m039: find-tags-completion %s" tags))

(define-mode-local-override semantic-analyze-find-tag-sequence
  java-mode (sequence &optional scope typereturn throwsym)
  "For Java buffers, use our javap typecache as a backup search method.
If the default returns only strings, search for the first part of sequence
in the typecache.  Create a return list from that, and append the last
string from sequence to the found tag in the typecache.
SCOPE, TYPERETURN, and THROWSYM are all passed to the default method, but
not used locally."
  (let* ((lastpart (car (last sequence)))
	 (ans (semantic-analyze-find-tag-sequence-default
	       sequence scope typereturn throwsym)))
    ;; If the car of ans is a STRING, then lets try our hack.
    (when (and (> (length ans) 1) (stringp (car ans)))
      (setq ans (append
		 (semanticdb-typecache-find (nreverse (cdr (reverse sequence))))
		 (list lastpart))))
    ;; If we have only one answer, but the name doesn't match the last string in sequence
    ;; then we need to perform a little trickery to fix up the problem.
    (when (and (= (length ans) 1) (semantic-tag-p (car ans)) (stringp lastpart)
	       (not (string= (semantic-tag-name (car ans)) lastpart)))
      (setq ans (append ans (list lastpart))))

    ;; Make sure typereturn has the right data in it.
    ;; THIS IS A QUICK HACK
    ;; Lets look for cases where this fails.
    (when (and typereturn (not (symbol-value typereturn)))
      (set typereturn (nreverse (cdr (reverse ans)))))

    ;; Return whatever we have left.
	(message "m039: analyzer ans %s sequence %s" ans sequence)
    ans))

(define-mode-local-override semanticdb-find-translate-path
  java-mode (path brutish)
  "Override the default path translator for Java.
This will execute the default implementation, but stick a table
including the current package onto the path.  See
`semanticdb-javap-dir-to-compound-table' for how this is done."
  (message "m039: find-tags-translate")
  (if (semanticdb-find-results-p path)
      ;; nil means perform the search over these results.
      nil
    (if brutish
	(semanticdb-find-translate-path-brutish-default path)
      ;; Of the includes case, add our special table.
      (let ((defaultpath (semanticdb-find-translate-path-includes-default path))
	    (javapath (semanticdb-javap-dir-to-compound-table default-directory))
	    ;; Any other packages w/ this name known in classpath or to EDE?
	    (similarpath (semanticdb-javap-paths-for-package (current-buffer))))
	;; If this is a dup, remove.
	(setq similarpath (remq javapath similarpath))
	(append defaultpath (list javapath) similarpath)
	))))

(defun semanticdb-javap-typecache-find-by-include-hack (type &optional path find-file-match)
  "Search through java DIRECTORY databases for TYPE based on PATH.
PATH is a database for the buffer from which the references should be derived.
For each, ask if TYPE is found.  If TYPE is a fully qualified name, leave it alone.
If it is a list, glom it back into a string for the search.
Uses `semanticdb-find-table-for-include' to find the TYPE by fully qualified name
using the same utility as looking for includes which are also fully qualified names."
  (let* ((tname (cond ((stringp type) type)
		      ((listp type) (mapconcat #'identity type "."))))
	 (fauxtag (semantic-tag-new-include tname nil :faux t))
	 (table (semanticdb-find-table-for-include fauxtag path))
	 (ans nil))
    ;; Look the answer up in TABLE from include search.
    (if table
	(let* ((tlist (cond ((listp type) type)
			    ((stringp type) (semantic-analyze-split-name type))))
	       (searchname (if (listp tlist) (car (last tlist))
			     tlist))
	       ;; Get the typecache version of the tags from the table.
	       ;; That will allow us to do a typecache like search
	       (tabletags (semanticdb-typecache-file-tags table))
	       )
	  (setq ans (semantic-find-first-tag-by-name searchname tabletags))
	  )
      ;; If there was no table, then perhaps it is just a package name.
      ;; We can look up "java.com.*" instead of just "java.com.".
      (semantic-tag-set-name fauxtag (concat tname "*"))
      (setq table (semanticdb-find-table-for-include fauxtag path))

      (when (semanticdb-table-jar-directory-child-p table)
	(setq ans (semanticdb-table-javap-table-as-faux-tag table))
	)
      )
	(message "m039: include-hack type %s ans %s" type ans)
    ;; Return what we found.
    ans))

(define-mode-local-override semanticdb-find-table-for-include
  java-mode (importtag &optional table)
  "For an IMPORTTAG in Java, return a table object that contains its tags.
See `semanticdb-find-table-for-include' for details.
Note that for an import of *, this function will return an unusual table
that just points to a database, and must redirect all its calls to its decendents."
  ;; Remove the extension.  The java version of semantic-tag-include-filename
  ;; will glom the .java extension on it.
  (let* ((fname (file-name-sans-extension (semantic-tag-include-filename importtag)))
	 (def (semanticdb-find-table-for-include-default importtag table))
	 (starry (string-match "\\*$" fname)))
	(message "m039: importing fname %s def  starry %s" fname  starry)
    ;; If the default implementation returns something, go with it.
    (if def def
      (let ((cpo (semanticdb-javap-classpath-objects (current-buffer))))
	;; Loop over the classpath, see if we can find it anywhere.
	(catch 'foo
	  (dolist (P cpo)
	    (let ((expanded (expand-file-name fname P)))
	      (cond
	       ((stringp P)
		(if starry
		    (progn
		      (setq def (semanticdb-javap-dir-to-compound-table (file-name-directory expanded)))
		      (when def
			(throw 'foo nil)))
		  ;; If it is a string, just glom the two together.
		  (let ((java  (concat expanded ".java"))
			(class (concat expanded ".class")))
		    (cond 
		     ((file-exists-p java)
		      (setq def (semanticdb-file-table-object java))
		      (throw 'foo nil))
		     ((file-exists-p class)
		      ;; need to javap the thing, but w/out the db...
		      nil)))))
	       
	       ;; For jar databases, we need to extract a table of classes first.
	       ((semanticdb-java-jar-database-child-p P)
		(let ((tab (semanticdb-create-table
			    P (if starry (file-name-directory fname) fname))))
		  (when tab
		    ;; We have a table, now we need to extract the tags from it.
		    ;; TODO - just return the table normally for now.  Fix later.
		    (setq def tab)
		    (throw 'foo nil))))
	       
	       ;; Default - do ntohing
	       (t nil)))))
	def))))

(defun semanticdb-javap-classpath-objects (buffer)
  "Return the classpath for BUFFER as a list of semanticdb objects and strings.
Strings are directories which can be searched.  Database objects
represent jar files."
  (message "m039:semanticdb-javap-classpath-objects")
  (save-current-buffer
    (set-buffer buffer)
	(let ((res (let* ( ;; Try EDE to see if it responds.
					  (edepaths (when ede-object-project
								  (ede-source-paths ede-object-project 'java-mode)))
					  ;; Try EDE's classpath feature
					  (edeclasspath (when ede-object-project
									  (ede-java-classpath ede-object-project)))

					  ;; Try JDEE to see if it knows
					  ;; (jdeep - what to put here??)
					  ;; Try our classpath
					  ;; (classp - what to put here??)
					  ;; Convert to a path
					  (cpaths nil)
					  (ans nil)
					  )
				 ;; Get a list of paths together
				 (dolist (P (append edepaths edeclasspath semanticdb-javap-classpath))
				   (cond
					;; Somtimes a null gets in.  Ignore it.
					((null P)
					 nil)
					;; A directory can be returned as a string.  Should we make a
					;; special dir for this???  @TODO
					((file-directory-p P)
					 (push P cpaths))
					;; Jar files need a special database
					((and (string= "jar" (file-name-extension P))
						  (file-exists-p P))
					 (push
					  (semanticdb-create-database semanticdb-java-jar-database P)
					  cpaths))
					;; What else?  Ignore.
					(t
					 (message "Classpath: %S not found" P))))
				 cpaths)
			   ))
		  (message "m039: res %s" res)
		  res)))

(defun semanticdb-javap-extract-tag-table (jarfile qualifiedclassfile)
  "Within JARFILE, get QUALIFIEDCLASSFILE's tag table.
JARFILE is the full filename to some jar file.
QUALIFIEDCLASSFILE is a filename with package qualifiers
to some class in JARFILE."
  (when (not (file-exists-p jarfile))
    (error "Javap: Cannot find %S" jarfile))
  (message "m039: extract-tag-table j:%s q:%s" jarfile qualifiedclassfile )
  (let ((javapbuff (cedet-javap-get-class
		    jarfile
		    (file-name-sans-extension qualifiedclassfile))))
    (save-current-buffer
      (set-buffer javapbuff)
      (goto-char (point-min))
      ;; The first line says "Compiled from ..." or some-such.
      (insert "// ")

      ;; strip out fully qualified part of class- and interface names
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "\\(class\\|interface\\) \\([^\\. ]*\\.\\)+" nil t)
	  (replace-match "\\1 " nil nil))
	)
      
      ;; Enable java mode and semantic parsing.
      (java-mode)
      (semantic-new-buffer-fcn)
      ;;Get the tags, and strip out buffer information.
      (let ((tagsout (semantic-fetch-tags)))
	;; Changes tags in place.
	(semantic--tag-unlink-list-from-buffer tagsout)
	;; Return the tag table
	tagsout))))

(defmethod semanticdb-create-table ((db semanticdb-java-jar-database) dirorfile)
  "Create a new table in DB for DIR and return it.
This overrides the default `semanticdb-create-table' as this database
creates tables of classes based on files, not files in a directory.
The class of DB contains the class name for the type of table to create.
If the table for DIR exists, return it.
If the table for DIR does not exist, create one."
  (message "m039: create-table dirorfile %s" dirorfile)
  (let ((newtab (semanticdb-file-table db dirorfile)))
    (unless newtab
      (let ((matchingfiles (or (semanticdb-java-jar-package-files db dirorfile)
			       (semanticdb-java-jar-package-one-file
				db dirorfile)))
	    (matchingpkg (semanticdb-java-jar-package-packages db dirorfile)))
	(when matchingfiles
	  ;; Only make a table if there are any matching files in it.
	  (if (= (length matchingfiles) 1)
	      ;; If there is only one table, create a jar-file table.
	      (setq newtab (funcall (oref db new-table-class)
				    (car matchingfiles)
				    :filename (car matchingfiles)))
	    ;; If there are multiple files, then we want a directory
	    ;; The file extractor restricts itself to .class, so no dups?
	    (setq newtab (funcall (oref db new-table-dir-class)
				  dirorfile
				  :directory dirorfile))
	    (oset newtab filenamecache
		  (mapcar 'file-name-nondirectory matchingfiles))
	    (oset newtab packagenamecache matchingpkg)
	    )
	  (oset newtab parent-db db)
	  (object-add-to-list db 'tables newtab t)
	  )))
    newtab))

(define-mode-local-override semanticdb-find-translate-path
  java-mode (path brutish)
  "Override the default path translator for Java.
This will execute the default implementation, but stick a table
including the current package onto the path.  See
`semanticdb-javap-dir-to-compound-table' for how this is done."
  (message "m039: find-translate-path")
  (if (semanticdb-find-results-p path)
      ;; nil means perform the search over these results.
      nil
    (if brutish
	(semanticdb-find-translate-path-brutish-default path)
      ;; Of the includes case, add our special table.
      (let ((defaultpath (semanticdb-find-translate-path-includes-default path))
	    (javapath (semanticdb-javap-dir-to-compound-table default-directory))
	    ;; Any other packages w/ this name known in classpath or to EDE?
	    (similarpath (semanticdb-javap-paths-for-package (current-buffer))))
	;; If this is a dup, remove.
	(setq similarpath (remq javapath similarpath))
	(append defaultpath (list javapath) similarpath)
	))))

(defmethod ede-java-classpath ((this ede-x-android-project))
  (message "m039:java-classpath")
  (list "/opt/android-sdk/platforms/android-14/android.jar"))

(defmethod ede-source-paths ((this ede-x-android-project) mode)
  (list "/opt/android-sdk/sources/android-14/"
		"/opt/android-sdk/sources/android-14/java/lang/"))

(defmethod semanticdb-typecache-file-tags ((table semanticdb-table-java-directory))
  "Create a list of tags from the files in the directory represented by this table."
  (let* ((dir (oref table directory))
	 (files (directory-files dir t "\\.java$"))
	 (tags nil))
    (dolist (F files)
      (setq tags (cons (semanticdb-javap-file-to-tag F table) tags)))
    ;; The typecache can't use our usual tags, but we gave them proxies
    ;; so the typecache can fix them.
	(message "m039:typecache-file-tags java-dir ")
    tags))

(defmethod semanticdb-typecache-file-tags ((table semanticdb-table-jar-directory))
  "Create a list of tags from the files in the directory represented by this table."
  (let* ((dir (oref table directory))
	 (files (oref table filenamecache))
	 (tags nil))
    (dolist (F files)
      (setq tags (cons (semanticdb-javap-file-to-tag F table) tags)))
    ;; The typecache can't use our usual tags, but we gave them proxies
    ;; so the typecache can fix them.
	(message "m039:typecache-file-tags jar-dir ")
    tags))

(defmethod semanticdb-typecache-file-tags ((table semanticdb-table-jar-file))
  "Create a list of tags from the files in the directory represented by this table."
  (semanticdb-refresh-table table)
  ;; For regular files, there is a pile of logic to deal with
  ;; "merging" in all the types from other files.  In Java there is no
  ;; recursion, and should only be one type we care about for each
  ;; class file, so we don't need to deal with that crud.
  (message "m039:typecache-file-tags jar-file ")
  (let ((tagstomerge (semantic-find-tags-by-class
		      'type (semanticdb-get-tags table))))
    (semanticdb-typecache-merge-streams tagstomerge nil)))

(define-mode-local-override semanticdb-typecache-find 
  java-mode (type &optional path find-file-match)
  "For Java, try the default.  If nothing, look in our JAR files.
This is because a string such as com.java.pgk.Class doesn't show up
as a hierarchy from the JAR files in the regular typecache, but we
can find it as a string directly from our directory and jar files."
  (if (not (and (featurep 'semanticdb) semanticdb-current-database))
      nil ;; No DB, no search
    ;; Else, try a few things.
    (or (semanticdb-typecache-find-default type path find-file-match)
		(semanticdb-javap-typecache-find-by-include-hack
		 type path find-file-match)
		(semanticdb-javap-typecache-find-by-include-hack
		 type semanticdb-current-table find-file-match))))

(define-mode-local-override semanticdb-find-table-for-include
  java-mode (importtag &optional table)
  "For an IMPORTTAG in Java, return a table object that contains its tags.
See `semanticdb-find-table-for-include' for details.
Note that for an import of *, this function will return an unusual table
that just points to a database, and must redirect all its calls to its decendents."
  ;; Remove the extension.  The java version of semantic-tag-include-filename
  ;; will glom the .java extension on it.
  (let* ((fname (file-name-sans-extension (semantic-tag-include-filename importtag)))
		 (def (semanticdb-find-table-for-include-default importtag table))
		 (starry (string-match "\\*$" fname)))
    ;; If the default implementation returns something, go with it.
    (if def def
      (let ((cpo (semanticdb-javap-classpath-objects (current-buffer))))
		;; Loop over the classpath, see if we can find it anywhere.
		(catch 'foo
		  (dolist (P cpo)
			(let ((expanded (expand-file-name fname P)))
			  (cond
			   ((stringp P)
				(if starry
					(progn
					  (setq def (semanticdb-javap-dir-to-compound-table (file-name-directory expanded)))
					  (when def
						(throw 'foo nil)))
				  ;; If it is a string, just glom the two together.
				  (let ((java  (concat expanded ".java"))
						(class (concat expanded ".class")))
					(cond 
					 ((file-exists-p java)
					  (setq def (semanticdb-file-table-object java))
					  (throw 'foo nil))
					 ((file-exists-p class)
					  ;; need to javap the thing, but w/out the db...
					  nil)))))
	       
			   ;; For jar databases, we need to extract a table of classes first.
			   ((semanticdb-java-jar-database-child-p P)
				(let ((tab (semanticdb-create-table
							P (if starry (file-name-directory fname) fname))))
				  (when tab
					;; We have a table, now we need to extract the tags from it.
					;; TODO - just return the table normally for now.  Fix later.
					(setq def tab)
					(throw 'foo nil))))
	       
			   ;; Default - do ntohing
			   (t nil)))))
		def))))

(defun semanticdb-javap-typecache-find-by-include-hack (type &optional path find-file-match)
  "Search through java DIRECTORY databases for TYPE based on PATH.
PATH is a database for the buffer from which the references should be derived.
For each, ask if TYPE is found.  If TYPE is a fully qualified name, leave it alone.
If it is a list, glom it back into a string for the search.
Uses `semanticdb-find-table-for-include' to find the TYPE by fully qualified name
using the same utility as looking for includes which are also fully qualified names."
  (message "m039:include hack [0] type %s path  find-file-match %s" type  find-file-match)

  (let* ((tname (cond ((stringp type) type)
					  ((listp type) (mapconcat #'identity type "."))))
		 (fauxtag (semantic-tag-new-include tname nil :faux t))
		 (table (semanticdb-find-table-for-include fauxtag path))
		 (ans nil))

	(message "m039:include hack [1] tname %s fauxtag %s " tname fauxtag)

	(if (null table)
		(message "m039:include hack [1] table null")
	  (message "m039:include hack [1] table %s" (object-name table)))
	
    ;; Look the answer up in TABLE from include search.
    (if table
		(progn
		  (message "m039:include hack [2] ")
		  (let* ((tlist (cond ((listp type) type)
							  ((stringp type) (semantic-analyze-split-name type))))
				 (searchname (if (listp tlist) (car (last tlist))
							   tlist))
				 ;; Get the typecache version of the tags from the table.
				 ;; That will allow us to do a typecache like search
				 (tabletags (semanticdb-typecache-file-tags table))
				 )
			(message "m039:include hack [2] tlist %s searchname %s tabletags %s"
					 tlist
					 searchname
					 (null tabletags))
			(setq ans (semantic-find-first-tag-by-name searchname tabletags))
			))
	  
	  (message "m039:include hack [3] ans %s" (null ans))
	  
      ;; If there was no table, then perhaps it is just a package name.
      ;; We can look up "java.com.*" instead of just "java.com.".
      (semantic-tag-set-name fauxtag (concat tname "*"))
      (setq table (semanticdb-find-table-for-include fauxtag path))

	  (message "m039:include hack [4] table %s" (null table))

      (when (semanticdb-table-jar-directory-child-p table)
		(setq ans (semanticdb-table-javap-table-as-faux-tag table))
		)
      )
	(message "m039:include hack [e] ans %s" (not (null ans)))
    ;; Return what we found.
    ans))


(defmethod semanticdb-typecache-file-tags ((table semanticdb-table))
  "Update the typecache for TABLE, and return the file-tags.
File-tags are those that belong to this file only, and excludes
all included files." 
  (let* (					 ;(idx (semanticdb-get-table-index table))
		 (cache (semanticdb-get-typecache table))
		 )

    ;; Make sure our file-tags list is up to date.

	(message "m039:typecache-file-tags [0] table %s" (object-name table))
	(message "m039:typecache-file-tags [1] cache %s" (oref cache filestream))
	
    (when (not (oref cache filestream))
	  (message "m039:typecache-file-tags [2]")
	  
      (let ((tags  (semantic-find-tags-by-class 'type table))
			(exptags nil))
		;; (message "m039:typecache-file-tags [3] tags %s" tags)
		;; (message "m039:typecache-file-tags [3] table %s" table)
		(when tags
		  (setq tags (semanticdb-typecache-safe-tag-list tags table))
		  (dolist (T tags)
			(push (semanticdb-expand-nested-tag T) exptags))
		  (oset cache filestream (semanticdb-typecache-merge-streams exptags nil)))))

	(message "m039:typecache-file-tags [e] cache %s" (object-name cache))
    ;; Return our cache.
    (oref cache filestream)
    ))
