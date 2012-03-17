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

