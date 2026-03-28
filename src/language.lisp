
(in-package :named-languages)


;; Types
(defclass language ()
  ((name          :initarg :name
                  :reader get-language-name
                  :writer set-language-name)
   (readtable     :initarg :readtable
                  :accessor language-readtable)
   (package       :initarg :package
                  :accessor language-package)
   (documentation :initarg :documentation
                  :accessor language-documentation)))

(defun languagep (obj)
  (typep obj 'language))

(deftype language-designator ()
  `(or null language))

(defun language-designator-p (obj)
  (typep obj 'language-designator))

(deftype name-designator ()
  `(or null symbol))

(defun name-designator-p (obj)
  (typep obj 'name-designator))

(deftype named-language-designator ()
  `(or language-designator symbol))

(defun named-language-designator-p (obj)
  (typep obj 'named-language-designator))

(deftype docstring ()
  `(or null string))

(defun docstringp (obj)
  (typep obj 'docstring))

(deftype package-designator ()
  `(or symbol string package))

(defun package-designator-p (obj)
  (typep obj 'package-designator))


;; Global data
(defvar *languages* (make-hash-table :test 'eq))
(defvar *preregistered-names* '(:common-lisp :standard nil))
(defvar *default-language* (make-instance 'language :name (first *preregistered-names*)
                                                    :readtable (find-readtable nil)
                                                    :package (find-package "COMMON-LISP-USER")
                                                    :documentation "The default language"))


;; Functions
(defun preregistered-name-p (name)
  (and (member name *preregistered-names*) t))

(defun preregistered-named-language-p (language)
  (or (preregistered-name-p language)
      (eq language *default-language*)))

(defun make-language (&key (readtable-name nil) (package-name :common-lisp-user) documentation)
  (check-type readtable-name named-readtable-designator)
  (check-type package-name package-designator)
  (check-type documentation docstring)
  (make-instance 'language :readtable (find-readtable readtable-name)
                           :package (find-package package-name)
                           :documentation documentation))



(defun find-language (name)
  (check-type name named-language-designator)
  (cond
    ((languagep name) name)
    ((preregistered-name-p name) *default-language*)
    (t (gethash name *languages*))))

(defun check-new-language (name language)
  (if-let ((lang (find-language name)))
    (error "Language already exists: ~s" (get-language-name lang)))
  (when (or (eq language *default-language*)
            (find-language (get-language-name language)))
    (error "Trying to register again the language with name ~s" (get-language-name language))))

(defun register-language (name language)
  (check-type name name-designator)
  (check-type language language-designator)
  (check-new-language name language)
  (setf (gethash name *languages*) language)
  (set-language-name language name)
  (values language))

(defun unregister-language (named-language)
  (check-type named-language named-language-designator)
  (unless (preregistered-named-language-p named-language)
    (if-let ((language (find-language named-language)))
      (remhash (get-language-name language) *languages*))))

(defun rename-language (old-name new-name)
  (check-type old-name named-language-designator)
  (check-type new-name name-designator)
  (when (preregistered-named-language-p old-name)
    (error "Cannot rename the default language"))
  (when (preregistered-name-p new-name)
    (error "The new name is designating the default language"))
  (if-let ((language (find-language old-name)))
    (cond
      ((find-language new-name)
       (error "Already registered name: ~s" new-name))
      (t
       (unregister-language language)
       (register-language new-name language)))
    (error "There is no language named ~s" old-name)))

(defun ensure-language (name &optional (default nil defaultp))
  (check-type name name-designator)
  (if-let ((language (find-language name)))
    (values language)
    (if (not defaultp)
        (error "The language ~s does not exist" name)
        (register-language name default))))

(defun copy-language (language)
  (check-type language language-designator)
  (with-slots (readtable package documentation) language
    (make-instance 'language :readtable readtable :package package :documentation documentation)))

(defun copy-named-language (named-language)
  (check-type named-language named-language-designator)
  (if-let ((language (find-language named-language)))
    (copy-language language)
    (error "Language does not exist: ~s" named-language)))

(defmacro deflanguage (name &key readtable-name package-name documentation)
  `(register-language ',name (make-language :readtable ',readtable-name
                                            :package ',package-name
                                            :documentation ',documentation)))

(defmacro in-language (name)
  (with-gensyms (lang readtable package)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (if-let ((,lang (find-language ',name)))
         (with-slots ((,readtable readtable) (,package package)) ,lang
           (in-readtable ,readtable)
           (in-package ,package)
           (values ,lang))
         (error "Language does not exist: ~s" ,lang)))))

(defmethod documentation ((obj language) (doc-type (eql 't)))
  (language-documentation language))

(defmethod documentation (obj (doc-type (eql 'language)))
  (check-type obj named-language-designator)
  (if-let ((language (find-language obj)))
    (language-documentation language)
    (error "Not a language: ~s" obj)))

(defmethod (setf documentation) (new-value (obj language) (doc-type (eql 't)))
  (check-type new-value docstring)
  (setf (language-documentation language) new-value))

(defmethod (setf documentation) (new-value obj (doc-type (eql 'language)))
  (check-type new-value docstring)
  (check-type obj named-language-designator)
  (if-let ((language (find-language obj)))
    (setf (language-documentation language) new-value)
    (error "Not a language: ~s" obj)))
