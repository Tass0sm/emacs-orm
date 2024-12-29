;;; -*- lexical-binding: t -*-

(require 'eieio)
(require 'orm-table)
(require 'orm-utils)

(defclass orm-assoc ()
  ((type :initarg :type)
   (class :initarg :class)
   (key :initarg :key))
  :documentation
  "A class for associations of an relation-mapped class.")

;; For macros

(defun orm-assoc--form-from-spec (type class options)
  "Make assoc instance"
  `(orm-assoc ;; :type ,type
    :class ,class
    ,@(orm--filter-plist options
                         (list :key))))

(defun orm-assoc--get-self-column (type table options)
  (let* ((fk (or (plist-get options :foreign-key)
                 (intern (format "%s-id" (orm-table-name table)))))
         (fk-keyword (intern (format ":%s" (symbol-name fk))))
         (fk-options (orm--filter-plist options
                                        (list :key)))
         (col-options (orm--filter-plist options
                                         (list :primary-key))))
    (pcase type
      (:belongs_to `(,fk :initarg ,fk-keyword
                         :foreign-key '(:references ,table ,@fk-options)
                         ,@col-options))
      (_ nil))))

(provide 'orm-assoc)
