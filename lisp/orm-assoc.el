;;; -*- lexical-binding: t -*-

(require 'eieio)

(defclass orm-assoc ()
  ((type :initarg :type)
   (class :initarg :class)
   (key :initarg :key))
  :documentation
  "A class for associations of an relation-mapped class.")

;; For macros

;; Form
(defun orm-assoc--form-from-spec (type class options)
  "Make assoc instance"
  `(orm-assoc :type ,type
	      :class ,class
	      ,@(orm--filter-plist options
				   (list :key))))

;; Self Columns

(defun orm-assoc--get-belongs-to-self-column (table options)
  (let* ((fk (or (plist-get options :foreign-key)
		 (intern (format "%s-id" (orm-table-name table)))))
	 (fk-keyword (intern (format ":%s" (symbol-name fk))))
	 (fk-options (orm--filter-plist options
					(list :key)))
	 (col-options (orm--filter-plist options
					 (list :primary-key))))
    `(,fk :initarg ,fk-keyword
	  :foreign-key '(:references ,table ,@fk-options)
	  ,@col-options)))

(defun orm-assoc--get-self-column (type table options)
  (pcase type
    (:belongs-to (orm-assoc--get-belongs-to-self-column table options))
    (_ nil)))


;; Post-hoc Associations

;; Aux Tables

(defun orm-assoc--get-join-table-name (table-name class)
  (let ((sorted-names (sort (list table-name class) (lambda (x y) (compare-strings x nil nil y nil nil)))))
    (intern (format "%s-%s" (car sorted-names) (cadr sorted-names)))))

(defun orm-assoc--get-has-and-belongs-to-many-aux-table (table1 table2)
  "Get auxiliary table for has-and-belongs-to-many"
  (let ((aux-table-name (orm-assoc--get-join-table-name
			 (symbol-name (orm-table-name table1))
			 (symbol-name (orm-table-name table2)))))
    `(deftable ,aux-table-name ()
	       ()
	       :associations
	       ((:belongs-to ,table1)
		(:belongs-to ,table2)))))

(defmacro defassoc (table1 type table2)
  ""
  (pcase type
    (:has-and-belongs-to-many (orm-assoc--get-has-and-belongs-to-many-aux-table table1 table2))
    (_ nil)))

(provide 'orm-assoc)
