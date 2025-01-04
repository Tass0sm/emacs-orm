;;; orm.el --- emacs lisp object relational mapping -*- lexical-binding: t; -*-

(require 'orm-table)
(require 'orm-column)
(require 'orm-assoc)
(require 'orm-db-connection)
(require 'orm-macros)

;; Static Utils

(cl-defmethod orm-column-names ((table orm-table))
  "Get table column names"
  (let* ((cols (orm-table-columns table)))
    (mapcar (lambda (x) (slot-value x 'name)) cols)))

(cl-defmethod orm-column-names ((table (subclass orm-table)))
  "Get table column names"
  (orm-column-names (make-instance table)))

;; Static Methods

(cl-defmethod orm-count ((table (subclass orm-table)))
  "Count rows of class table in database."
  (let ((conn orm-default-conn)
	(table (orm-table-name table)))
    (emacsql conn [:select (funcall count *) :from $i1] table)))

(cl-defmethod orm-create ((table (subclass orm-table)))
  "Create table for object class in database."
  (let ((conn orm-default-conn)
	(table (orm-table-name table))
	(schema (orm-table-schema table)))
    (emacsql-with-transaction conn
      (emacsql conn [:create-table $i1 $S2] table schema))))

;; Instance Utils

(cl-defmethod orm--object-values ((this orm-table))
  "Get values vector for object"
  (let* ((class (eieio-object-class this))
	 (column-names (orm-column-names class)))
    (apply 'vector
	   (cl-loop for slot in column-names
		    collect (if (slot-boundp this slot)
				(slot-value this slot)
			      nil)))))

;; Insert

(cl-defmethod orm-insert ((this orm-table))
  "Insert object into database."
  (let ((conn orm-default-conn)
	(table-name (orm-table-name this))
	(values (orm--object-values this)))
    (emacsql-with-transaction conn
      (emacsql conn [:insert :into $i1 :values $v2] table-name values))))

;; (cl-defmethod orm-replace ((this orm-table))
;;   "Insert object into database."
;;   (let ((conn orm-default-conn)
;; 	(table-name (orm-table-name this))
;; 	(values (orm-object-values this)))
;;     (emacsql-with-transaction conn
;;       (emacsql conn [:insert :into $i1 :values $v2] table-name values))))


(provide 'orm)
