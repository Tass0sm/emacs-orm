;;; orm.el --- An object relational mapping for emacs lisp
;; -*- coding: utf-8; lexical-binding: t; -*-

(require 's)
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

(defun symbol-to-keyword (sym)
  (intern-soft (s-prepend ":" (symbol-name sym))))

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
  (let* ((class (class-of this))
	 (column-names (orm-column-names class)))
    (apply 'vector
	   (cl-loop for slot in column-names
		    collect (if (slot-boundp this slot)
				(slot-value this slot)
			      nil)))))

;; CRUD

;; Create - orm-insert

(cl-defmethod orm-insert ((this orm-table))
  "Insert object into database."
  (let ((conn orm-default-conn)
	(table-name (orm-table-name this))
	(values (orm--object-values this)))
    (emacsql-with-transaction conn
      (emacsql conn [:insert :into $i1 :values $v2] table-name values))))

;; Read - orm-all, orm-first

(cl-defmethod orm--make-from-record ((table (subclass orm-table)) record)
  (apply 'make-instance (cons table (-interleave (-map 'symbol-to-keyword (orm-column-names table)) record))))

(cl-defmethod orm-all ((table (subclass orm-table)))
  "Select from table for class in database."
  (let* ((conn orm-default-conn)
	 (table-name (orm-table-name table))
	 (records (emacsql-with-transaction conn
		    (emacsql conn [:select :* :from $S1] (vector table-name)))))
    (mapcar (lambda (r) (orm--make-from-record table r)) records)))

(cl-defmethod orm-first ((table (subclass orm-table)))
  "Select from table for class in database."
  (let* ((conn orm-default-conn)
	 (table-name (orm-table-name table))
	 (record (emacsql-with-transaction conn
		   (emacsql conn [:select :* :from $S1 :asc :limit $s2] (vector table-name) 1))))
    (when record
      (orm--make-from-record table (car record)))))

;; Update - orm-update

(defun orm-make-set-exprs (obj)
  (let ((column-names (orm-column-names obj))
	;; Convert values vector to list
	(values (append (orm--object-values obj) nil)))
    (apply 'vector (-zip-with (lambda (x y) (list '= x y)) column-names values))))

(cl-defmethod orm-update ((this orm-table))
  "Update object in database."
  (let* ((conn orm-default-conn)
	 (table-name (orm-table-name this))
	 (primary-key (aref (orm-table-primary-key (class-of this)) 0))
	 (primary-key-value (slot-value this primary-key)))
    (emacsql-with-transaction conn
      (emacsql conn (vector :update '$i1 :set (orm-make-set-exprs this) :where (list '= '$i2 primary-key-value))
	       table-name primary-key))))

;; Delete - orm-delete

(provide 'orm)
