;;; orm.el --- An object relational mapping for emacs lisp
;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'orm-table)
(require 'orm-column)
(require 'orm-assoc)
(require 'orm-db-connection)
(require 'orm-macros)

;; Static Utils

(defun orm-table-p (table)
  "If table is a subclass of orm-table and therefore a table-mapped class"
  (child-of-class-p table orm-table))

(cl-defmethod orm-primary-key ((table orm-table))
  "Get primary key of orm-table object instance TABLE"
  (let ((table-primary-key (aref (orm-table-primary-key (type-of table)) 0)))
    (slot-value table table-primary-key)))

(cl-defmethod orm-column-names ((table orm-table))
  "Get table column names"
  (let* ((cols (orm-table-columns table)))
    (mapcar (lambda (x) (slot-value x 'name)) cols)))

(cl-defmethod orm-column-names ((table (subclass orm-table)))
  "Get table column names"
  (orm-column-names (make-instance table)))

(defun orm--symbol-to-keyword (sym)
  (intern (concat ":" (symbol-name sym))))

;; Static Methods

(cl-defmethod orm-count ((table (subclass orm-table)))
  "Count rows of class table in database."
  (let ((conn orm-default-conn)
	(table (orm-table-name table)))
    (caar (emacsql conn [:select (funcall count *) :from $i1] table))))

(cl-defmethod orm-create ((table (subclass orm-table)))
  "Create table for object class in database."
  (let ((conn orm-default-conn)
	(table (orm-table-name table))
	(schema (orm-table-schema table)))
    (emacsql-with-transaction conn
      (emacsql conn [:create-table $i1 $S2] table schema))))

(cl-defmethod orm-created-p ((table (subclass orm-table)))
  "Is table for object class in SQLITE database?"
  (let ((conn orm-default-conn)
	(table-name (intern (replace-regexp-in-string "-" "_" (symbol-name (orm-table-name table))))))
    (emacsql-with-transaction conn
      (emacsql conn [:select 1 :from 'sqlite_master :where (= name $s1)] table-name))))

(cl-defmethod orm-drop ((table (subclass orm-table)))
  "Create table for object class in database."
  (let ((conn orm-default-conn)
	(table (orm-table-name table)))
    (emacsql-with-transaction conn
      (emacsql conn [:drop-table $i1] table))))


;; Instance Utils

(defun orm--slot-value (obj slot)
  (let ((v (slot-value obj slot)))
    (if (eieio-object-p v)
        (orm-primary-key v)
      v)))

(cl-defmethod orm--object-values ((obj orm-table))
  "Get values vector for object"
  (let* ((class (class-of obj))
	 (column-names (orm-column-names class)))
    (apply 'vector
	   (cl-loop for slot in column-names
		    collect (if (slot-boundp obj slot)
				(orm--slot-value obj slot)
			      nil)))))

;; CRUD

;; Create - orm-insert

(cl-defmethod orm-insert ((obj orm-table) &key (skip-assocs nil))
  "Insert object into database."
  (let ((conn orm-default-conn)
	(table-name (orm-table-name obj))
	(values (orm--object-values obj)))
    (emacsql-with-transaction conn
      (emacsql conn [:insert :into $i1 :values $v2] table-name values)

      (unless skip-assocs
        ;; Insert all that is associated with OBJ
        (mapcar (lambda (assoc) (orm-assoc--insert-assoc conn obj assoc))
                (orm-table-associations obj))))))

;; Read - orm-all, orm-first

(defun orm--interleave (l1 l2)
  (when (and l1 l2)
    (cons (car l1) (cons (car l2) (orm--interleave (cdr l1) (cdr l2))))))

(cl-defmethod orm--make-from-record ((table (subclass orm-table)) record)
  (let ((obj (apply 'make-instance (cons table (orm--interleave (mapcar 'orm--symbol-to-keyword (orm-column-names table)) record)))))
    ;; (mapcar (lambda (assoc) (setf (slot-value obj (orm-assoc-name assoc))
    ;;                               (orm-assoc--all obj assoc)))
    ;;         (seq-remove 'orm-belongs-to-assoc-p (orm-table-associations obj)))
    obj))

(cl-defmethod orm-all ((table (subclass orm-table)))
  "Select all rows from orm class TABLE."
  (let* ((conn orm-default-conn)
	 (table-name (orm-table-name table))
	 (records (emacsql-with-transaction conn
		    (emacsql conn [:select :* :from $S1] (vector table-name)))))
    (mapcar (lambda (r) (orm--make-from-record table r)) records)))

(cl-defmethod orm-first ((table (subclass orm-table)))
  "Select first row from orm class TABLE."
  (let* ((conn orm-default-conn)
	 (table-name (orm-table-name table))
	 (record (emacsql-with-transaction conn
		   (emacsql conn [:select :* :from $S1 :asc :limit $s2] (vector table-name) 1))))
    (when record
      (orm--make-from-record table (car record)))))

(cl-defmethod orm-find ((table (subclass orm-table)) id)
  "Find object in TABLE by primary-key ID."
  (let* ((conn orm-default-conn)
	 (table-name (orm-table-name table))
	 (primary-key (aref (orm-table-primary-key table) 0))
	 (record (car (emacsql-with-transaction conn
			(emacsql conn (vector :select :* :from '$S1 :where (list '= '$i2 id) :limit '$s3)
				 (vector table-name) primary-key 1)))))
    (when record
      (orm--make-from-record table record))))

(cl-defmethod orm-present-p ((obj orm-table))
  "Return '(1) if object is in the database and nil otherwise."
  (let* ((conn orm-default-conn)
	 (table-name (orm-table-name obj))
	 (primary-key (aref (orm-table-primary-key (class-of obj)) 0))
	 (primary-key-value (slot-value obj primary-key))
	 (record (car (emacsql-with-transaction conn
			(emacsql conn (vector :select :1 :from '$S1 :where (list '= '$i2 '$s3))
				 (vector table-name) primary-key primary-key-value)))))
    record))

;; Update - orm-update

(defun orm--zip-with-set-exprs (l1 l2)
  (when (and l1 l2)
    (cons (list '= (car l1) (car l2)) (orm--zip-with-set-exprs (cdr l1) (cdr l2)))))

(defun orm--make-set-exprs (obj)
  (let ((column-names (orm-column-names obj))
	;; Convert values vector to list
	(values (append (orm--object-values obj) nil)))
    (apply 'vector (orm--zip-with-set-exprs column-names values))))

(cl-defmethod orm-update ((obj orm-table) &key (skip-assocs nil))
  "Update object in database."
  (let* ((conn orm-default-conn)
	 (table-name (orm-table-name obj))
	 (primary-key (aref (orm-table-primary-key (class-of obj)) 0))
	 (primary-key-value (slot-value obj primary-key)))
    (emacsql-with-transaction conn
      (emacsql conn (vector :update '$i1 :set (orm--make-set-exprs obj) :where (list '= '$i2 primary-key-value))
	       table-name primary-key)

      (unless skip-assocs
        ;; Update all that is associated with OBJ
        (mapcar (lambda (assoc) (orm-assoc--update-assoc conn obj assoc))
                (orm-table-associations obj))))))

(cl-defmethod orm-insert-or-update ((obj orm-table) &key (skip-assocs nil))
  "Insert object in database or update if already present."
  (if (orm-present-p obj)
      (orm-update obj :skip-assocs skip-assocs)
    (orm-insert obj :skip-assocs skip-assocs)))

;; Delete - orm-delete

(cl-defmethod orm-delete ((obj orm-table) &key (skip-assocs nil))
  "Delete object in database."
  (let* ((conn orm-default-conn)
	 (table-name (orm-table-name obj))
	 (primary-key (aref (orm-table-primary-key (class-of obj)) 0))
	 (primary-key-value (slot-value obj primary-key)))
    (emacsql-with-transaction conn
      (emacsql conn (vector :delete-from '$i1 :where (list '= '$i2 primary-key-value))
	       table-name primary-key)

      (unless skip-assocs
        ;; Handle deletion for all that is associated with "obj"
        (mapcar (lambda (assoc) (orm-assoc--delete-assoc conn obj assoc))
	        (orm-table-associations obj))))))


(provide 'orm)
