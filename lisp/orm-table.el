;;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'eieio)
(require 'emacsql)

(defclass orm-table ()
  ((table-name :initarg :table
	       :type symbol
	       :accessor orm-table-name
	       :allocation :class
	       :documentation
	       "The relation/table in which instances of this object are saved.")
   (columns :initarg :columns
	    :type list
	    :accessor orm-table-columns
	    :allocation :class
	    :documentation
	    "A list of column specifications.")
   (associations :initarg :associations
		 :type list
		 :accessor orm-table-associations
		 :allocation :class
		 :documentation
		 "A list of association objects.")
   (table-constraints :initarg :table-constraints
		      :type list
		      :accessor orm-table-constraints
		      :allocation :class
		      :documentation
		      "A list of table constraints."))
  :documentation
  "This special class enables persistence through a database."
  :abstract t)

(cl-defmethod orm-ref ((this orm-table) key)
  (mapcar (lambda (k) (slot-value this k)) key))

(cl-defmethod orm-table-name ((table (subclass orm-table)))
  "Get class table name"
  (orm-table-name (make-instance table)))

(cl-defmethod orm-table-name ((table string))
  "Get class table name"
  (intern table))

(cl-defmethod orm-table-primary-key ((table (subclass orm-table)))
  "Get class primary key name"
  (let* ((obj (make-instance table))
	 (cols (orm-table-columns obj))
	 (pk-cols (cl-remove-if-not (lambda (x) (oref x primary-key)) cols)))
    (when-let ((names (mapcar (lambda (x) (oref x name)) pk-cols)))
      (apply 'vector names))))

(cl-defmethod orm-table-primary-key ((table string))
  "Get default primary key"
  [id])

(defun orm-table--column-constraints (cols)
  "Get column constraints for table"
  (apply 'vector (mapcar 'orm-column-constraint cols)))

(cl-defmethod orm-table-schema ((table (subclass orm-table)))
  "Get schema for table which is made up of a vector of column constraints,
and table constraints"
  (let* ((table (make-instance table))
	 (cols (orm-table-columns table))
	 (col-constraints (orm-table--column-constraints cols))
	 (table-constraints (orm-table-constraints table)))
    (cons col-constraints table-constraints)))

(provide 'orm-table)
