;;; -*- lexical-binding: t -*-

(require 'eieio)

(defclass orm-assoc ()
  ((type :initarg :type
	 :accessor orm-assoc-type)
   (class :initarg :class
	  :accessor orm-assoc-class)
   (key :initarg :key)
   (join-table :initarg :join-table
	       :accessor orm-assoc-join-table))
  :documentation
  "A class for associations of an relation-mapped class.")

;; utility for redefining classes with an additional slots


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

(defun orm-assoc--define-has-and-belongs-to-many-aux-table (table1 table2 options)
  "Get auxiliary table for has-and-belongs-to-many"
  (let ((aux-table-name (orm-assoc--get-join-table-name
			 (symbol-name (orm-table-name table1))
			 (symbol-name (orm-table-name table2)))))
    `((deftable ,aux-table-name ()
		,(plist-get options :extra-columns)
		:associations
		((:belongs-to ,table1)
		 (:belongs-to ,table2)))
      (push (orm-assoc
	     :type :has-and-belongs-to-many
	     :class ,table2
	     :key nil
	     :join-table ,aux-table-name)
	    (slot-value (make-instance ,table1) 'associations))
      (push (orm-assoc
	     :type :has-and-belongs-to-many
	     :class ,table1
	     :key nil
	     :join-table ,aux-table-name)
	    (slot-value (make-instance ,table2) 'associations))
      ,aux-table-name)))

(defun orm-assoc--add-has-many-assoc (table1 table2 options)
  "Get auxiliary table for has-and-belongs-to-many"
  `(progn
     (push (orm-assoc
	    :type :has-many
	    :class ,table2
	    :key nil
	    :join-table nil)
	   (slot-value (make-instance ,table1) 'associations))))

(defun orm-assoc--defassoc-has-many (table1 table2 options)
  (let* ((name2 (orm-table-name table2))
	 (initarg2 (orm--symbol-to-keyword name2)))
    `(progn
       (augment-table-slots ,table1 ((,name2 :initarg ,initarg2)))
       (orm-assoc--add-has-many-assoc table1 table2 options))))


(defun orm-assoc--defassoc-has-and-belongs-to-many (table1 table2 options)
  (let* ((name1 (orm-table-name table1))
	 (initarg1 (orm--symbol-to-keyword name1))
	 (name2 (orm-table-name table2))
	 (initarg2 (orm--symbol-to-keyword name2)))
    `(progn
       (augment-table-slots ,table1 ((,name2 :initarg ,initarg2)))
       (augment-table-slots ,table2 ((,name1 :initarg ,initarg1)))
       ,@(orm-assoc--define-has-and-belongs-to-many-aux-table table1 table2 options))))

(defmacro defassoc (table1 type table2 &rest options)
  ""
  (pcase type
    (:belongs-to nil)
    (:has-one nil)
    (:has-many (orm-assoc--defassoc-has-many table1 table2 options))
    (:has-and-belongs-to-many (orm-assoc--defassoc-has-and-belongs-to-many table1 table2 options))
    (_ nil)))

;; eieio--add-new-slot

;; methods

(defclass orm--assoc-pair ()
  ((obj :initarg :obj
	:accessor orm--assoc-pair-obj)
   (assoc :initarg :assoc
	  :accessor orm--assoc-pair-assoc))
  :documentation
  "A class for has-and-belongs-to-many associations.")

(cl-defmethod orm-get-assoc ((obj orm-table) name)
  "Get assoc from orm object based on name."
  (orm--assoc-pair :obj obj
		   :assoc (car (orm-table-associations obj))))


(cl-defmethod orm-all ((assoc-pair orm--assoc-pair))
  "Select from table for class in database."
  (let* ((obj (orm--assoc-pair-obj assoc-pair))
	 (assoc (orm--assoc-pair-assoc assoc-pair))
	 (other (orm-assoc-class assoc))
	 (conn orm-default-conn)
	 ;; primary key for self table
	 (obj-primary-key (aref (orm-table-primary-key (type-of obj)) 0))
	 (obj-primary-key-value (slot-value obj obj-primary-key))
	 ;; primary key for other table
	 (other-primary-key (aref (orm-table-primary-key other) 0))
	 (other-table-name (orm-table-name other)))
    (pcase (orm-assoc-type assoc)
      (:has-and-belongs-to-many
       (let ((join-table-name (orm-table-name (orm-assoc-join-table assoc)))
	     (obj-join-table-key (intern (format "%s_id" (orm-table-name (type-of obj)))))
	     (other-join-table-key (intern (format "%s_id" (orm-table-name other)))))
	 (emacsql-with-transaction conn
	   (emacsql conn [:select :* :from $i1 :where $i2 :in
				  [:select $i3 :from $i4 :where (= $i5 $s6)]]
		    other-table-name
		    other-primary-key
		    other-join-table-key
		    join-table-name
		    obj-join-table-key
		    obj-primary-key-value))))
      (:has-many
       (emacsql-with-transaction conn
	 (emacsql-compile conn [:select :* :from $i1]
		  ;; :where $i2 :in
		  ;; [:select $i3 :from $i4 :where (= $i5 $s6)]]
		  other-table-name
		  ;; other-primary-key
		  ;; other-join-table-key
		  ;; join-table-name
		  ;; obj-join-table-key
		  ;; obj-primary-key-value
		  ))))))

(cl-defmethod orm-append ((assoc-pair orm--assoc-pair) (other orm-table))
  "Insert object into database."
  (let* ((obj (orm--assoc-pair-obj assoc-pair))
	 (conn orm-default-conn)
	 (obj-primary-key (aref (orm-table-primary-key (type-of obj)) 0))
	 (obj-primary-key-value (slot-value obj obj-primary-key))
	 (other-primary-key (aref (orm-table-primary-key (type-of other)) 0))
	 (other-primary-key-value (slot-value other other-primary-key))
	 (assoc (orm--assoc-pair-assoc assoc-pair))
	 (join-table-name (orm-table-name (orm-assoc-join-table assoc))))
    (pcase (orm-assoc-type assoc)
      (:has-and-belongs-to-many
       join-table-name
       (orm-insert-or-update obj)
       (orm-insert-or-update other)
       (emacsql-with-transaction conn
	 (emacsql conn [:insert :into $i1 :values $v2] join-table-name
		  (vector obj-primary-key-value
			  other-primary-key-value))))
      (_ nil))))

;; (cl-defmethod (

(provide 'orm-assoc)
