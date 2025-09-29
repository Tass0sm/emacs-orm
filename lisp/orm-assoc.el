;;; -*- lexical-binding: t -*-

(require 'eieio)

;; association class

(defclass orm-assoc ()
  ((class :initarg :class
          :accessor orm-assoc-class)
   (key :initarg :key
        :accessor orm-assoc-key)
   (join-table :initarg :join-table
               :accessor orm-assoc-join-table))
  :documentation
  "A class for associations of an relation-mapped class.")

                                        ; belongs-to

(defclass orm-belongs-to-assoc (orm-assoc) ())

;; self columns
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

                                        ; has-many

(defclass orm-has-many-assoc (orm-assoc) ())

(defun orm-assoc--add-has-many-assoc (table1 table2 options)
  "Get auxiliary table for has-and-belongs-to-many"
  `(progn
     (push (orm-has-many-assoc
	    :class ,table2
	    :key nil
	    :join-table nil)
	   (slot-value (make-instance ,table1) 'associations))))

(defun orm-assoc--defassoc-has-many (table1 table2 options)
  (let* ((name2 (orm-table-name table2))
	 (initarg2 (orm--symbol-to-keyword name2)))
    `(progn
       (orm--augment-table-slots ,table1 ((,name2 :initarg ,initarg2
					     :initform nil)))
       (orm-assoc--add-has-many-assoc table1 table2 options))))

;; (cl-defmethod orm-assoc--insert ((obj1 orm-table) (assoc orm-has-many-assoc) (obj2 orm-table))
;;   "Insert object for has-many association"
;;   )

                                        ; has-and-belongs-to-many

(defclass orm-has-and-belongs-to-many-assoc (orm-assoc) ())

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
      (push (orm-has-and-belongs-to-many-assoc
	     :class ,table2
	     :key nil
	     :join-table ,aux-table-name)
	    (slot-value (make-instance ,table1) 'associations))
      (push (orm-has-and-belongs-to-many-assoc
	     :class ,table1
	     :key nil
	     :join-table ,aux-table-name)
	    (slot-value (make-instance ,table2) 'associations))
      ,aux-table-name)))

(defun orm-assoc--defassoc-has-and-belongs-to-many (table1 table2 options)
  (let* ((name1 (orm-table-name table1))
	 (initarg1 (orm--symbol-to-keyword name1))
	 (name2 (orm-table-name table2))
	 (initarg2 (orm--symbol-to-keyword name2)))
    `(progn
       (orm--augment-table-slots ,table1 ((,name2 :initarg ,initarg2
					     :initform nil)))
       (orm--augment-table-slots ,table2 ((,name1 :initarg ,initarg1
					     :initform nil)))
       ,@(orm-assoc--define-has-and-belongs-to-many-aux-table table1 table2 options))))

(cl-defmethod orm-assoc--insert ((obj1 orm-table) (assoc orm-has-and-belongs-to-many-assoc) (obj2 orm-table))
  "Insert object for has-and-belongs-to-many association"
  (let* ((conn orm-default-conn)
	 (obj-primary-key (aref (orm-table-primary-key (type-of obj1)) 0))
	 (obj-primary-key-value (slot-value obj1 obj-primary-key))
	 (other-primary-key (aref (orm-table-primary-key (type-of obj2)) 0))
	 (other-primary-key-value (slot-value obj2 other-primary-key))
	 (join-table-name (orm-table-name (orm-assoc-join-table assoc))))
    (orm-insert-or-update obj1)
    (orm-insert-or-update obj2)
    (push obj2 (slot-value obj1 (orm-table-name obj2)))
    (push obj1 (slot-value obj2 (orm-table-name obj1)))
    (emacsql-with-transaction conn
      (emacsql conn [:insert :into $i1 :values $v2] join-table-name
               (vector obj-primary-key-value other-primary-key-value)))))

(cl-defmethod orm-assoc--all ((obj orm-table) (assoc orm-has-and-belongs-to-many-assoc))
  "Find all objects for has-and-belongs-to-many association"
  (let* ((conn orm-default-conn)
	 (other (orm-assoc-class assoc))
	 ;; primary key for self table
	 (obj-primary-key (aref (orm-table-primary-key (type-of obj)) 0))
	 (obj-primary-key-value (slot-value obj obj-primary-key))
	 ;; primary key for other table
	 (other-primary-key (aref (orm-table-primary-key other) 0))
	 (other-table-name (orm-table-name other))
         ;; join table keys
         (join-table-name (orm-table-name (orm-assoc-join-table assoc)))
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

(cl-defmethod orm-assoc--update ((obj1 orm-table) (assoc orm-has-and-belongs-to-many-assoc) (obj2 orm-table))
  "Update object in has-and-belongs-to-many association"
  )

(cl-defmethod orm-assoc--delete ((obj1 orm-table) (assoc orm-has-and-belongs-to-many-assoc) (obj2 orm-table))
  "Delete object in has-and-belongs-to-many association"
  )

                                        ; general utilities

;; association class form from a spec
(defun orm-assoc--form-from-spec (type class options)
  "Make assoc instance"
  (pcase type
    (:belongs-to `(orm-belongs-to-assoc
                   :class ,class
	           ,@(orm--filter-plist options
				        (list :key))))
    (:has-one `(orm-has-one-assoc
                :class ,class
	        ,@(orm--filter-plist options
				     (list :key))))
    (:has-many `(orm-has-many-assoc
                 :class ,class
	         ,@(orm--filter-plist options
				      (list :key))))
    (_ nil)))

(defmacro defassoc (table1 type table2 &rest options)
  ""
  (pcase type
    (:belongs-to nil)
    (:has-one nil)
    (:has-many (orm-assoc--defassoc-has-many table1 table2 options))
    (:has-and-belongs-to-many (orm-assoc--defassoc-has-and-belongs-to-many table1 table2 options))
    (_ nil)))

(cl-defmethod orm-assoc-get ((obj orm-table) name)
  "Get association NAME of OBJ."
  (seq-find (lambda (x) (eq name (orm-table-name (orm-assoc-class x)))) (orm-table-associations bolt)))

;; (cl-defmethod orm-all ((assoc-pair orm--assoc-pair))
;;   "Select from table for class in database."
;;   (let* ((obj (orm--assoc-pair-obj assoc-pair))
;; 	 (assoc (orm--assoc-pair-assoc assoc-pair))
;; 	 (other (orm-assoc-class assoc))
;; 	 (conn orm-default-conn)
;; 	 ;; primary key for self table
;; 	 (obj-primary-key (aref (orm-table-primary-key (type-of obj)) 0))
;; 	 (obj-primary-key-value (slot-value obj obj-primary-key))
;; 	 ;; primary key for other table
;; 	 (other-primary-key (aref (orm-table-primary-key other) 0))
;; 	 (other-table-name (orm-table-name other)))
;;     (pcase (orm-assoc-type assoc)
;;       (:has-and-belongs-to-many
;;        (let ((join-table-name (orm-table-name (orm-assoc-join-table assoc)))
;; 	     (obj-join-table-key (intern (format "%s_id" (orm-table-name (type-of obj)))))
;; 	     (other-join-table-key (intern (format "%s_id" (orm-table-name other)))))
;; 	 (emacsql-with-transaction conn
;; 	   (emacsql conn [:select :* :from $i1 :where $i2 :in
;; 				  [:select $i3 :from $i4 :where (= $i5 $s6)]]
;; 		    other-table-name
;; 		    other-primary-key
;; 		    other-join-table-key
;; 		    join-table-name
;; 		    obj-join-table-key
;; 		    obj-primary-key-value))))
;;       (:has-many
;;        (emacsql-with-transaction conn
;; 	 (emacsql-compile conn [:select :* :from $i1]
;; 		  ;; :where $i2 :in
;; 		  ;; [:select $i3 :from $i4 :where (= $i5 $s6)]]
;; 		  other-table-name
;; 		  ;; other-primary-key
;; 		  ;; other-join-table-key
;; 		  ;; join-table-name
;; 		  ;; obj-join-table-key
;; 		  ;; obj-primary-key-value
;; 		  ))))))

;; (cl-defmethod orm-append ((assoc-pair orm--assoc-pair) (other orm-table))
;;   "Insert object into database."
;;   (let* ((obj (orm--assoc-pair-obj assoc-pair))
;; 	 (conn orm-default-conn)
;; 	 (obj-primary-key (aref (orm-table-primary-key (type-of obj)) 0))
;; 	 (obj-primary-key-value (slot-value obj obj-primary-key))
;; 	 (other-primary-key (aref (orm-table-primary-key (type-of other)) 0))
;; 	 (other-primary-key-value (slot-value other other-primary-key))
;; 	 (assoc (orm--assoc-pair-assoc assoc-pair))
;; 	 (join-table-name (orm-table-name (orm-assoc-join-table assoc))))
;;     (pcase (orm-assoc-type assoc)
;;       (:has-and-belongs-to-many
;;        join-table-name
;;        (orm-insert-or-update obj)
;;        (orm-insert-or-update other)
;;        (emacsql-with-transaction conn
;; 	 (emacsql conn [:insert :into $i1 :values $v2] join-table-name
;; 		  (vector obj-primary-key-value
;; 			  other-primary-key-value))))
;;       (_ nil))))

(defun orm-assoc--insert-assoc-row (conn assoc self-primary-key-value other-primary-key-value)
  "Insert one association row into database."
  (let ((join-table-name (orm-table-name (orm-assoc-join-table assoc))))
    (emacsql-with-transaction conn
      (emacsql conn [:insert :into $i1 :values $v2] join-table-name
	       ;; TODO: see if this order is a problem
	       (vector other-primary-key-value
		       self-primary-key-value)))))

(defun orm-assoc--insert-assoc (conn assoc obj)
  "Insert entire association into database."
  (let ((assoc-slot (orm-table-name (orm-assoc-class assoc)))
	(obj-primary-key (orm-primary-key obj)))
    (mapcar (lambda (x) (orm-assoc--insert-assoc-row
			 conn assoc obj-primary-key x))
	    (when (slot-boundp obj assoc-slot)
		(slot-value obj assoc-slot)))))

(defun orm-assoc--update-assoc-row (conn assoc self-primary-key-value other-primary-key-value)
  "Insert one association row into database."
  (let ((join-table-name (orm-table-name (orm-assoc-join-table assoc))))
    (emacsql-with-transaction conn
      (emacsql conn [:insert :into $i1 :values $v2] join-table-name
	       ;; TODO: see if this order is a problem
	       (vector other-primary-key-value
		       self-primary-key-value)))))

(defun orm-assoc--update-assoc (conn assoc obj)
  "Update entire association in database."
  (let ((assoc-slot (orm-table-name (orm-assoc-class assoc)))
	(obj-primary-key (orm-primary-key obj)))
    ;; Identify deleted association rows


    ;; Insert / update remaining assocation rows
    (mapcar (lambda (x) (orm-assoc--update-assoc-row
			 conn assoc obj-primary-key x))
	    (when (slot-boundp obj assoc-slot)
	      (slot-value obj assoc-slot)))))

(defun orm-assoc--delete-assoc (conn assoc obj)
  "Handle deletion for association in database."
  (let ((assoc-slot (orm-table-name (orm-assoc-class assoc)))
	(obj-primary-key (orm-primary-key obj)))
    ;; Depending on association's deletion mode (e.g. delete, nullify) handle
    ;; other items
    ))


;; TODO: has-one association

;; (cl-defmethod orm-assoc--insert ((assoc orm-belongs-to) (obj orm-table))
;;   "Insert object for belongs-to association"
;;   )

;; Interface

(cl-defmethod orm-assoc-insert ((obj1 orm-table) association-name (obj2 orm-table))
  "Insert object for association."
  (orm-assoc--insert obj1 (orm-assoc-get obj1 association-name) obj2))

(cl-defmethod orm-assoc-present-p (obj1 orm-table) association-name (obj2 orm-table)
  ""
  )

(cl-defmethod orm-assoc-first ((obj orm-table) association-name)
  ""
  )

(cl-defmethod orm-assoc-all ((obj orm-table) association-name)
  ""
  (orm-assoc--all obj (orm-assoc-get obj association-name)))

(cl-defmethod orm-assoc-find ((obj orm-table) association-name)
  ""
  )

(cl-defmethod orm-assoc-update ((obj1 orm-table) association-name (obj2 orm-table))
  "Update rows in the association."
  )

(cl-defmethod orm-assoc-delete ((obj1 orm-table) association-name (obj2 orm-table))
  "Delete rows from the association."
  )


(provide 'orm-assoc)
