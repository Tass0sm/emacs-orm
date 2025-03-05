;;; -*- lexical-binding: t -*-

(require 'eieio)
(require 'inflections)

;; association class

(defclass orm-assoc ()
  ((name :initarg :name
         :accessor orm-assoc-name
         :documentation "Name of slot storing associated data")
   (class :initarg :class
          :accessor orm-assoc-class
          :documentation "Class of associated data"))
  :documentation
  "A class for associations of an relation-mapped class.")


(cl-defgeneric orm-assoc--insert (obj1 assoc obj2)
  "insert")

(cl-defgeneric orm-assoc--present-p (obj1 assoc obj2)
  "present-p")

(cl-defgeneric orm-assoc--all (obj assoc)
  "all")

(cl-defgeneric orm-assoc--find (obj1 assoc pk)
  "find")

(cl-defgeneric orm-assoc--delete (obj1 assoc obj2)
  "delete")

;; For maintaining associations in other operations

(cl-defgeneric orm-assoc--insert-assoc (obj1 assoc)
  "inserts entire association")

(cl-defgeneric orm-assoc--update-assoc (obj1 assoc)
  "updates entire association")

(cl-defgeneric orm-assoc--delete-assoc (obj1 assoc)
  "deletes entire association")

                                        ; utils

(defun orm-assoc--pluralize-symbol (sym)
  (intern (inflection-pluralize-string (symbol-name sym))))

(defun orm-assoc--singularize-symbol (sym)
  (intern (inflection-singularize-string (symbol-name sym))))

                                        ; belongs-to

(defclass orm-belongs-to-assoc (orm-assoc)
  ((reverse-name :initarg :reverse-name
                 :accessor orm-assoc-reverse-name)
   (key :initarg :key
        :accessor orm-assoc-key)))

(defun orm-assoc--defassoc-belongs-to (table1 table2 options)
  (let* ((name1 (orm-assoc--singularize-symbol table1))
         (initarg1 (orm--symbol-to-keyword name1))
         (name2 (orm-assoc--singularize-symbol table2))
	 (initarg2 (orm--symbol-to-keyword name2)))
    `(progn
       (orm--augment-table-slots ,table1 ((,name2 :initarg ,initarg2
        				          :initform nil)))

       ;; TODO: use key from options
       (pcase-let ((`(,sname . ,soptions) (orm-assoc--get-self-column :belongs-to ,table2 (list :foreign-key ,table2))))
         (push (eval (orm-column--form-from-spec sname soptions))
               (slot-value (make-instance ,table1) 'columns))
	 (push (orm-column--table-constraint-from-spec sname soptions)
               (slot-value (make-instance ,table1) 'table-constraints)))

       (push (orm-belongs-to-assoc
              :name (quote ,name2)
              :reverse-name (quote ,name1)
	      :class ,table2
	      :key (quote ,table2)) ;; TODO: use key from options
	     (slot-value (make-instance ,table1) 'associations)))))

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

(cl-defmethod orm-assoc--insert-assoc (conn obj (assoc orm-belongs-to-assoc))
  "Insert entire association into database."
  (let ((in-obj (slot-value obj (orm-assoc-name assoc))))
    (when (and in-obj (not (orm-present-p in-obj)))
      (orm-insert in-obj :skip-assocs t))))

(cl-defmethod orm-assoc--update-assoc (conn obj (assoc orm-belongs-to-assoc))
  "Update entire association in database."
  (let ((in-obj (slot-value obj (orm-assoc-name assoc))))
    (when in-obj (orm-insert-or-update in-obj :skip-assocs t))
    (orm-insert-or-update obj :skip-assocs t)))

(cl-defmethod orm-assoc--delete-assoc (conn obj (assoc orm-belongs-to-assoc))
  "UNSURE: Handle deletion for association in database."
  (let ((in-obj (slot-value obj (orm-assoc-name assoc))))
    (setf (slot-value obj (orm-assoc-key assoc)) nil)
    (when (orm-present-p obj)
      (orm-delete obj))))

(cl-defmethod orm-assoc--insert ((obj1 orm-table) (assoc orm-belongs-to-assoc) (obj2 orm-table))
  "Insert object for belongs-to association"
  (let ((conn orm-default-conn))
    (setf (slot-value obj1 (orm-assoc-name assoc)) obj2)
    (orm-assoc--insert-assoc conn obj1 assoc)))

(cl-defmethod orm-assoc--present-p ((obj1 orm-table) (assoc orm-belongs-to-assoc) (obj2 orm-table))
  "Check if obj2 is present in obj1's belongs-to association."
  (let* ((conn orm-default-conn)
         (other-table (orm-assoc-class assoc))
	 (other-table-name (orm-table-name other-table))
	 (other-table-pk (orm-table-primary-key other-table))
         (fkv (orm--slot-value obj1 (orm-assoc-key assoc)))
	 (obj2-pkv (orm-primary-key obj2)))
    (car (emacsql-with-transaction conn
           (emacsql conn [:select :1 :from $i1 :where (and (= $i2 $s3) (= $i4 $s5))]
                    other-table-name other-table-pk fkv other-table-pk obj2-pkv)))))

(cl-defmethod orm-assoc--all ((obj orm-table) (assoc orm-belongs-to-assoc))
  "Find the object for belongs-to association"
  (let* ((conn orm-default-conn)
         (other-table (orm-assoc-class assoc))
	 (other-table-name (orm-table-name other-table))
         (other-table-pk (orm-table-primary-key other-table))
         (other-fkv (slot-value obj (orm-assoc-key assoc))))
    (when other-fkv
      (let ((fkv (orm-primary-key (slot-value obj (orm-assoc-key assoc)))))
        (mapcar (lambda (r) (orm--make-from-record other-table r))
                (emacsql-with-transaction conn
                  (emacsql conn [:select :* :from $i1 :where (= $i2 $s3)]
                           other-table-name other-table-pk fkv)))))))

(cl-defmethod orm-assoc--find ((obj orm-table) (assoc orm-belongs-to-assoc) id)
  "Find objects by id for belongs-to association"
  (let* ((conn orm-default-conn)
         (other-table (orm-assoc-class assoc))
	 (other-table-name (orm-table-name other-table))
         (other-table-pk (orm-table-primary-key other-table))
         (other (slot-value obj (orm-assoc-key assoc))))
    (when other
         (let ((fkv (orm-primary-key other)))
           (let ((records (emacsql-with-transaction conn
                            (emacsql conn [:select :* :from $i1 :where (and (= $i2 $s3) (= $i4 $s5)) :limit $s6]
                                     other-table-name other-table-pk fkv other-table-pk id 1))))
             (orm--make-from-record other-table (car records)))))))

(cl-defmethod orm-assoc--delete ((obj1 orm-table) (assoc orm-belongs-to-assoc) obj2)
  "Delete object in belongs-to association"
  (let ((conn orm-default-conn))
    (setf (slot-value obj1 (orm-assoc-name assoc)) nil)
    (orm-assoc--update-assoc conn obj1 assoc)))

(cl-defmethod orm-assoc--delete ((obj1 orm-table) (assoc orm-belongs-to-assoc) (obj2 orm-table))
  "Delete object in belongs-to association"
  (let ((conn orm-default-conn)
        (obj1-y (slot-value obj1 (orm-assoc-name assoc))))
    (when (and obj1-y (equal (orm-primary-key obj2) (orm-primary-key obj1-y)))
      (setf (slot-value obj1 (orm-assoc-name assoc)) nil)
      (orm-assoc--update-assoc conn obj1 assoc))))

                                        ; has-one

(defclass orm-has-one-assoc (orm-assoc)
  ((reverse-name :initarg :reverse-name
                 :accessor orm-assoc-reverse-name)
   (key :initarg :key
        :accessor orm-assoc-key)))

(defun orm-assoc--defassoc-has-one (table1 table2 options)
  (let* ((name1 (orm-assoc--singularize-symbol table1))
         (initarg1 (orm--symbol-to-keyword name1))
         (name2 (orm-assoc--singularize-symbol table2))
	 (initarg2 (orm--symbol-to-keyword name2)))
    `(progn
       (orm--augment-table-slots ,table1 ((,name2 :initarg ,initarg2
        				          :initform nil)))

       (push (orm-has-one-assoc
              :name (quote ,name2)
              :reverse-name (quote ,name1)
	      :class ,table2
	      :key (quote ,table1)) ;; TODO: use key from options
	     (slot-value (make-instance ,table1) 'associations))

       (unless (orm-assoc-get ,table2 ,name1)
         (orm--augment-table-slots ,table2 ((,name1 :initarg ,initarg1
        				            :initform nil)))

         ;; TODO: use key from options
         (pcase-let ((`(,sname . ,soptions) (orm-assoc--get-self-column :belongs-to ,table1 (list :foreign-key ,table1))))
           (push (eval (orm-column--form-from-spec sname soptions))
                 (slot-value (make-instance ,table2) 'columns))
	   (push (orm-column--table-constraint-from-spec sname soptions)
                 (slot-value (make-instance ,table2) 'table-constraints)))

         (push (orm-belongs-to-assoc
                :name (quote ,name1)
                :reverse-name (quote ,name2)
	        :class ,table1
	        :key (quote ,table1)) ;; TODO: use key from options
	       (slot-value (make-instance ,table2) 'associations))))))

(cl-defmethod orm-assoc--insert-assoc (conn obj (assoc orm-has-one-assoc))
  "Insert has-one association into database."
  (let ((other (slot-value obj (orm-assoc-name assoc))))
    (when other
      (setf (slot-value other (orm-assoc-key assoc)) obj)
      (orm-insert-or-update other :skip-assocs t))))

(cl-defmethod orm-assoc--update-assoc (conn obj (assoc orm-has-one-assoc))
  "Update has-one association in database."
  (let ((other (slot-value obj (orm-assoc-name assoc))))
    (when other
      (setf (slot-value other (orm-assoc-key assoc)) obj)
      (orm-insert-or-update other :skip-assocs t))))

(cl-defmethod orm-assoc--delete-assoc (conn obj (assoc orm-has-one-assoc))
  "Handle deletion for association in database."
  (let ((other (slot-value obj (orm-assoc-name assoc))))
    (when other
      (setf (slot-value other (orm-assoc-key assoc)) nil)
      (orm-delete other))))

(cl-defmethod orm-assoc--insert ((obj1 orm-table) (assoc orm-has-one-assoc) (obj2 orm-table))
  "Insert object for has-one association"
  (let ((conn orm-default-conn)
        (previous-ownee (slot-value obj1 (orm-assoc-name assoc)))
        (previous-owner (slot-value obj2 (orm-assoc-reverse-name assoc))))

    ;; if obj1 already owns something, update that thing to have no owner
    (when (and previous-ownee (not (equal (orm-primary-key previous-ownee) (orm-primary-key obj2))))
      (setf (slot-value previous-ownee (orm-assoc-reverse-name assoc)) nil)
      (orm-insert-or-update previous-ownee :skip-assocs t))

    ;; add obj2 to obj1 slot
    (setf (slot-value obj1 (orm-assoc-name assoc)) obj2)
    ;; insert association, which updates owner of obj2
    (orm-assoc--insert-assoc conn obj1 assoc)))

(cl-defmethod orm-assoc--all ((obj orm-table) (assoc orm-has-one-assoc))
  "Find all objects for has-many association"
  (let ((conn orm-default-conn)
        (other-table (orm-assoc-class assoc))
	(other-table-name (orm-table-name (orm-assoc-class assoc)))
	(pkv (orm-primary-key obj))
        (fk (orm-assoc-key assoc)))
    (mapcar (lambda (r) (orm--make-from-record other-table r))
            (emacsql-with-transaction conn
              (emacsql conn [:select :* :from $i1 :where (= $i2 $s3)]
                       other-table-name fk pkv)))))

(cl-defmethod orm-assoc--find ((obj orm-table) (assoc orm-has-one-assoc) id)
  "Find objects by id for has-to association"
  (let ((conn orm-default-conn)
        (other-table (orm-assoc-class assoc))
	(other-table-name (orm-table-name (orm-assoc-class assoc)))
	(pkv (orm-primary-key obj))
        (fk (orm-assoc-key assoc)))
    (car (mapcar (lambda (r) (orm--make-from-record other-table r))
                 (emacsql-with-transaction conn
                   (emacsql conn [:select :* :from $i1 :where (and (= $i2 $s3) (= $i4 $s5))]
                            other-table-name fk pkv (orm-table-primary-key other-table) id))))))

(cl-defmethod orm-assoc--present-p ((obj1 orm-table) (assoc orm-has-one-assoc) (obj2 orm-table))
  "Test for presence of obj2 in obj1's has-many association"
  (let ((conn orm-default-conn)
        (other-table (orm-assoc-class assoc))
	(other-table-name (orm-table-name (orm-assoc-class assoc)))
        (fk (orm-assoc-key assoc))
	(obj1-pkv (orm-primary-key obj1))
        (obj2-pk (orm-table-primary-key (type-of obj2)))
	(obj2-pkv (orm-primary-key obj2)))
    (car (emacsql-with-transaction conn
           (emacsql conn [:select :1 :from $i1 :where (and (= $i2 $s3) (= $i4 $s5))]
                    other-table-name fk obj1-pkv obj2-pk obj2-pkv)))))

(cl-defmethod orm-assoc--delete ((obj1 orm-table) (assoc orm-has-one-assoc) obj2)
  "Delete object in has-one association"
  (let ((conn orm-default-conn)
        (obj1-y (slot-value obj1 (orm-assoc-name assoc))))
    (setf (slot-value obj1-y (orm-assoc-reverse-name assoc)) nil)
    (orm-insert-or-update obj1-y :skip-assocs t)
    (setf (slot-value obj1 (orm-assoc-name assoc)) nil)
    (orm-assoc--update-assoc conn obj1 assoc)))

(cl-defmethod orm-assoc--delete ((obj1 orm-table) (assoc orm-has-one-assoc) (obj2 orm-table))
  "Delete object in has-one association"
  (let ((conn orm-default-conn)
        (obj1-y (slot-value obj1 (orm-assoc-name assoc))))
    (when (equal (orm-primary-key obj2) (orm-primary-key obj1-y))
      (setf (slot-value obj1-y (orm-assoc-reverse-name assoc)) nil)
      (orm-insert-or-update obj1-y :skip-assocs t)
      (setf (slot-value obj1 (orm-assoc-name assoc)) nil)
      (orm-assoc--update-assoc conn obj1 assoc))))

                                        ; has-many

(defclass orm-has-many-assoc (orm-assoc)
  ((reverse-name :initarg :reverse-name
                 :accessor orm-assoc-reverse-name)
   (key :initarg :key
        :accessor orm-assoc-key)))

(defun orm-assoc--defassoc-has-many (table1 table2 options)
  (let* ((name1 (orm-assoc--singularize-symbol table1))
         (initarg1 (orm--symbol-to-keyword name1))
         (name2 (orm-assoc--pluralize-symbol table2))
	 (initarg2 (orm--symbol-to-keyword name2)))
    `(progn
       (orm--augment-table-slots ,table1 ((,name2 :initarg ,initarg2
					          :initform nil)))

       (push (orm-has-many-assoc
              :name (quote ,name2)
              :reverse-name (quote ,name1)
	      :class ,table2
	      :key (quote ,table1)) ;; TODO: use key from options
	     (slot-value (make-instance ,table1) 'associations))

       (unless (orm-assoc-get ,table2 ,name1)
         (orm--augment-table-slots ,table2 ((,name1 :initarg ,initarg1
        				            :initform nil)))

         ;; TODO: use key from options
         (pcase-let ((`(,sname . ,soptions) (orm-assoc--get-self-column :belongs-to ,table1 (list :foreign-key ,table1))))
           (push (eval (orm-column--form-from-spec sname soptions))
                 (slot-value (make-instance ,table2) 'columns))
	   (push (orm-column--table-constraint-from-spec sname soptions)
                 (slot-value (make-instance ,table2) 'table-constraints)))

         (push (orm-belongs-to-assoc
                :name (quote ,name1)
                :reverse-name (quote ,name2)
	        :class ,table1
	        :key (quote ,table1)) ;; TODO: use key from options
	       (slot-value (make-instance ,table2) 'associations))))))

(cl-defmethod orm-assoc--insert-assoc (conn obj (assoc orm-has-many-assoc))
  "Insert entire association into database."
  (let* ((in-db (orm-assoc--all obj assoc))
         (in-obj (slot-value obj (orm-assoc-name assoc)))
         (to-insert (cl-set-difference in-obj in-db :test (lambda (x y) (equal (orm-primary-key x)
                                                                               (orm-primary-key y))))))
    (mapcar (lambda (x)
              (setf (slot-value x (orm-assoc-key assoc)) obj)
              (orm-insert-or-update x :skip-assocs t))
            to-insert)))

(cl-defmethod orm-assoc--update-assoc (conn obj (assoc orm-has-many-assoc))
  "Update entire association in database."
  (let* ((in-db (orm-assoc--all obj assoc))
         (in-obj (slot-value obj (orm-assoc-name assoc)))
         (to-insert (cl-set-difference in-obj in-db :test (lambda (x y) (equal (orm-primary-key x)
                                                                               (orm-primary-key y)))))
         (to-update (cl-intersection in-obj in-db :test (lambda (x y) (equal (orm-primary-key x)
                                                                             (orm-primary-key y)))))
         (to-delete (cl-set-difference in-db in-obj :test (lambda (x y) (equal (orm-primary-key x)
                                                                               (orm-primary-key y))))))
    (mapcar (lambda (x)
              (setf (slot-value x (orm-assoc-key assoc)) obj)
              (orm-insert x :skip-assocs t))
            to-insert)
    (mapcar (lambda (x)
              (setf (slot-value x (orm-assoc-key assoc)) obj)
              (orm-update x :skip-assocs t))
            to-update)
    (mapcar (lambda (x)
              (setf (slot-value x (orm-assoc-key assoc)) nil)
              (orm-update x :skip-assocs t))
            to-delete)))

(cl-defmethod orm-assoc--delete-assoc (conn obj (assoc orm-has-many-assoc))
  "Handle deletion for association in database."
  (let* ((in-db (orm-assoc--all obj assoc))
         (in-obj (slot-value obj (orm-assoc-name assoc)))
         (to-insert (cl-set-difference in-obj in-db :test (lambda (x y) (equal (orm-primary-key x)
                                                                               (orm-primary-key y)))))
         (to-update (cl-intersection in-obj in-db :test (lambda (x y) (equal (orm-primary-key x)
                                                                             (orm-primary-key y)))))
         (to-delete (cl-set-difference in-db in-obj :test (lambda (x y) (equal (orm-primary-key x)
                                                                               (orm-primary-key y))))))
    ;; Update objects
    (mapcar (lambda (x) (setf (slot-value x (orm-assoc-key assoc)) nil)) in-obj)
    ;; Delete objects in database
    (mapcar (lambda (x) (orm-delete x :skip-assocs t)) in-db)))

(cl-defmethod orm-assoc--insert ((obj1 orm-table) (assoc orm-has-many-assoc) (obj2 orm-table))
  "Insert object for has-many association"
  (let ((conn orm-default-conn)
        (previous-owner (slot-value obj2 (orm-assoc-reverse-name assoc))))

    ;; if obj2 already belongs to something, update it and that thing
    (when (and previous-owner (not (equal (orm-primary-key previous-owner) (orm-primary-key obj1))))
      (let ((prev-owner-ys (slot-value previous-owner (orm-assoc-name assoc))))
        ;; remove obj2 from previous owner
        (setf (slot-value previous-owner (orm-assoc-name assoc))
              (cl-remove-if (lambda (y) (equal (orm-primary-key obj2) (orm-primary-key y))) prev-owner-ys))))

    ;; add obj2 to obj1 slot
    (cl-pushnew obj2 (slot-value obj1 (orm-assoc-name assoc)))
    ;; insert association, which updates owner of obj2
    (orm-assoc--insert-assoc conn obj1 assoc)))

(cl-defmethod orm-assoc--delete ((obj1 orm-table) (assoc orm-has-many-assoc) (obj2 orm-table))
  "Delete object in has-many association"
  (let ((conn orm-default-conn)
        (obj1-ys (slot-value obj1 (orm-assoc-name assoc))))
      (setf (slot-value obj1 (orm-assoc-name assoc))
            (cl-remove-if (lambda (y) (equal (orm-primary-key obj2) (orm-primary-key y))) obj1-ys))
      (setf (slot-value obj2 (orm-assoc-reverse-name assoc)) nil)
      (orm-assoc--update-assoc conn obj1 assoc)))

(cl-defmethod orm-assoc--all ((obj orm-table) (assoc orm-has-many-assoc))
  "Find all objects for has-many association"
  (let ((conn orm-default-conn)
        (other-table (orm-assoc-class assoc))
	(other-table-name (orm-table-name (orm-assoc-class assoc)))
	(pkv (orm-primary-key obj))
        (fk (orm-assoc-key assoc)))
    (mapcar (lambda (r) (orm--make-from-record other-table r))
            (emacsql-with-transaction conn
              (emacsql conn [:select :* :from $i1 :where (= $i2 $s3)]
                       other-table-name fk pkv)))))

(cl-defmethod orm-assoc--find ((obj orm-table) (assoc orm-has-many-assoc) id)
  "Find objects by id for has-many association"
  (let ((conn orm-default-conn)
        (other-table (orm-assoc-class assoc))
	(other-table-name (orm-table-name (orm-assoc-class assoc)))
	(pkv (orm-primary-key obj))
        (fk (orm-assoc-key assoc)))
    (let ((records (emacsql-with-transaction conn
                     (emacsql conn [:select :* :from $i1 :where (and (= $i2 $s3) (= $i4 $s5)) :limit $s6]
                              other-table-name fk pkv (orm-table-primary-key other-table) id 1))))
      (orm--make-from-record other-table (car records)))))

(cl-defmethod orm-assoc--present-p ((obj1 orm-table) (assoc orm-has-many-assoc) (obj2 orm-table))
  "Test for presence of obj2 in obj1's has-many association"
  (let ((conn orm-default-conn)
        (other-table (orm-assoc-class assoc))
	(other-table-name (orm-table-name (orm-assoc-class assoc)))
        (fk (orm-assoc-key assoc))
	(obj1-pkv (orm-primary-key obj1))
        (obj2-pk (orm-table-primary-key (type-of obj2)))
	(obj2-pkv (orm-primary-key obj2)))
    (car (emacsql-with-transaction conn
           (emacsql conn [:select :1 :from $i1 :where (and (= $i2 $s3) (= $i4 $s5))]
                    other-table-name fk obj1-pkv obj2-pk obj2-pkv)))))

                                        ; has-and-belongs-to-many

(defclass orm-has-and-belongs-to-many-assoc (orm-assoc)
  ((reverse-name :initarg :reverse-name
                 :accessor orm-assoc-reverse-name)
   (key1 :initarg :key1
         :accessor orm-assoc-key1)
   (class1 :initarg :class1
           :accessor orm-assoc-class1)
   (key2 :initarg :key2
         :accessor orm-assoc-key2)
   (class2 :initarg :class2
           :accessor orm-assoc-class2)
   (join-table :initarg :join-table
               :accessor orm-assoc-join-table)))

(defclass orm-assoc--join-table-row ()
  ((value1 :initarg :value1
           :accessor orm-assoc--join-table-row-value1)
   (value2 :initarg :value2
           :accessor orm-assoc--join-table-row-value2)))

(defun orm-assoc--make-joint-table-row (assoc x y)
  "Make join table row from association and two objects"
  (if (eq (orm-assoc-class1 assoc) (type-of x))
      (orm-assoc--join-table-row :value1 (orm-primary-key x)
                                 :value2 (orm-primary-key y))
    (orm-assoc--join-table-row :value1 (orm-primary-key y)
                               :value2 (orm-primary-key x))))


;; Aux Tables
(defun orm-assoc--get-join-table-name (table-name class)
  (let ((sorted-names (sort (list table-name class) (lambda (x y) (compare-strings x nil nil y nil nil)))))
    (intern (format "%s-%s" (car sorted-names) (cadr sorted-names)))))

(defun orm-assoc--define-has-and-belongs-to-many-aux-table (table1 name1 table2 name2 options)
  "Get auxiliary table for has-and-belongs-to-many"
  (let ((aux-table-name (orm-assoc--get-join-table-name
			 (symbol-name (orm-table-name table1))
			 (symbol-name (orm-table-name table2))))
        (join-table-key1 (intern (format "%s_id" (orm-table-name table1))))
	(join-table-key2 (intern (format "%s_id" (orm-table-name table2)))))
    `((deftable ,aux-table-name ()
		,(plist-get options :extra-columns)
		:associations
		((:belongs-to ,table1)
		 (:belongs-to ,table2)))
      (push (orm-has-and-belongs-to-many-assoc
             :name (quote ,name2)
             :reverse-name (quote ,name1)
	     :class ,table2
	     :join-table ,aux-table-name
             :class1 ,table1 :class2 ,table2
             :key1 (quote ,join-table-key1) :key2 (quote ,join-table-key2))
	    (slot-value (make-instance ,table1) 'associations))
      (push (orm-has-and-belongs-to-many-assoc
             :name (quote ,name1)
             :reverse-name (quote ,name2)
	     :class ,table1
	     :join-table ,aux-table-name
             :class1 ,table1 :class2 ,table2
             :key1 (quote ,join-table-key1) :key2 (quote ,join-table-key2))
	    (slot-value (make-instance ,table2) 'associations))
      ,aux-table-name)))

(defun orm-assoc--defassoc-has-and-belongs-to-many (table1 table2 options)
  (let* ((name1 (orm-assoc--pluralize-symbol table1))
	 (initarg1 (orm--symbol-to-keyword name1))
	 (name2 (orm-assoc--pluralize-symbol table2))
	 (initarg2 (orm--symbol-to-keyword name2)))
    `(progn
       (orm--augment-table-slots ,table1 ((,name2 :initarg ,initarg2
					     :initform nil)))
       (orm--augment-table-slots ,table2 ((,name1 :initarg ,initarg1
					     :initform nil)))
       ,@(orm-assoc--define-has-and-belongs-to-many-aux-table table1 name1 table2 name2 options))))

(cl-defmethod orm-assoc--insert-assoc-row (conn (assoc orm-has-and-belongs-to-many-assoc) (row orm-assoc--join-table-row))
  "Insert one association row into database."
  (let ((join-table-name (orm-table-name (orm-assoc-join-table assoc))))
    (emacsql-with-transaction conn
      (emacsql conn [:insert :into $i1 :values $v2] join-table-name
	       (vector (orm-assoc--join-table-row-value1 row)
		       (orm-assoc--join-table-row-value2 row))))))

(cl-defmethod orm-assoc--insert-assoc (conn obj (assoc orm-has-and-belongs-to-many-assoc))
  "Insert entire association into database."
  (let* ((in-db (orm-assoc--all obj assoc))
         (in-obj (slot-value obj (orm-assoc-name assoc)))
         (to-insert (cl-set-difference in-obj in-db :test (lambda (x y) (equal (orm-primary-key x)
                                                                               (orm-primary-key y))))))
    (mapcar (lambda (x) (orm-assoc--insert-assoc-row conn assoc (orm-assoc--make-joint-table-row assoc obj x))) to-insert)))

(cl-defmethod orm-assoc--update-assoc-row (conn (assoc orm-has-and-belongs-to-many-assoc) (row orm-assoc--join-table-row))
  "TODO: Update one association row in join table."
  nil
  ;; (let ((join-table-name (orm-table-name (orm-assoc-join-table assoc))))
  ;;   (emacsql-with-transaction conn
  ;;     (emacsql conn (vector :update '$i1 :set (orm--make-set-exprs obj) :where (list '= '$i2 primary-key-value))
  ;;              table-name primary-key)))
  )

(cl-defmethod orm-assoc--delete-assoc-row (conn (assoc orm-has-and-belongs-to-many-assoc) (row orm-assoc--join-table-row))
  "Delete one association row in join table."
  (let ((join-table-name (orm-table-name (orm-assoc-join-table assoc)))
        (key1 (orm-assoc-key1 assoc))
        (key2 (orm-assoc-key2 assoc)))
    (emacsql-with-transaction conn
      (emacsql conn [:delete-from $i1 :where (and (= $i2 $s3) (= $i4 $s5))]
               join-table-name
               key1 (orm-assoc--join-table-row-value1 row)
               key2 (orm-assoc--join-table-row-value2 row)))))

(cl-defmethod orm-assoc--update-assoc (conn obj (assoc orm-has-and-belongs-to-many-assoc))
  "Update entire association in database."
  (let* ((in-db (orm-assoc--all obj assoc))
         (in-obj (slot-value obj (orm-assoc-name assoc)))
         (to-insert (cl-set-difference in-obj in-db :test (lambda (x y) (equal (orm-primary-key x)
                                                                               (orm-primary-key y)))))
         (to-update (cl-intersection in-obj in-db :test (lambda (x y) (equal (orm-primary-key x)
                                                                             (orm-primary-key y)))))
         (to-delete (cl-set-difference in-db in-obj :test (lambda (x y) (equal (orm-primary-key x)
                                                                               (orm-primary-key y))))))
    (mapcar (lambda (x) (orm-assoc--insert-assoc-row conn assoc (orm-assoc--make-joint-table-row assoc obj x))) to-insert)
    (mapcar (lambda (x) (orm-assoc--update-assoc-row conn assoc (orm-assoc--make-joint-table-row assoc obj x))) to-update)
    (mapcar (lambda (x) (orm-assoc--delete-assoc-row conn assoc (orm-assoc--make-joint-table-row assoc obj x))) to-delete)))

(cl-defmethod orm-assoc--delete-assoc (conn obj (assoc orm-has-and-belongs-to-many-assoc))
  "Handle deletion for association in database."
  (let ((in-db (orm-assoc--all obj assoc))
        (in-obj (slot-value obj (orm-assoc-name assoc))))

    (mapcar (lambda (x)
              (when (cl-member-if (lambda (y) (equal (orm-primary-key x) (orm-primary-key y))) in-db)
                (setf (slot-value x (orm-assoc-reverse-name assoc)) nil)))
            in-obj)

    (setf (slot-value obj (orm-assoc-name assoc)) nil)

    (mapcar (lambda (x) (orm-assoc--delete-assoc-row conn assoc (orm-assoc--make-joint-table-row assoc obj x))) in-db)))

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

(cl-defmethod orm-assoc--present-p ((obj1 orm-table) (assoc orm-has-and-belongs-to-many-assoc) (obj2 orm-table))
  "Test for presence of obj2 in obj1's has-and-belongs-to-many association"
  (let ((conn orm-default-conn)
	(join-table-name (orm-table-name (orm-assoc-join-table assoc)))
	(join-table-row (orm-assoc--make-joint-table-row assoc obj1 obj2))
        (key1 (orm-assoc-key1 assoc))
        (key2 (orm-assoc-key2 assoc)))
    (emacsql-with-transaction conn
      (emacsql conn [:select :1 :from $i1 :where (and (= $i2 $s3) (= $i4 $s5))]
	       join-table-name
               key1 (orm-assoc--join-table-row-value1 join-table-row)
               key2 (orm-assoc--join-table-row-value2 join-table-row)))))

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
    (mapcar (lambda (r) (orm--make-from-record other r))
            (emacsql-with-transaction conn
              (emacsql conn [:select :* :from $i1 :where $i2 :in
			             [:select $i3 :from $i4 :where (= $i5 $s6)]]
	               other-table-name
	               other-primary-key
	               other-join-table-key
	               join-table-name
	               obj-join-table-key
	               obj-primary-key-value)))))

(cl-defmethod orm-assoc--find ((obj orm-table) (assoc orm-has-and-belongs-to-many-assoc) id)
  "Find objects by id for has-and-belongs-to-many association"
  (let* ((conn orm-default-conn)
         ;; Other class info
         (other (orm-assoc-class assoc))
         (other-table-name (orm-table-name other))
         (other-primary-key (aref (orm-table-primary-key other) 0))
         (other-join-table-key (intern (format "%s_id" (orm-table-name other))))
         ;; Join table info
	 (join-table-name (orm-table-name (orm-assoc-join-table assoc)))
	 (key1 (orm-assoc-key1 assoc))
         (key2 (orm-assoc-key2 assoc))
         (value-pair (if (eq (orm-assoc-class1 assoc) (type-of obj))
                         (cons (orm-primary-key obj) id)
                       (cons id (orm-primary-key obj)))))
    (let ((record (car (emacsql-with-transaction conn
                         (emacsql conn [:select :* :from $i1 :where $i2 :in
                                                [:select $i3 :from $i4 :where (and (= $i5 $s6) (= $i7 $s8))]]
                                  other-table-name
	                          other-primary-key
	                          other-join-table-key
                                  join-table-name
                                  key1 (car value-pair)
                                  key2 (cdr value-pair))))))
      (orm--make-from-record other record))))

(cl-defmethod orm-assoc--delete ((obj1 orm-table) (assoc orm-has-and-belongs-to-many-assoc) (obj2 orm-table))
  "Delete object in has-and-belongs-to-many association"
  (let ((conn orm-default-conn)
        (obj1-ys (slot-value obj1 (orm-assoc-name assoc)))
        (obj2-xs (slot-value obj2 (orm-assoc-reverse-name assoc))))
    (setf (slot-value obj1 (orm-assoc-name assoc))
          (cl-remove-if (lambda (y) (equal (orm-primary-key obj2) (orm-primary-key y))) obj1-ys))
    (setf (slot-value obj2 (orm-assoc-reverse-name assoc))
          (cl-remove-if (lambda (x) (equal (orm-primary-key x) (orm-primary-key obj1))) obj2-xs))
    (orm-assoc--update-assoc conn obj1 assoc)))


                                        ; general utilities

;; association class form from a spec
(defun orm-assoc--form-from-spec (type class options)
  "Make assoc instance"
  (pcase type
    (:belongs-to `(orm-belongs-to-assoc
                   :name (quote ,(orm--plist-get-with-default options '(:key :foreign-key) class))
                   :reverse-name nil ;; TODO, maybe remove this functionality
                   :class ,class
                   :key (quote ,(orm--plist-get-with-default options '(:key :foreign-key) class))
	           ,@(orm--filter-plist options
                                        (list :key))))
    ;; (:has-one `(orm-has-one-assoc
    ;;             :name (quote ,(orm-assoc--pluralize-symbol class))
    ;;             :class ,class
    ;;             :key (quote ,(orm--plist-get-with-default options '(:key :foreign-key) class))
    ;;             ,@(orm--filter-plist options
    ;;     			     (list :key))))
    ;; (:has-many `(orm-has-many-assoc
    ;;              :name (quote ,(orm-assoc--pluralize-symbol class))
    ;;              :class ,class
    ;;              :key (quote ,(orm--plist-get-with-default options '(:key :foreign-key) class))
    ;;              ,@(orm--filter-plist options
    ;;     			      (list :key))))
    (_ nil)))

(defmacro defassoc (table1 type table2 &rest options)
  ""
  (pcase type
    (:belongs-to (orm-assoc--defassoc-belongs-to table1 table2 options))
    (:has-one (orm-assoc--defassoc-has-one table1 table2 options))
    (:has-many (orm-assoc--defassoc-has-many table1 table2 options))
    (:has-and-belongs-to-many (orm-assoc--defassoc-has-and-belongs-to-many table1 table2 options))
    (_ nil)))

(defun orm-assoc-get (obj name)
  "Get association NAME of OBJ."
  (seq-find (lambda (x) (eq name (orm-assoc-name x))) (orm-table-associations obj)))

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

;; TODO: has-one association

;; (cl-defmethod orm-assoc--insert ((assoc orm-belongs-to) (obj orm-table))
;;   "Insert object for belongs-to association"
;;   )

;; Interface

(defun orm-assoc-insert (obj1 association-name obj2)
  "Insert object for association."
  (orm-assoc--insert obj1 (orm-assoc-get obj1 association-name) obj2))

(defun orm-assoc-present-p (obj1 association-name obj2)
  "Check if OBJ2 is present in OBJ1's association ASSOCIATION-NAME"
  (orm-assoc--present-p obj1 (orm-assoc-get obj1 association-name) obj2))

(defun orm-assoc-first (obj association-name)
  ""
  (car (orm-assoc--all obj (orm-assoc-get obj association-name))))

(defun orm-assoc-all (obj association-name)
  ""
  (orm-assoc--all obj (orm-assoc-get obj association-name)))

(defun orm-assoc-find (obj association-name id)
  ""
  (orm-assoc--find obj (orm-assoc-get obj association-name) id))

(defun orm-assoc-delete (obj1 association-name obj2)
  "Delete rows from the association."
  (orm-assoc--delete obj1 (orm-assoc-get obj1 association-name) obj2))


(provide 'orm-assoc)
