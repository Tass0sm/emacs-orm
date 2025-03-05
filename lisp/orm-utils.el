;;; -*- lexical-binding: t; -*-

(require 'generator)
(require 'cl-lib)

(iter-defun orm--by-two (l)
  "Iterate over a list in chunks of two elements"
  (while l
    (iter-yield (list (car l) (cadr l)))
    (setq l (cddr l))))

(defun orm--filter-plist (plist keys)
  (apply 'append
	 (cl-loop for x iter-by (orm--by-two plist)
		  if (member (car x) keys)
		  collect x)))

(defun orm--plist-get-with-default (plist keys default)
  (let ((k (cl-find-if (lambda (x) (plist-get plist x)) keys)))
    (if k (plist-get plist k) default)))

(defun orm--eieio-get-initarg (class slot-name)
  (car (rassoc slot-name (eieio--class-initarg-tuples (eieio--full-class-object class)))))

(defun orm--eieio-instance->sexp (object)
  "Return a Lisp form that reconstructs the EIEIO OBJECT."
  (let* ((class (eieio-object-class object))
         (slots (eieio-class-slots class))
         (args
	  (mapcan (lambda (slot)
		    (let* ((sname (cl--slot-descriptor-name slot))
			   (initarg (orm--eieio-get-initarg class sname)))
                      (when (and initarg (slot-boundp object sname))
			    (cons initarg (cons `(quote ,(slot-value object sname)) nil)))
			;; (condition-case nil
			    ;; (error nil)))
		      ;; 	(cons initarg (slot-value object sname)))
		      ))
		  slots)))
    `(,class ,@args)))

(defun orm--slot-descriptor->slot-form (class slot)
  "Return a Lisp form that describes the EIEIO slot."
  (let* ((name (cl--slot-descriptor-name slot))
	 (initarg (orm--eieio-get-initarg class name))
	 ;; returns (quote initform) it seems
         (initform (cadr (cl--slot-descriptor-initform slot)))
         (documentation nil))
    `(,name
      ,@(when initarg `(:initarg ,initarg))
      ,@(unless (eq initform eieio--unbound) `(:initform ,initform))
      ,@(when documentation `(:documentation ,documentation)))))

(defmacro orm--augment-table-slots (table new-slots)
  "Redefine TABLE, adding NEW-COLUMNS to its definition."
  (let* ((class table)
	 (parents (mapcar (lambda (parent) (eieio--class-name parent)) (eieio-class-parents class)))
         (old-slots (mapcar (lambda (s) (orm--slot-descriptor->slot-form class s)) (eieio-class-slots class)))
	 (slots (append old-slots new-slots))
	 (table-name (orm-table-name table))
	 (columns (mapcar 'orm--eieio-instance->sexp (orm-table-columns table)))
	 (associations (mapcar 'orm--eieio-instance->sexp (orm-table-associations table)))
	 (table-constraints (orm-table-constraints table))
         ;; Collect other class options (e.g. :documentation, :abstract)
         (options (let ((doc (eieio--class-docstring (eieio--full-class-object class))))
                    (append (when doc `(:documentation ,doc))))))
    ;; (pcase-dolist (`(,sname . ,soptions) column-specs)
    ;;   (push (orm-column--get-slot-spec sname soptions) slots)
    ;;   (push (orm-column--form-from-spec sname soptions) columns)
    ;;   (push (orm-column--table-constraint-from-spec sname soptions) table-constraints))

    ;; (pcase-dolist (`(,type ,class . ,options) assoc-specs)
    ;;   (push (orm-assoc--form-from-spec type class options) assocs)

    ;;   (when-let ((col-spec (orm-assoc--get-self-column type class options)))
    ;; 	(pcase-let ((`(,sname . ,soptions) col-spec))
    ;; 	  (push (orm-column--get-slot-spec sname soptions) slots)
    ;; 	  (push (orm-column--form-from-spec sname soptions) columns)
    ;; 	  (push (orm-column--table-constraint-from-spec sname soptions) table-constraints))))

    `(defclass ,class ,parents
       (,@(nreverse slots)

	(table-name :initform (quote ,table-name))
	(columns :initform (list ,@columns))
	(associations :initform (list ,@associations))
	(table-constraints :initform ,@table-constraints))
       ,@options)))

(defmacro augment-table (table column-specs)
  "Redefine TABLE, adding NEW-COLUMNS to its definition."
  (let* ((class table)
	 (parents (mapcar (lambda (parent) (eieio--class-name parent)) (eieio-class-parents class)))
         (slots (mapcar (lambda (s) (orm--slot-descriptor->slot-form class s)) (eieio-class-slots class)))
	 (table-name (orm-table-name table))
	 (columns (mapcar 'orm--eieio-instance->sexp (orm-table-columns table)))
	 (associations (mapcar 'orm--eieio-instance->sexp (orm-table-associations table)))
	 (table-constraints (orm-table-constraints table))
         ;; Collect other class options (e.g. :documentation, :abstract)
         (options (let ((doc (eieio--class-docstring (eieio--full-class-object class))))
                    (append (when doc `(:documentation ,doc))))))
    (pcase-dolist (`(,sname . ,soptions) column-specs)
      (push (orm-column--get-slot-spec sname soptions) slots)
      (push (orm-column--form-from-spec sname soptions) columns)
      (push (orm-column--table-constraint-from-spec sname soptions) table-constraints))

    ;; (pcase-dolist (`(,type ,class . ,options) assoc-specs)
    ;;   (push (orm-assoc--form-from-spec type class options) assocs)

    ;;   (when-let ((col-spec (orm-assoc--get-self-column type class options)))
    ;; 	(pcase-let ((`(,sname . ,soptions) col-spec))
    ;; 	  (push (orm-column--get-slot-spec sname soptions) slots)
    ;; 	  (push (orm-column--form-from-spec sname soptions) columns)
    ;; 	  (push (orm-column--table-constraint-from-spec sname soptions) table-constraints))))

    `(defclass ,class ,parents
       (,@(nreverse slots)

	(table-name :initform (quote ,table-name))
	(columns :initform (list ,@columns))
	(associations :initform (list ,@associations))
	(table-constraints :initform ,@table-constraints))
       ,@options)))

(provide 'orm-utils)
