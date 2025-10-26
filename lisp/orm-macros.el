(require 'cl-lib)

(require 'orm-column)
(require 'orm-table)
(require 'orm-assoc)
(require 'orm-utils)

(defun orm--filter-options-and-doc (options-and-doc)
  (cond ((and (stringp (car options-and-doc))
              (/= 1 (% (length options-and-doc) 2)))
         (error "Too many arguments to `defclass'"))
        ((and (symbolp (car options-and-doc))
              (/= 0 (% (length options-and-doc) 2)))
         (error "Too many arguments to `defclass'")))

  (if (stringp (car options-and-doc))
      (setq options-and-doc
            (cons :documentation options-and-doc)))

  (orm--filter-plist options-and-doc
		     (list :documentation
			   :allow-nil-initform
			   :custom-groups
			   :abstract
			   :method-invocation-order)))

(defun orm-macros--find-col-form-by-name (col-forms name)
  (cl-find-if
   (lambda (x) (let ((x-name (cadr (plist-get (cdr x) :name))))
                 (eq x-name name)))
   columns))

(defmacro deftable (name mixins column-specs &rest options-and-doc)
  "Define a table, which is a class that maps to a relation."
  (let ((table-name (or (plist-get options-and-doc :table) name))
	(assoc-specs (plist-get options-and-doc :associations))
	(table-constraints (plist-get options-and-doc :constraints))
	slots columns assocs)

    ;; Iterate over column specs and construct slot specs, column object forms,
    ;; and table constraints
    (pcase-dolist (`(,sname . ,soptions) column-specs)
      (push (orm-column--get-slot-spec sname soptions) slots)
      (push (orm-column--form-from-spec sname soptions) columns)
      (push (orm-column--table-constraint-from-spec sname soptions) table-constraints))

    ;; Iterate over association specs and construct slot specs, column object forms,
    ;; and table constraints
    (pcase-dolist (`(,type ,class . ,options) assoc-specs)
      (push (orm-assoc--form-from-spec type class options) assocs)

      (when-let ((col-spec (orm-assoc--get-self-column type class options)))
	(pcase-let ((`(,sname . ,soptions) col-spec))
	  (push (orm-column--get-slot-spec sname soptions) slots)
	  (push (orm-column--form-from-spec sname soptions) columns)
	  (push (orm-column--table-constraint-from-spec sname soptions) table-constraints))))

    ;; If primary key is specified with a table constraint, modify the column
    ;; forms to identify themselves as part of the primary key
    (when-let ((pk-constraint (assoc :primary-key table-constraints)))
      (mapcar (lambda (x)
                (let ((col-form (orm-macros--find-col-form-by-name columns x)))
                  (cl-callf plist-put (cdr col-form) :composite-key t)))
              (cadr pk-constraint)))

    `(progn
       (defclass ,name (orm-table ,@mixins)
	 (,@(nreverse slots)

	  (table-name
	   :initform (quote ,table-name))
	  (columns
	   :initform (list ,@(nreverse columns)))
	  (associations
	   :initform (list ,@(nreverse assocs)))
	  (table-constraints
	   :initform (quote ,(cl-remove-if 'null (nreverse table-constraints)))))

	 ,@(orm--filter-options-and-doc options-and-doc)))))

(provide 'orm-macros)
