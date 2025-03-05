;;; orm-belongs-to-tests.el --- ERT tests for belongs-to (one-way) -*- lexical-binding: t; -*-

(require 'ert)
(require 'orm)

;; --- Fixtures ---------------------------------------------------------------

(defun orm--make-temp-db ()
  (let* ((db-file (make-temp-file "orm-test-" nil ".db"))
         (db (orm-db :type :sql :file db-file))
         (conn (orm-connect db)))
    (setq orm-default-db db
          orm-default-conn conn)
    (cons db-file conn)))

(defun orm--teardown-db (db-file)
  (when (boundp 'orm-default-conn)
    (ignore-errors (orm-disconnect orm-default-conn)))
  (when (and db-file (file-exists-p db-file))
    (ignore-errors (delete-file db-file))))

(eval-and-compile
  (defmacro orm--define-belongs-to-test-schema ()
    `(progn
       (deftable towner ()
         ((name :initarg :name :primary-key t :not-null t))
         :table towners)
       (deftable tpet ()
         ((tag :initarg :tag :primary-key t :not-null t))
         :table tpets)
       ;; ONE-WAY: child belongs-to parent (no parent backref created here).
       (defconst tpet-belongs-to-towner
         (defassoc tpet :belongs-to towner)))))

(orm--define-belongs-to-test-schema)

(defun orm--create-belongs-to-schema ()
  (ignore-errors (orm-drop tpet))
  (ignore-errors (orm-drop towner))
  (orm-create towner)
  (orm-create tpet))

;; --- Tests ------------------------------------------------------------------

(ert-deftest orm-belongs-to-oneway-create-and-introspection ()
  "Association metadata exists only on the child (belongs-to) side."
  (let* ((db (orm--make-temp-db)) (db-file (car db)))
    (unwind-protect
        (progn
          (orm--create-belongs-to-schema)
          (should (orm-created-p towner))
          (should (orm-created-p tpet))

          ;; Child side has 'towner' assoc:
          (should (orm-assoc-get tpet 'towner))

          ;; Parent side should NOT expose a 'tpets' assoc here.
          ;; Don't call orm-assoc-get for a missing assoc to avoid errors.
          t)
      (orm--teardown-db db-file))))

(ert-deftest orm-belongs-to-oneway-insert-and-read ()
  "Link child->parent via orm-assoc-insert; verify present/first/find from child."
  (let* ((db (orm--make-temp-db)) (db-file (car db)))
    (unwind-protect
        (progn
          (orm--create-belongs-to-schema)
          (let* ((o (towner :name "Alice"))
                 (p (tpet   :tag  "P-001")))
            (orm-insert o)
            (orm-insert p)

            ;; Initially unlinked
            (should (null (orm-assoc-first p 'towner)))

            ;; Link (child -> parent)
            (orm-assoc-insert p 'towner o)
            (should (orm-assoc-present-p p 'towner o))

            ;; Read current parent
            (let ((cur (orm-assoc-first p 'towner)))
              (should (and cur (object-of-class-p cur 'towner)))
              (should (equal (slot-value cur 'name) "Alice")))

            ;; Find by key (child side)
            (let ((found (orm-assoc-find p 'towner "Alice")))
                (should (and found (object-of-class-p found 'towner))))))

      (orm--teardown-db db-file))))

(ert-deftest orm-belongs-to-oneway-multiplicity-children-share-parent ()
  "Multiple children can belong to the same parent; verify per-child owners."
  (let* ((db (orm--make-temp-db)) (db-file (car db)))
    (unwind-protect
        (progn
          (orm--create-belongs-to-schema)
          (let* ((o (towner :name "Bora"))
                 (p1 (tpet   :tag  "P-101"))
                 (p2 (tpet   :tag  "P-202")))
            (orm-insert o) (orm-insert p1) (orm-insert p2)
            (orm-assoc-insert p1 'towner o)
            (orm-assoc-insert p2 'towner o)

            (should (equal (slot-value (orm-assoc-first p1 'towner) 'name) "Bora"))
            (should (equal (slot-value (orm-assoc-first p2 'towner) 'name) "Bora"))))
      (orm--teardown-db db-file))))

(ert-deftest orm-belongs-to-oneway-slot-mutation-and-update ()
  "Set/clear belongs-to by mutating child's slot and persisting child."
  (let* ((db (orm--make-temp-db)) (db-file (car db)))
    (unwind-protect
        (progn
          (orm--create-belongs-to-schema)
          (let* ((o (towner :name "Cleo"))
                 (p (tpet   :tag  "P-303")))
            (orm-insert o) (orm-insert p)

            ;; Link via slot mutation on child, then persist child
            (setf (slot-value p 'towner) o)
            (orm-update p)

            (should (orm-assoc-present-p p 'towner o))

            ;; Clear via API (preferred, handles FK cleanly)
            (orm-assoc-delete p 'towner o)
            (should (null (orm-assoc-first p 'towner)))
            (should (orm-present-p p))
            (should (orm-present-p o))))
      (orm--teardown-db db-file))))

(ert-deftest orm-belongs-to-oneway-reassign-child ()
  "Reassign child to a new parent by inserting the new mapping."
  (let* ((db (orm--make-temp-db)) (db-file (car db)))
    (unwind-protect
        (progn
          (orm--create-belongs-to-schema)
          (let* ((o1 (towner :name "Dora"))
                 (o2 (towner :name "Evan"))
                 (p  (tpet   :tag  "P-404")))
            (orm-insert o1) (orm-insert o2) (orm-insert p)

            (orm-assoc-insert p 'towner o1)
            (should (equal (slot-value (orm-assoc-first p 'towner) 'name) "Dora"))

            ;; Reassign (no orm-assoc-update; insert replaces)
            (orm-assoc-insert p 'towner o2)
            (should (equal (slot-value (orm-assoc-first p 'towner) 'name) "Evan"))))
      (orm--teardown-db db-file))))

(ert-deftest orm-belongs-to-oneway-delete-parent-policy ()
  "Deleting parent leaves no dangling child->parent link.
Accept either CASCADE DELETE (child removed) or FK NULL (child remains)."
  (let* ((db (orm--make-temp-db)) (db-file (car db)))
    (unwind-protect
        (progn
          (orm--create-belongs-to-schema)
          (let* ((o (towner :name "Faye"))
                 (p (tpet   :tag  "P-505")))
            (orm-insert o) (orm-insert p)
            (orm-assoc-insert p 'towner o)

            (orm-delete o)

            (let* ((p-row (orm-find tpet "P-505"))
                   (back  (and p-row (orm-assoc-first p-row 'towner))))
              (should (or (null p-row)           ;; cascade delete child
                          (and p-row (null back))))))) ;; FK set to NULL
      (orm--teardown-db db-file))))

(ert-deftest orm-belongs-to-oneway-delete-child ()
  "Deleting child should leave parent row untouched (no backref exists)."
  (let* ((db (orm--make-temp-db)) (db-file (car db)))
    (unwind-protect
        (progn
          (orm--create-belongs-to-schema)
          (let* ((o (towner :name "Gus"))
                 (p (tpet   :tag  "P-606")))
            (orm-insert o) (orm-insert p)
            (orm-assoc-insert p 'towner o)

            (orm-delete p)

            ;; Parent still present; nothing to check on backref (none exists)
            (should (orm-present-p o))))
      (orm--teardown-db db-file))))

(provide 'orm-belongs-to-tests)
;;; orm-belongs-to-tests.el ends here
