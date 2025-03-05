;;; orm-habtm-tests.el --- ERT tests for has-and-belongs-to-many  -*- lexical-binding: t; -*-

(require 'ert)
(require 'orm)

;; --- Fixtures ---------------------------------------------------------------

(defun orm--make-temp-db ()
  "Create and connect to a fresh temp DB; return (DB-FILE . CONN)."
  (let* ((db-file (make-temp-file "orm-test-" nil ".db"))
         (db (orm-db :type :sql :file db-file))
         (conn (orm-connect db)))
    (setq orm-default-db db)
    (setq orm-default-conn conn)
    (cons db-file conn)))

(defun orm--teardown-db (db-file)
  "Disconnect and delete DB-FILE if it exists."
  (when (boundp 'orm-default-conn)
    (ignore-errors (orm-disconnect orm-default-conn)))
  (when (and db-file (file-exists-p db-file))
    (ignore-errors (delete-file db-file))))

;; Define test-only tables with unique names to avoid class clashes.
;; Using minimal schemas so HABTM focuses on the join behavior.

(eval-and-compile
  (defmacro orm--define-habtm-test-schema ()
    `(progn
       (deftable tpart ()
         ((name :initarg :name :primary-key t :not-null t))
         :table tparts)
       (deftable tassembly ()
         ((name :initarg :name :primary-key t :not-null t))
         :table tassemblies)
       ;; Return the generated join-table class symbol so tests can reference it.
       (defconst tparts-tassemblies-join
         (defassoc tpart :has-and-belongs-to-many tassembly)))))

(orm--define-habtm-test-schema)

(defun orm--create-habtm-schema ()
  ;; Ensure clean slate: drop if present (ignore errors), then create.
  (ignore-errors (orm-drop tparts-tassemblies-join))
  (ignore-errors (orm-drop tassembly))
  (ignore-errors (orm-drop tpart))
  (orm-create tpart)
  (orm-create tassembly)
  (orm-create tparts-tassemblies-join))

;; --- Tests ------------------------------------------------------------------

(ert-deftest orm-habtm-create-and-introspection ()
  "Create tpart, tassembly, and HABTM join table; basic introspection."
  (let* ((db (orm--make-temp-db))
         (db-file (car db)))
    (unwind-protect
        (progn
          (orm--create-habtm-schema)

          ;; Sanity: tables exist
          (should (orm-created-p tpart))
          (should (orm-created-p tassembly))
          (should (orm-created-p tparts-tassemblies-join))

          ;; Association metadata available from both sides
          (let ((part->assemblies (orm-assoc-get tpart 'tassemblies))
                (assembly->parts (orm-assoc-get tassembly 'tparts)))
            (should part->assemblies)
            (should assembly->parts)))
      (orm--teardown-db db-file))))

(ert-deftest orm-habtm-insert-and-read-via-assoc-api ()
  "Insert association via orm-assoc-insert; check present, first, all, find."
  (let* ((db (orm--make-temp-db))
         (db-file (car db)))
    (unwind-protect
        (progn
          (orm--create-habtm-schema)

          (let* ((bolt (tpart :name "bolt"))
                 (box  (tassembly :name "box")))
            (orm-insert bolt)
            (orm-insert box)

            ;; Initially empty
            (should (equal (length (orm-assoc-all bolt 'tassemblies)) 0))
            (should (equal (length (orm-assoc-all box 'tparts)) 0))

            ;; Create the association
            (orm-assoc-insert bolt 'tassemblies box)

            ;; Present both ways
            (should (orm-assoc-present-p bolt 'tassemblies box))
            (should (orm-assoc-present-p box  'tparts       bolt))

            ;; First & All
            (let ((all-asm   (orm-assoc-all bolt 'tassemblies))
                  (all-parts (orm-assoc-all box 'tparts)))
              (should (equal (length all-asm) 1))
              (should (equal (slot-value (car all-asm) 'name) "box"))
              (should (equal (length all-parts) 1))
              (should (equal (slot-value (car all-parts) 'name) "bolt")))

            ;; Find by key (HABTM usually uses the other table's PK)
            (let ((found-asm (orm-assoc-find bolt 'tassemblies "box"))
                  (found-part (orm-assoc-find box 'tparts "bolt")))
              (should (and found-asm (object-of-class-p found-asm 'tassembly)))
              (should (and found-part (object-of-class-p found-part 'tpart)))))

          ;; Idempotent insert shouldn’t duplicate rows
          (let* ((bolt (orm-find tpart "bolt"))
                 (box  (orm-find tassembly "box")))
            (orm-assoc-insert bolt 'tassemblies box)
            (should (equal (length (orm-assoc-all bolt 'tassemblies)) 1))
            (should (equal (length (orm-assoc-all box 'tparts)) 1))))
      (orm--teardown-db db-file))))

(ert-deftest orm-habtm-insert-via-slot-mutation-and-update ()
  "Create/delete association by mutating slots and calling orm-update."
  (let* ((db (orm--make-temp-db))
         (db-file (car db)))
    (unwind-protect
        (progn
          (orm--create-habtm-schema)

          (let* ((screw (tpart :name "screw"))
                 (crate (tassembly :name "crate")))
            (orm-insert screw)
            (orm-insert crate)

            ;; Add by mutating both sides then updating one side
            (push crate (slot-value screw 'tassemblies))
            (push screw (slot-value crate 'tparts))
            (orm-update crate)

            (should (orm-assoc-present-p screw 'tassemblies crate))
            (should (equal (length (orm-assoc-all screw 'tassemblies)) 1))

            ;; Remove by clearing slots then updating
            (setf (slot-value screw 'tassemblies) nil)
            (setf (slot-value crate 'tparts) nil)
            (orm-update crate)

            (should (equal (length (orm-assoc-all screw 'tassemblies)) 0))
            (should (equal (length (orm-assoc-all crate 'tparts)) 0))))
      (orm--teardown-db db-file))))

(ert-deftest orm-habtm-delete-and-cascade-cleanup ()
  "Delete association directly and ensure object deletion cleans join rows."
  (let* ((db (orm--make-temp-db))
         (db-file (car db)))
    (unwind-protect
        (progn
          (orm--create-habtm-schema)

          (let* ((bolt (tpart :name "bolt"))
                 (box  (tassembly :name "box"))
                 (lid  (tassembly :name "lid")))
            (orm-insert bolt)
            (orm-insert box)
            (orm-insert lid)

            ;; bolt <-> box, bolt <-> lid
            (orm-assoc-insert bolt 'tassemblies box)
            (orm-assoc-insert bolt 'tassemblies lid)
            (should (equal (length (orm-assoc-all bolt 'tassemblies)) 2))

            ;; Delete single link
            (orm-assoc-delete bolt 'tassemblies box)
            (should (not (orm-assoc-present-p bolt 'tassemblies box)))
            (should (orm-assoc-present-p bolt 'tassemblies lid))
            (should (equal (length (orm-assoc-all bolt 'tassemblies)) 1))

            ;; Delete object 'lid' and ensure join rows are cleaned,
            ;; but 'bolt' still exists.
            (orm-delete lid)
            (should (equal (length (orm-assoc-all bolt 'tassemblies)) 0))
            (should (orm-present-p bolt))
            (should (orm-present-p box))))
      (orm--teardown-db db-file))))

(provide 'orm-habtm-tests)
;;; orm-habtm-tests.el ends here
