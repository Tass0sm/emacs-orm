;;; orm-hasone-tests.el --- ERT tests for has-one  -*- lexical-binding: t; -*-

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

;; Test-only tables (unique names)
(eval-and-compile
  (defmacro orm--define-hasone-test-schema ()
    `(progn
       (deftable tperson ()
         ((name :initarg :name :primary-key t :not-null t))
         :table tpersons)
       (deftable tidcard ()
         ((idnum :initarg :idnum :primary-key t :not-null t))
         :table tidcards)
       (defconst tperson-has-one-tidcard
         (defassoc tperson :has-one tidcard)))))

(orm--define-hasone-test-schema)

(defun orm--create-hasone-schema ()
  ;; Fresh schema
  (ignore-errors (orm-drop tidcard))
  (ignore-errors (orm-drop tperson))
  (orm-create tperson)
  (orm-create tidcard))

;; --- Tests ------------------------------------------------------------------

(ert-deftest orm-hasone-create-and-introspection ()
  "Create tables; association metadata visible from both sides."
  (let* ((db (orm--make-temp-db))
         (db-file (car db)))
    (unwind-protect
        (progn
          (orm--create-hasone-schema)
          (should (orm-created-p tperson))
          (should (orm-created-p tidcard))
          (let ((person->idcard (orm-assoc-get tperson 'tidcard))
                (idcard->person (orm-assoc-get tidcard 'tperson)))
            (should person->idcard)
            (should idcard->person)))
      (orm--teardown-db db-file))))

(ert-deftest orm-hasone-insert-and-read-via-assoc-api ()
  "Attach exactly one tidcard to a tperson; verify present/first/all/find."
  (let* ((db (orm--make-temp-db))
         (db-file (car db)))
    (unwind-protect
        (progn
          (orm--create-hasone-schema)

          (let* ((p (tperson :name "Ada"))
                 (c (tidcard :idnum "ID-001")))
            (orm-insert p)
            (orm-insert c)

            ;; Initially none
            (should (null (orm-assoc-first p 'tidcard)))
            (should (null (orm-assoc-first c 'tperson)))
            (should (equal (length (or (orm-assoc-all p 'tidcard) '())) 0))

            ;; Link via API
            (orm-assoc-insert p 'tidcard c)
            (should (orm-assoc-present-p p 'tidcard c))
            (should (orm-assoc-present-p c 'tperson p))

            ;; first/all/find
            (let ((f (orm-assoc-first p 'tidcard))
                  (all (or (orm-assoc-all p 'tidcard) '())))
              (should (and f (object-of-class-p f 'tidcard)))
              ;; all should behave like 0/1 elements for has-one
              (should (equal (length all) 1))
              (should (equal (slot-value (car all) 'idnum) "ID-001")))

            (let ((found-c (orm-assoc-find p 'tidcard "ID-001"))
                  (found-p (orm-assoc-find c 'tperson "Ada")))
              (should (and found-c (object-of-class-p found-c 'tidcard)))
              (should (and found-p (object-of-class-p found-p 'tperson))))))

      (orm--teardown-db db-file))))

(ert-deftest orm-hasone-replace-existing-child-with-insert ()
  "Inserting another child for the same parent should replace the association.
Old child either gets FK cleared or is deleted (both acceptable)."
  (let* ((db (orm--make-temp-db))
         (db-file (car db)))
    (unwind-protect
        (progn
          (orm--create-hasone-schema)
          (let* ((p  (tperson :name "Grace"))
                 (c1 (tidcard :idnum "ID-001"))
                 (c2 (tidcard :idnum "ID-002")))
            (orm-insert p)
            (orm-insert c1)
            (orm-insert c2)

            (orm-assoc-insert p 'tidcard c1)
            (should (orm-assoc-present-p p 'tidcard c1))

            ;; Insert new card for same person (no orm-assoc-update available)
            (orm-assoc-insert p 'tidcard c2)

            ;; Now parent should point to c2
            (let ((cur (orm-assoc-first p 'tidcard)))
              (should (equal (slot-value cur 'idnum) "ID-002")))

            ;; c1 should no longer claim p: either deleted or disassociated
            (let ((c1-row (orm-find tidcard "ID-001")))
              (if (null c1-row)
                  (should (null c1-row)) ;; cascade-deleted policy
                (should (null (orm-assoc-first c1-row 'tperson)))))))
      (orm--teardown-db db-file))))

(ert-deftest orm-hasone-slot-mutation-and-update ()
  "Create/clear has-one by mutating slots and calling orm-update."
  (let* ((db (orm--make-temp-db))
         (db-file (car db)))
    (unwind-protect
        (progn
          (orm--create-hasone-schema)
          (let* ((p (tperson :name "Linus"))
                 (c (tidcard :idnum "ID-XYZ")))
            (orm-insert p)
            (orm-insert c)

            ;; Link by slot mutation on both sides, persist via child update
            (setf (slot-value p 'tidcard) c)
            (setf (slot-value c 'tperson) p)
            (orm-update c)

            (should (orm-assoc-present-p p 'tidcard c))
            (should (orm-assoc-present-p c 'tperson p))

            ;; Clear association via API (preferred to ensure FK handling)
            (orm-assoc-delete p 'tidcard c)
            (should (null (orm-assoc-first p 'tidcard)))
            (should (null (orm-assoc-first c 'tperson)))
            (should (orm-present-p p))
            (should (orm-present-p c))))
      (orm--teardown-db db-file))))

(ert-deftest orm-hasone-delete-parent-cleans-up ()
  "Deleting parent should not leave a dangling association.
Accept either cascade-delete (child removed) or FK-null (child remains, no backref)."
  (let* ((db (orm--make-temp-db))
         (db-file (car db)))
    (unwind-protect
        (progn
          (orm--create-hasone-schema)
          (let* ((p (tperson :name "Toni"))
                 (c (tidcard :idnum "ID-777")))
            (orm-insert p)
            (orm-insert c)
            (orm-assoc-insert p 'tidcard c)

            (orm-delete p)

            (let* ((c-row (orm-find tidcard "ID-777"))
                   (back (and c-row (orm-assoc-first c-row 'tperson))))
              (should (or (null c-row)                 ;; cascade-deleted
                          (and c-row (null back)))))))  ;; FK set to NULL
      (orm--teardown-db db-file))))

(ert-deftest orm-hasone-delete-child-clears-parent ()
  "Deleting the child should clear the parent's has-one slot/backref."
  (let* ((db (orm--make-temp-db))
         (db-file (car db)))
    (unwind-protect
        (progn
          (orm--create-hasone-schema)
          (let* ((p (tperson :name "Nadia"))
                 (c (tidcard :idnum "ID-909")))
            (orm-insert p)
            (orm-insert c)
            (orm-assoc-insert p 'tidcard c)

            (orm-delete c)

            ;; Parent should not still point to a non-existent child
            (let ((cur (orm-assoc-first p 'tidcard)))
              (should (null cur)))))
      (orm--teardown-db db-file))))

(provide 'orm-has-one-tests)
;;; orm-has-one-tests.el ends here
