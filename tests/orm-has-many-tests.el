;;; orm-hasmany-tests.el --- ERT tests for has-many  -*- lexical-binding: t; -*-

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

;; Define test-only tables (unique names to avoid collisions).
(eval-and-compile
  (defmacro orm--define-hasmany-test-schema ()
    `(progn
       (deftable tauthor ()
         ((name :initarg :name :primary-key t :not-null t))
         :table tauthors)
       (deftable tbook ()
         ((isbn :initarg :isbn :primary-key t :not-null t)
          ;; Many books per author (FK stored on tbook).
          ;; (FK will be added by defassoc)
          )
         :table tbooks)
       (defconst tauthor-has-many-tbook
         (defassoc tauthor :has-many tbook)))))

(orm--define-hasmany-test-schema)

(defun orm--create-hasmany-schema ()
  ;; Ensure clean slate (ignore if not present), then create.
  (ignore-errors (orm-drop tbook))
  (ignore-errors (orm-drop tauthor))
  (orm-create tauthor)
  (orm-create tbook))

;; --- Tests ------------------------------------------------------------------

(ert-deftest orm-hasmany-create-and-introspection ()
  "Create tauthor/tbook; association metadata visible from both sides."
  (let* ((db (orm--make-temp-db))
         (db-file (car db)))
    (unwind-protect
        (progn
          (orm--create-hasmany-schema)
          (should (orm-created-p tauthor))
          (should (orm-created-p tbook))

          ;; Association metadata
          (let ((author->books (orm-assoc-get tauthor 'tbooks)))
            (should author->books)))
      (orm--teardown-db db-file))))

(ert-deftest orm-hasmany-insert-and-read-via-assoc-api ()
  "Attach books to an author via orm-assoc-*; verify present/first/all/find."
  (let* ((db (orm--make-temp-db))
         (db-file (car db)))
    (unwind-protect
        (progn
          (orm--create-hasmany-schema)

          (let* ((a (tauthor :name "Ursula K. Le Guin"))
                 (b1 (tbook :isbn "978-0-553-34624-6")) ;; Earthsea (example)
                 (b2 (tbook :isbn "978-0-441-78358-8"))) ;; The Dispossessed
            (orm-insert a)
            (orm-insert b1)
            (orm-insert b2)

            ;; Initially no books linked
            (should (equal (length (orm-assoc-all a 'tbooks)) 0))

            ;; Link b1 and b2 to a
            (orm-assoc-insert a 'tbooks b1)
            (orm-assoc-insert a 'tbooks b2)

            ;; present-p from both directions
            (should (orm-assoc-present-p a  'tbooks  b1))
            (should (orm-assoc-present-p a  'tbooks  b2))
            (should (orm-assoc-present-p b1 'tauthor a))
            (should (orm-assoc-present-p b2 'tauthor a))

            ;; all
            (let ((all-books  (orm-assoc-all a 'tbooks)))
              (should (equal (length all-books) 2)))

            ;; find
            (let ((found-b1 (orm-assoc-find a 'tbooks "978-0-553-34624-6"))
                  (found-a  (orm-assoc-find b2 'tauthor "Ursula K. Le Guin")))
              (should (and found-b1 (object-of-class-p found-b1 'tbook)))
              (should (and found-a (object-of-class-p found-a 'tauthor))))

            ;; Idempotent update shouldn’t duplicate rows
            (orm-assoc-insert a 'tbooks b1)
            (should (equal (length (orm-assoc-all a 'tbooks)) 2))))
      (orm--teardown-db db-file))))

(ert-deftest orm-hasmany-insert-via-slot-mutation-and-update ()
  "Create/remove links by mutating slots then orm-update."
  (let* ((db (orm--make-temp-db))
         (db-file (car db)))
    (unwind-protect
        (progn
          (orm--create-hasmany-schema)

          (let* ((a (tauthor :name "Octavia E. Butler"))
                 (b (tbook :isbn "978-0-446-67554-5")))
            (orm-insert a)
            (orm-insert b)

            ;; Add by mutating both sides (if both sides exist) then update one.
            ;; Parent side list:
            (push b (slot-value a 'tbooks))
            ;; Child backref (singular):
            (setf (slot-value b 'tauthor) a)
            (orm-update b)

            (should (orm-assoc-present-p a 'tbooks b))
            (should (equal (length (orm-assoc-all a 'tbooks)) 1))

            ;; Remove association:
            ;; Prefer API so FK becomes NULL (child remains).
            (orm-assoc-delete a 'tbooks b)
            (should (equal (length (orm-assoc-all a 'tbooks)) 0))
            (should (orm-present-p b)) ;; child row still exists
            ;; And its backref should be cleared
            (should (null (orm-assoc-all b 'tauthor))))))
      (orm--teardown-db db-file)))

(ert-deftest orm-hasmany-reassign-child-to-different-parent ()
  "Move a child from one parent to another via assoc-update."
  (let* ((db (orm--make-temp-db))
         (db-file (car db)))
    (unwind-protect
        (progn
          (orm--create-hasmany-schema)

          (let* ((a1 (tauthor :name "Author A"))
                 (a2 (tauthor :name "Author B"))
                 (b  (tbook :isbn "111-1-11-111111-1")))
            (orm-insert a1)
            (orm-insert a2)
            (orm-insert b)

            (orm-assoc-insert a1 'tbooks b)
            (should (orm-assoc-present-p a1 'tbooks b))

            ;; Reassign to a2
            (orm-assoc-insert a2 'tbooks b)

            (should (not (orm-assoc-present-p a1 'tbooks b)))
            (should (orm-assoc-present-p a2 'tbooks b))
            (let ((parent (orm-assoc-first b 'tauthor)))
              (should (and parent (equal (slot-value parent 'name) "Author B"))))))
      (orm--teardown-db db-file))))

(ert-deftest orm-hasmany-delete-parent-and-cascade-cleanup ()
  "Deleting parent should clean up associations; policy: children either cascade-delete or get FK cleared.
We assert no dangling association remains."
  (let* ((db (orm--make-temp-db))
         (db-file (car db)))
    (unwind-protect
        (progn
          (orm--create-hasmany-schema)

          (let* ((a (tauthor :name "Terry Pratchett"))
                 (b1 (tbook :isbn "978-0-06-222571-9"))
                 (b2 (tbook :isbn "978-0-06-223737-8")))
            (orm-insert a)
            (orm-insert b1)
            (orm-insert b2)
            (orm-assoc-insert a 'tbooks b1)
            (orm-assoc-insert a 'tbooks b2)
            (should (equal (length (orm-assoc-all a 'tbooks)) 2))

            ;; Delete the parent
            (orm-delete a)

            ;; No book should still claim 'a' as parent.
            (let* ((b1-row (orm-find tbook "978-0-06-222571-9"))
                   (b2-row (orm-find tbook "978-0-06-223737-8"))
                   (b1-parent (and b1-row (orm-assoc-first b1-row 'tauthor)))
                   (b2-parent (and b2-row (orm-assoc-first b2-row 'tauthor))))
              ;; Two acceptable policies:
              ;;  (1) CASCADE DELETE: b1-row/b2-row are gone (nil)
              ;;  (2) FK NULL: rows exist, backref is nil
              (should (or (and (null b1-row) (null b2-row)) ; cascade delete
                          (and b1-row b2-row (null b1-parent) (null b2-parent))))))
      (orm--teardown-db db-file)))))

(provide 'orm-has-many-tests)
;;; orm-hasmany-tests.el ends here
