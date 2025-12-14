;;; orm-labeled-habtm-tests.el --- -*- lexical-binding: t; -*-
;;; ERT tests for has-and-belongs-to-many with edge labels

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
  (defmacro orm--define-labeled-habtm-test-schema ()
    `(progn
       (deftable tingredient ()
                 ((name :initarg :name :primary-key t :not-null t))
                 :table tingredients)
       (deftable trecipe ()
                 ((name :initarg :name :primary-key t :not-null t))
                 :table trecipes)
       ;; Return the generated join-table class symbol so tests can reference it.
       (defconst tingredients-trecipes-join
         (defassoc tingredient :has-and-belongs-to-many trecipe
                   :extra-columns ((quantity :initarg :quantity)
                                   (unit :initarg :unit)))))))

(orm--define-labeled-habtm-test-schema)

(defun orm--create-labeled-habtm-schema ()
  ;; Ensure clean slate: drop if present (ignore errors), then create.
  (ignore-errors (orm-drop tingredients-trecipes-join))
  (ignore-errors (orm-drop trecipe))
  (ignore-errors (orm-drop tingredient))
  (orm-create trecipe)
  (orm-create tingredient)
  (orm-create tingredients-trecipes-join))

;; --- Tests ------------------------------------------------------------------

(ert-deftest orm-labeled-habtm-create-and-introspection ()
  "Create tpart, tassembly, and HABTM join table; basic introspection."
  (let* ((db (orm--make-temp-db))
         (db-file (car db)))
    (unwind-protect
        (progn
          (orm--create-labeled-habtm-schema)

          ;; Sanity: tables exist
          (should (orm-created-p tingredient))
          (should (orm-created-p trecipe))
          (should (orm-created-p tingredients-trecipes-join))

          ;; Association metadata available from both sides
          (let ((ingredients->recipes (orm-assoc-get tingredient 'trecipes))
                (recipes->ingredients (orm-assoc-get trecipe 'tingredients)))
            (should ingredients->recipes)
            (should recipes->ingredients))

          ;; Check columns
          (let ((assoc-columns (orm-table-columns tingredients-trecipes-join)))
            (should (cl-member-if (lambda (x) (equal (slot-value x 'name) 'quantity)) assoc-columns))
            (should (cl-member-if (lambda (x) (equal (slot-value x 'name) 'unit)) assoc-columns))))
      (orm--teardown-db db-file))))

(ert-deftest orm-labeled-habtm-insert-and-read-via-assoc-api ()
  "Insert association via orm-assoc-insert; check present, first, all, find."
  (let* ((db (orm--make-temp-db))
         (db-file (car db)))
    (unwind-protect
        (progn
          (orm--create-labeled-habtm-schema)

          (let* ((banana-pudding (trecipe :name "Banana Pudding"))
                 (graham-crackers (tingredient :name "Graham Crackers")))
            (orm-insert banana-pudding)
            (orm-insert graham-crackers)

            ;; Create the association
            (orm-assoc-insert banana-pudding 'tingredients graham-crackers
                              :quantity 1 :unit "package")

            ;; Present both ways
            (should (orm-assoc-present-p graham-crackers 'trecipes banana-pudding))
            (should (orm-assoc-present-p banana-pudding 'tingredients graham-crackers))

            ;; First & All
            (let ((all-recipes (orm-assoc-all graham-crackers 'trecipes))
                  (all-ingredients (orm-assoc-all banana-pudding 'tingredients)))
              (should (equal (length all-recipes) 1))
              (should (equal (slot-value (caar all-recipes) 'name) "Banana Pudding"))
              (should (equal (plist-get (cdar all-recipes) :quantity) 1))
              (should (equal (plist-get (cdar all-recipes) :unit) "package"))
              (should (equal (length all-ingredients) 1))
              (should (equal (slot-value (caar all-ingredients) 'name) "Graham Crackers"))
              (should (equal (plist-get (cdar all-ingredients) :quantity) 1))
              (should (equal (plist-get (cdar all-ingredients) :unit) "package")))

            ;; Find by key (HABTM usually uses the other table's PK)
            (let ((found-recipe (orm-assoc-find graham-crackers 'trecipes "Banana Pudding"))
                  (found-ingredient (orm-assoc-find banana-pudding 'tingredients "Graham Crackers")))
              (should (and found-recipe (object-of-class-p found-recipe 'trecipe)))
              (should (and found-ingredient (object-of-class-p found-ingredient 'tingredient)))))
          ;; Idempotent insert shouldn’t duplicate rows
          (let* ((banana-pudding (orm-find trecipe "Banana Pudding"))
                 (graham-crackers (orm-find tingredient "Graham Crackers")))
            (orm-assoc-insert banana-pudding 'tingredients graham-crackers)
            (should (equal (length (orm-assoc-all banana-pudding 'tingredients)) 1))
            (should (equal (length (orm-assoc-all graham-crackers 'trecipes)) 1))))
      (orm--teardown-db db-file))))

(provide 'orm-labeled-habtm-tests)
;;; orm-labeled-habtm-tests.el ends here
