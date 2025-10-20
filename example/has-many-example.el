(require 'orm)

(setq orm-default-db (orm-db :type :sql :file "/tmp/test.db"))
(setq orm-default-conn (orm-connect orm-default-db))

(deftable tauthor ()
          ((name :initarg :name :primary-key t :not-null t))
          :table tauthors)

(deftable tbook ()
          ((isbn :initarg :isbn :primary-key t :not-null t))
          :table tbooks)

(defconst tauthor-has-many-tbook
  (defassoc tauthor :has-many tbook))

(orm-drop tauthor)
(orm-drop tbook)


(orm-create tauthor)
(orm-create tbook)

(setq a (tauthor :name "Ursula K. Le Guin"))
(setq b1 (tbook :isbn "978-0-553-34624-6"))
(setq b2 (tbook :isbn "978-0-441-78358-8"))

(orm-insert a)
(orm-insert b1)
(orm-insert b2)

;; Link b1 and b2 to a
(orm-assoc-insert a 'tbooks b1)
;; OR
(push b1 (slot-value a 'tbooks))
(setf (slot-value b1 'tauthor) a)
(orm-update b1)
