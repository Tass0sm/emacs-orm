(require 'orm)

(setq orm-default-db (orm-db :type :sql :file "/tmp/test.db"))
(setq orm-default-conn (orm-connect orm-default-db))

(deftable tperson ()
          ((name :initarg :name :primary-key t :not-null t))
          :table tpersons)

(deftable tidcard ()
          ((idnum :initarg :idnum :primary-key t :not-null t))
          :table tidcards)

(defconst tperson-has-one-tidcard
  (defassoc tperson :has-one tidcard))

(orm-create tperson)
(orm-create tidcard)

(setq p  (tperson :name "Grace"))
(setq c1 (tidcard :idnum "ID-001"))
(setq c2 (tidcard :idnum "ID-002"))

(orm-insert p)
(orm-insert c1)
(orm-insert c2)

;; insert

(orm-assoc-insert p 'tidcard c2)

;; delete

(orm-assoc-delete p 'tidcard c2)
