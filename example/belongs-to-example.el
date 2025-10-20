(require 'orm)

(setq orm-default-db (orm-db :type :sql :file "/tmp/test.db"))
(setq orm-default-conn (orm-connect orm-default-db))

(deftable towner ()
         ((name :initarg :name :primary-key t :not-null t))
         :table towners)

(deftable tpet ()
          ((tag :initarg :tag :primary-key t :not-null t))
          :table tpets)

(defconst tpet-belongs-to-towner
  (defassoc tpet :belongs-to towner))

(orm-create towner)
(orm-create tpet)

(setq o (towner :name "Cleo"))
(setq p (tpet   :tag  "P-303"))

(orm-insert o)
(orm-insert p)

(setf (slot-value p 'towner) o)
(orm-update p)

(orm-assoc-present-p p 'towner o)

(orm-assoc-delete p 'towner o)
