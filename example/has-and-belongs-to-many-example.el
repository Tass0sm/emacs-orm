(require 'orm)

(setq orm-default-db (orm-db :type :sql :file "/tmp/test.db"))
(setq orm-default-conn (orm-connect orm-default-db))

(deftable part ()
	  ((name :initarg :name
		 :primary-key t
		 :not-null t))
	  :table parts)

(deftable assembly ()
	  ((name :initarg :name
		 :primary-key t
		 :not-null t))
	  :table assemblies)

(setq join-table (defassoc part :has-and-belongs-to-many assembly))

(if (orm-created-p part)
    (message "part already created")
  (orm-create part))

(if (orm-created-p assembly)
    (message "assembly already created")
  (orm-create assembly))

(if (orm-created-p join-table)
    (message "join-table already created")
  (orm-create join-table))

(setq bolt (part :name "bolt"))
(setq box (assembly :name "box"))

(orm-insert bolt)
(orm-insert box)

;; insert

(orm-assoc-insert bolt 'assemblies box)
;; OR
(push box (slot-value bolt 'assemblies))
(push bolt (slot-value box 'parts))
(orm-update box)

;; all

(orm-assoc-all bolt 'assemblies)
(orm-assoc-all box 'parts)
(orm-assoc-find bolt 'assemblies "box")

;; delete

(orm-assoc-delete bolt 'assemblies box)
;; OR
(setf (slot-value bolt 'assemblies) nil)
(setf (slot-value box 'parts) nil)
(orm-update box)
