(require 'orm)

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

(orm-assoc-get bolt 'assemblies)
(orm-assoc-get box 'parts)

;; (orm-assoc-insert bolt 'assemblies box)
;; OR
(push box (slot-value bolt 'assemblies))
(orm-update box)

(orm-assoc-all bolt 'assemblies)



(orm-assoc-all bolt 'assemblies)
(orm-assoc-all box 'parts)

(orm-assoc-update

;; (orm-append (orm-get-assoc bolt 'assemblies) box)

;; (orm-append (orm-get-assoc bolt 'assemblies) "box")


;; (orm-all (orm-get-assoc bolt 'assemblies))
;; ;; (orm-all (orm-get part-assemblies)
