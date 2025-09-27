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


;; (orm-get part assemblies)
;; (setq part-assemblies (car (orm-table-associations assembly)))

(setq bolt (part :name "bolt" :assemblies (list "box")))
(setq box (assembly :name "box"))

(orm-assoc-get bolt 'assemblies)



;; (orm-all (orm-get-assoc bolt 'assemblies))
;; (orm-append (orm-get-assoc bolt 'assemblies) box)

;; (orm-append (orm-get-assoc bolt 'assemblies) "box")


;; (orm-all (orm-get-assoc bolt 'assemblies))
;; ;; (orm-all (orm-get part-assemblies)
