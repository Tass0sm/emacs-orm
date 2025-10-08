(require 'orm)

(setq orm-default-db (orm-db :type :sql :file "/tmp/test.db"))
(setq orm-default-conn (orm-connect orm-default-db))

(deftable user ()
	  ((name :initarg :name
		 :primary-key t
		 :not-null t))
	  :table users)

(deftable profile ()
	  ((alias :initarg :alias
		  :primary-key t
		  :not-null t))
	  :associations
	  ((:belongs-to user :foreign-key user)))

(defassoc user :has-one profile)

;; Create

(orm-create user)
(setq tassos (user :name "Tassos"))
(orm-insert tassos)

(orm-create profile)
(setq tassos-profile (profile :alias "Tass0sm"))
(orm-insert tassos-profile)

;; check association

(orm-assoc-get profile 'user)
(orm-assoc-get user 'profile)

;; Associate

(orm-assoc-insert tassos 'profile tassos-profile)

;; Read
