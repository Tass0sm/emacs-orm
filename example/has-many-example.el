(require 'orm)

(setq orm-default-db (orm-db :type :sql :file "/tmp/test.db"))
(setq orm-default-conn (orm-connect orm-default-db))

(deftable homework ()
	  ((number :initarg :number
		   :primary-key t
		   :not-null t))
	  :table homeworks)

(deftable problem ()
	  ((text :initarg :text
		 :primary-key t
		 :not-null t))
          :table problems
	  :associations
	  ((:belongs-to homework :foreign-key homework)))

(defassoc homework :has-many problem)

;; Create

(orm-create homework)
(setq hw1 (homework :number 1))
(orm-insert hw1)

(orm-create problem)
(setq prob1 (problem :text "Problem 1"))
(orm-insert prob1)

;; check association

(orm-assoc-get hw1 'problems)
(orm-assoc-get prob1 'homework)

;; Associate

(orm-assoc-insert hw1 'problems prob1)

;; Read

(orm-assoc-all hw1 'problems)
