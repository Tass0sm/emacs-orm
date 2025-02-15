(require 'orm-table)

(setq orm-default-db (orm-db :type :sql :file "/tmp/test.db"))
(setq orm-default-conn (orm-connect orm-default-db))

(deftable author ()
	  ((name :initarg :name
		 :primary-key t
		 :not-null t))
	  :table authors)

(orm-drop author)

(orm-create author)
(setq ayn-rand (author :name "Ayn Rand"))
(orm-insert ayn-rand)

(setq franz-kafka (author :name "Franz Kafka"))
(orm-insert franz-kafka)

(deftable book ()
	  ((title :initarg :title
		  :primary-key t
		  :not-null t)
	   (year :initarg :year))
	  :associations
	  ((:belongs_to author :foreign-key writer)))

;; Create

(orm-create book)
(setq fountain-head (book :title "Fountain Head" :writer "Ayn Rand" :year 2000))
(orm-insert fountain-head)

;; Read

(orm-all author)
(orm-first author)

;; Update

(setf (oref fountain-head :year) 2002)
(orm-update fountain-head)

;; Delete

(orm-delete fountain-head)
