(require 'orm-table)

(setq orm-default-db (orm-db :type :sql :file "/tmp/test.db"))
(setq orm-default-conn (orm-connect orm-default-db))

(deftable author ()
          ((name :initarg :name
                 :primary-key t
                 :not-null t))
          :table authors)

(orm-create author)
(setq ayn-rand (author :name "Ayn Rand"))
(orm-insert ayn-rand)

(deftable book ()
          ((title :initarg :title
                  :primary-key t
                  :not-null t))
          :associations
          ((:belongs_to author :foreign-key writer)))

(orm-create book)
(setq fountain-head (book :title "Fountain Head" :writer "Ayn Rand"))
(orm-insert fountain-head)
