(require 'ert)
(require 'orm)

(deftable author ()
	  ((name :initarg :name
		 :primary-key t
		 :not-null t))
	  :table authors)

(deftable book ()
	  ((title :initarg :title
		  :primary-key t
		  :not-null t)
	   (year :initarg :year))
	  :associations
	  ((:belongs_to author :foreign-key writer)))

(ert-deftest orm-crud-features ()
  "Tests orm-create"
  (skip-unless (not (file-exists-p "/tmp/test.db")))

  (let ((orm-default-db (orm-db :type :sql :file "/tmp/test.db"))
	(orm-default-conn (orm-connect orm-default-db))
	(ayn-rand (author :name "Ayn Rand"))
	(franz-kafka (author :name "Franz Kafka")))
    (orm-create author)
    (orm-create book)

    (should (equal (orm-count author) 0))
    (should (equal (orm-count book) 0))

    (orm-insert ayn-rand)

    (should (equal (orm-count author) 1))
    (should (equal (orm-count book) 0))

    (orm-drop author)
    (orm-drop book)

    (orm-disconnect orm-default-conn)))

(provide 'orm-crud-tests)
