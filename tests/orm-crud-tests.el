(require 'ert)
(require 'orm)

(deftable author ()
	  ((name :initarg :name
		 :primary-key t
		 :not-null t)
	   (age :initarg :age))
	  :table authors)

(deftable book ()
	  ((title :initarg :title
		  :primary-key t
		  :not-null t)
	   (year :initarg :year))
	  :associations
	  ((:belongs_to author :foreign-key writer)))

(ert-deftest orm-book-crud-example ()
  "Tests orm-create"
  (skip-unless (not (file-exists-p "/tmp/test.db")))

  ;; TODO: Figure out why the let-form doesn't work for these
  (setq orm-default-db (orm-db :type :sql :file "/tmp/test.db"))
  (setq orm-default-conn (orm-connect orm-default-db))

  (setq ayn-rand (author :name "Ayn Rand" :age 77))


  (let ((ayn-rand (author :name "Ayn Rand" :age 77))
	(franz-kafka (author :name "Franz Kafka" :age 41)))

    ;; Create Table (orm-create)

    (orm-create author)

    ;; CRUD Methods

    ;; Create (orm-insert)

    (should (equal (orm-count author) 0))

    (orm-insert ayn-rand)

    (should (equal (orm-count author) 1))

    (orm-insert franz-kafka)

    (should (equal (orm-count author) 2))

    ;; Read (orm-first, orm-all)

    ;; Order after insertion isn't tested
    (let ((first-author-name (slot-value (orm-first author) 'name)))
      (should (or (equal first-author-name "Ayn Rand")
		  (equal first-author-name "Franz Kafka"))))

    ;; Read & Update (orm-find, orm-update)

    (let ((inserted-franz-kafka-record1 (orm-find author "Franz Kafka")))
      (should (equal (slot-value franz-kafka 'age) 41))
      (should (equal (slot-value inserted-franz-kafka-record1 'age) 41))

      (setf (slot-value franz-kafka 'age) 40)
      (orm-update franz-kafka)

      (let ((inserted-franz-kafka-record2 (orm-find author "Franz Kafka")))
	(should (equal (slot-value franz-kafka 'age) 40))
	(should (equal (slot-value inserted-franz-kafka-record2 'age) 40))))

    ;; Delete (orm-delete)

    (orm-delete franz-kafka)

    (should (equal (orm-count author) 1))

    (orm-delete ayn-rand)

    (should (equal (orm-count author) 0))

    ;; Drop Table (orm-drop)

    (orm-drop author)

    (orm-disconnect orm-default-conn)
    (shell-command "rm /tmp/test.db")))

(provide 'orm-crud-tests)
