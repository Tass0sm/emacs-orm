(require 'ert)

(require 'orm-tests-setup)
(require 'orm-table)


(ert-deftest orm-table-schema ()
  "Tests orm-table-schema"

  ;; Simple
  (should (equal
           (orm-table-schema author)
           '([(name :primary-key :not-null) (age)])))

  ;; With belongs-to association
  (should (equal
           (orm-table-schema book)
           '([(title :primary-key :not-null) (writer)]
             (:foreign-key [writer] :references authors [name])))))

(ert-deftest orm-table-name ()
  "Tests orm-table-name"
  (should (equal (orm-table-name author) 'authors))
  (should (equal (orm-table-name book) 'books)))

(ert-deftest orm-table-primary-key ()
  "Test orm-table-primary-key"
  (should (equal (orm-table-primary-key author) [name]))
  (should (equal (orm-table-primary-key book) [title])))

(provide 'orm-table-tests)
