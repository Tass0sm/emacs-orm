(require 'orm)

(deftable author ()
          ((name :initarg :name
                 :primary-key t
                 :not-null t)
           (age  :initarg :age))
          :table
          authors)

(deftable book ()
          ((title :initarg :title
                  :primary-key t
                  :not-null t))
          :associations
          ((:belongs-to author :foreign-key writer))
          :table
          books)

(provide 'orm-tests-setup)
