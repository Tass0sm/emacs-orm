;;; -*- lexical-binding: t -*-

(require 'eieio)
(require 'emacsql-sqlite-builtin)

(defclass orm-db ()
  ((type :initarg :type)
   (file :initarg :file))
  :documentation
  "Class of database specification.")

(cl-defmethod orm-connect ((db orm-db))
  (pcase (oref db type)
    (:sql (emacsql-sqlite-builtin (oref db file)))
    (_ nil)))

(defvar orm-default-db (orm-db :type :sql :file "/home/tassos/desktop/test.db"))
(defvar orm-default-conn (orm-connect orm-default-db))

(provide 'orm-db-connection)
