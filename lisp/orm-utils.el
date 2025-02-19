;;; -*- lexical-binding: t; -*-

(require 'generator)
(require 'cl-lib)

(iter-defun orm--by-two (l)
  "Iterate over a list in chunks of two elements"
  (while l
    (iter-yield (list (car l) (cadr l)))
    (setq l (cddr l))))

(defun orm--filter-plist (plist keys)
  (apply 'append
	 (cl-loop for x iter-by (orm--by-two plist)
		  if (member (car x) keys)
		  collect x)))

(provide 'orm-utils)
