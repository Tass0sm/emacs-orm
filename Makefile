test:
	emacs -batch -l ert -L "/Users/tassos/.emacs.d/elpa/emacsql-20241201.1551" -L lisp -L tests -l orm-tests -f ert-run-tests-batch-and-exit

clean-tests:
	rm /tmp/test.db
