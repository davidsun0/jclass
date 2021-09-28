;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defsystem "jclass"
  :version "0.1.1"
  :author "David Sun"
  :license "MIT"
  :description "Java class file manipulation"
  :components ((:module "src"
		:serial t
		:components ((:file "package")
			     (:file "constant-pool")
			     (:file "structures")
			     (:file "bytecode")
			     (:file "jclass"))))
  :in-order-to ((test-op (test-op "jclass/tests"))))

(defsystem "jclass/tests"
  :depends-on ("jclass" "fiveam")
  :components ((:module "test"
		:serial t
		:components ((:file "jclass-tests"))))
  :perform (test-op (op c)
	     (symbol-call :fiveam '#:run! (find-symbol* '#:all-tests '#:jclass/tests))))
