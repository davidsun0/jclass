;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defsystem "jclass"
  :version "0.2.0"
  :author "David Sun"
  :license "MIT"
  :description "Java class file manipulation"
  :depends-on ("float-features")
  :components
  ((:file "package")
   (:module "bytecode"
	    :depends-on ("package")
	    :serial t
	    :components
	    ((:file "encoding")))
   (:module "class"
	    :depends-on ("package" "bytecode")
	    :serial t
	    :components
	    ((:file "constant-pool")
	     (:file "structures")
	     (:file "jclass"))))
  :in-order-to ((test-op (test-op "jclass/tests"))))

(defsystem "jclass/tests"
  :depends-on ("jclass" "fiveam")
  :components
  ((:module "test"
    :serial t
    :components ((:file "package")
		 (:file "modified-utf8")
		 (:file "serialization"))))
  :perform (test-op (op c)
	     (symbol-call :fiveam '#:run! (find-symbol* '#:all-tests '#:jclass/tests))))
