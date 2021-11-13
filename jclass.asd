;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defsystem "jclass"
  :version "0.2.1"
  :author "David Sun"
  :license "MIT"
  :description "Java class file manipulation"
  :depends-on ("float-features")
  :components
  ((:module "core"
	    :components
	    ((:file "package")
	     (:file "utility" :depends-on ("package"))
	     (:file "constant-pool" :depends-on ("utility"))
	     (:file "bytecode" :depends-on ("constant-pool"))
	     (:file "structures" :depends-on ("bytecode" "constant-pool"))
	     (:file "jclass" :depends-on ("structures")))))
  :in-order-to ((test-op (test-op "jclass/tests"))))

(defsystem "jclass/tests"
  :depends-on ("jclass" "fiveam")
  :components
  ((:module "core.test"
    :serial t
    :components ((:file "package")
		 (:file "modified-utf8")
		 (:file "serialization"))))
  :perform (test-op (op c)
	     (symbol-call :fiveam '#:run! (find-symbol* '#:all-tests '#:jclass/tests))))
