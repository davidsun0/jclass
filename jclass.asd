;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defsystem "jclass"
  :version "1.0.0"
  :author "David Sun"
  :license "MIT"
  :description "Java class file manipulation"
  :depends-on ("float-features")
  :components
  ((:file "package")
   (:file "utility" :depends-on ("package"))
   (:file "constant-pool" :depends-on ("utility"))
   (:file "bytecode" :depends-on ("constant-pool"))
   (:file "structures" :depends-on ("bytecode" "constant-pool"))
   (:file "print-object" :depends-on ("structures"))
   (:file "jclass" :depends-on ("structures")))
  :in-order-to ((test-op (test-op "jclass/tests"))))

(defsystem "jclass/tests"
  :depends-on ("jclass" "fiveam")
  :components
  ((:module "tests"
    :serial t
    :components ((:file "package")
		 (:file "modified-utf8")
		 (:file "serialization"))))
  :perform (test-op (op c)
	     (symbol-call :fiveam '#:run! (find-symbol* '#:all-tests '#:jclass/tests))))
