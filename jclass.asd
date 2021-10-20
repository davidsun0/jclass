;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defsystem "jclass"
  :version "0.1.1"
  :author "David Sun"
  :license "MIT"
  :description "Java class file manipulation"
  :depends-on ("float-features")
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "constant-pool" :depends-on ("package"))
     (:file "bytecode"      :depends-on ("package"))
     (:file "structures"    :depends-on ("constant-pool" "bytecode"))
     (:file "jclass"        :depends-on ("structures")))))
  :in-order-to ((test-op (test-op "jclass/tests"))))

(defsystem "jclass/tests"
  :depends-on ("jclass" "fiveam")
  :components
  ((:module "test"
    :serial t
    :components ((:file "jclass-tests"))))
  :perform (test-op (op c)
	     (symbol-call :fiveam '#:run! (find-symbol* '#:all-tests '#:jclass/tests))))
