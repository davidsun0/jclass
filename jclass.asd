;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defsystem "jclass"
  :version "0.1.0"
  :author "David Sun"
  :description "Java class file manipulation"
  :serial t
  :components ((:file "constant-pool")
	       (:file "structures")
	       (:file "bytecode")))
