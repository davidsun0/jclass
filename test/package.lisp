(defpackage #:jclass/tests
  (:use :cl)
  (:export #:all-tests))
(in-package #:jclass/tests)

(fiveam:def-suite all-tests
  :description "jclass test suite")

(defun all-tests ()
  (fiveam:run! 'all-tests))
