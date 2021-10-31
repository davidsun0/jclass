(in-package #:jclass/tests)
(fiveam:in-suite all-tests)

;;; Class Components

(fiveam:test access-flags
  (let* ((keywords '(:public :super :abstract))
	 (flags (jclass::access-modifiers keywords jclass::*class-modifiers*)))
    (fiveam:is (= #x0421 flags)))
  (let* ((inverse (jclass::access-flag-lookup #x0421 jclass::*class-modifiers*)))
    (fiveam:is (equal '(:public :super :abstract) inverse))))

;; Used in invokeinterface encoding
(fiveam:test interface-count
  (fiveam:is (= 1 (jclass::interface-count "()V")))
  (fiveam:is (= 3 (jclass::interface-count "(D)V")))
  (fiveam:is (= 3 (jclass::interface-count "(J)V")))
  (fiveam:is (= 2 (jclass::interface-count "(Ljava/lang/Object;)V")))
  (fiveam:is (= 2 (jclass::interface-count "([D)V")))
  (fiveam:is (= 2 (jclass::interface-count "([[[Ljava/lang/Object;)V"))))

;;; Testing jclass against files created by javac
;; (1) Decode class from file
;; (2) Build a constant pool with the same indicies
;; (3) Encode the class and check that the bytes are the same

(defun recreate-pool (pool-array)
  (let ((pool (jclass::make-constant-pool)))
    ;; Ignore constant #0, which is always NIL.
    ;; See "Optional Constants" in the Manual.
    (loop for i from 1 below (length pool-array)
	  for constant = (aref pool-array i)
	  when constant
	    do (jclass::pool-index pool constant))
    pool))

(defun re-encode-class (bytes)
  (multiple-value-bind (jclass pool-array)
      (jclass:disassemble-jclass bytes)
    (jclass:java-class-bytes jclass (recreate-pool pool-array))))

(defun test-file-path (name)
  (let ((local-path (format nil "test/data/~A.class" name)))
    (asdf:system-relative-pathname "jclass" local-path)))

(defun test-class-file (filename)
  (with-open-file (stream (test-file-path filename)
			  :direction :input
			  :element-type '(unsigned-byte 8)
			  :if-does-not-exist :error)
    (let* ((length (file-length stream))
	   (buffer (make-array length :element-type '(unsigned-byte 8))))
      (read-sequence buffer stream)
      (equal (coerce buffer 'list) (re-encode-class buffer)))))

(defun dump-re-encode (name output-path)
  (with-open-file (input (test-file-path name)
			 :direction :input
			 :element-type '(unsigned-byte 8)
			 :if-does-not-exist :error)
    (let* ((length (file-length input))
	   (buffer (make-array length :element-type '(unsigned-byte 8))))
      (read-sequence buffer input)
      (with-open-file (output output-path
			      :direction :output
			      :if-exists :supersede
			      :element-type '(unsigned-byte 8))
	(write-sequence (re-encode-class buffer) output)))))

(fiveam:test serialization
  (fiveam:is (test-class-file "Hello")
	     "Re-encoding produced different binary files.")
  (fiveam:is (test-class-file "Math")
	     "Re-encoding produced different binary files.")
  (fiveam:is (test-class-file "Switch")
	     "Re-encoding produced different binary files.")
  (fiveam:is (test-class-file "Unicode")
	     "Re-encoding produced different binary files."))
