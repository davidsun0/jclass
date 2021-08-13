(in-package #:jclass)

(defparameter *bytecode-handlers*
  (make-hash-table :size 384)) ; 256 instructions * 1.5

;; Instructions may have different length based on pool index & args
;; form, pool, labels

(defmacro def-instruction (mnemonic &body body)
  `(setf (gethash ,mnemonic *bytecode-handlers*)
	 (lambda (form pool)
	   (declare (ignorable form pool))
	   ,@body)))

(def-instruction :nop '(#x00))
(def-instruction :aconst_null '(#x01))

;; tableswitch will be difficult because of the padding

(defstruct bytecode
  assembled-p
  instructions
  (label-map nil))

(def-attribute code "Code" (max-stack max-locals bytecode exceptions attributes)
  (u2 max-stack)
  (u2 max-locals)
  (u4 (length bytecode))
  ;; bytecode ; constant resolution is done seperately in assemble-bytecode
  (byte-list (constant-pool) bytecode)
  (with-length u2 exceptions ((start-pc end-pc handler-pc catch-type))
    (u2 start-pc)
    (u2 end-pc)
    (u2 handler-pc)
    (u2-pool-index catch-type))
  (u2 (length exceptions))
  (inner-structs exceptions (start-pc end-pc handler-pc catch-type)
    (u2 start-pc)
    (u2 end-pc)
    (u2 handler-pc)
    (u2-pool-index catch-type))
  (u2 (length attributes))
  (substructs attributes))

(def-attribute-parser "Code" (bytes pool-array)
  (let ((max-stack  (parse-u2 bytes))
	(max-locals (parse-u2 bytes))
	(bytecode   (parse-bytes (parse-u4 bytes) bytes))
	(exceptions (loop repeat (parse-u2 bytes)
			  collect (list
				   (parse-u2 bytes)
				   (parse-u2 bytes)
				   (parse-u2 bytes)
				   (u2-pool-index bytes pool-array))))
	(attributes (loop repeat (parse-u2 bytes)
			  collect (parse-attribute bytes pool-array))))
    (make-code max-stack max-locals bytecode exceptions attributes)))

(defun stack-map-entries (entries pool)
  
  )

(def-attribute stack-map-table "StackMapTable" (entries)
  (u2 (length entries))
  (stack-map-entries entries (constant-pool)))
