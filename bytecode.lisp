(in-package #:jclass)

(defparameter *bytecode-encoders*
  ;; 205 instructions used / 256 = 0.8008
  (make-hash-table :size 256 :rehash-threshold 0.85))

(defparameter *bytecode-decoders*
  ;; default value should be lambda that errors
  (make-array 256 :initial-element nil))

(let ((simple-instructions
	'((#x00 :nop)
	  ;; constants
	  (#x01 :aconst_null)
	  (#x02 :iconst_m1)
	  (#x03 :iconst_0) (#x04 :iconst_1) (#x05 :iconst_2)
	  (#x06 :iconst_3) (#x07 :iconst_4) (#x08 :iconst_5)
	  (#x09 :lconst_0) (#x0A :lconst_1)
	  (#x0B :fconst_0) (#x0C :fconst_1) (#x0D :fconst_2)
	  (#x0E :dconst_0) (#x0F :dconst_1)
	  ;; loads
	  (#x1A :iload_0) (#x1B :iload_1) (#x1C :iload_2) (#x1D :iload_3)
	  (#x1E :lload_0) (#x1F :lload_1) (#x20 :lload_2) (#x21 :lload_3)
	  (#x22 :fload_0) (#x23 :fload_1) (#x24 :fload_2) (#x25 :fload_3)
	  (#x26 :dload_0) (#x27 :dload_1) (#x28 :dload_2) (#x29 :dload_3)
	  (#x2A :aload_0) (#x2B :aload_1) (#x2C :aload_2) (#x2D :aload_3)
	  (#x2E :iaload) (#x2F :laload) (#x30 :faload) (#x31 :daload)
	  (#x32 :aaload) (#x33 :baload) (#x34 :caload) (#x35 :saload)
	  ;; stores
	  (#x3B :istore_0) (#x3C :istore_1) (#x3D :istore_2) (#x3E :istore_3)
	  (#x3F :lstore_0) (#x40 :lstore_1) (#x41 :lstore_2) (#x42 :lstore_3)
	  (#x43 :fstore_0) (#x44 :fstore_1) (#x45 :fstore_2) (#x46 :fstore_3)
	  (#x47 :dstore_0) (#x48 :dstore_1) (#x49 :dstore_2) (#x4A :dstore_3)
	  (#x4B :astore_0) (#x4C :astore_1) (#x4D :astore_2) (#x4E :astore_3)
	  (#x4F :iastore) (#x50 :lastore) (#x51 :fastore) (#x52 :dastore)
	  (#x53 :aastore) (#x54 :bastore) (#x55 :castore) (#x56 :sastore)
	  ;; stack
	  (#x57 :pop) (#x58 :pop2)
	  (#x59 :dup)  (#x5A :dup_x1)  (#x5B :dup_x2)
	  (#x5C :dup2) (#x5D :dup2_x1) (#x5E :dup2_x2)
	  (#x5F :swap)
	  ;; math
	  (#x60 :iadd) (#x61 :ladd) (#x62 :fadd) (#x63 :dadd)
	  (#x64 :isub) (#x65 :lsub) (#x66 :fsub) (#x67 :dsub)
	  (#x68 :imul) (#x69 :lmul) (#x6A :fmul) (#x6B :dmul)
	  (#x6C :idiv) (#x6D :ldiv) (#x6E :fdiv) (#x6F :ddiv)
	  (#x70 :irem) (#x71 :lrem) (#x72 :frem) (#x73 :drem)
	  (#x74 :ineg) (#x75 :lneg) (#x76 :fneg) (#x77 :dneg)
	  (#x78 :ishl) (#x79 :lshl)
	  (#x7A :ishr) (#x7B :lshr)
	  (#x7C :iushr) (#x7D :lushr)
	  (#x7E :iand) (#x7F :land)
	  (#x80 :ior) (#x81 :lor)
	  (#x82 :ixor) (#x83 :lxor)
	  (#x84 :iinc)
	  ;; conversion
	  (#x85 :i2l) (#x86 :i2f) (#x87 :i2d)
	  (#x88 :l2i) (#x89 :l2f) (#x8A :l2d)
	  (#x8B :f2i) (#x8C :f2l) (#x8D :f2d)
	  (#x8E :d2i) (#x8F :d2l) (#x90 :d2f)
	  (#x91 :i2b) (#x92 :i2c) (#x93 :i2s)
	  ;; comparison
	  (#x94 :lcmp)
	  (#x95 :fcmpl) (#x96 :fcmpg)
	  (#x97 :dcmpl) (#x98 :dcmpg)
	  ;; references
	  (#xBE :arraylength)
	  (#xBF :athrow)
	  (#xC2 :monitorenter) (#xC3 :monitorexit)
	  ;; control flow
	  (#xAC :ireturn) (#xAD :lreturn) (#xAE :freturn)
	  (#xAF :dreturn) (#xB0 :areturn) (#xB1 :return)
	  ;; reserved
	  (#xCA :breakpoint) (#xFE :impdep1) (#xFF :impdep2))))
  (loop for (code instruction) in simple-instructions
	do (setf (gethash instruction *bytecode-encoders*) code)
	do (setf (aref *bytecode-decoders* code) instruction)))

(let ((u1-instructions
	'((#x10 :bipush)
	  (#xA9 :ret)
	  (#x15 :iload)  (#x16 :lload)  (#x17 :fload)  (#x18 :dload)  (#x19 :aload)
	  (#x36 :istore) (#x37 :lstore) (#x38 :fstore) (#x39 :dstore) (#x3A :astore)
	  (#xBC :newarray))))
  (loop for (code instruction) in u1-instructions
	;; do (setf (gethash instruction *bytecode-encoders*)
	do (setf (aref *bytecode-decoders* code)
		 (lambda (bytes pool offset)
		   (declare (ignore pool offset))
		   (list instruction (parse-u1 bytes))))))

(let ((u2-instructions
	'((#x11 :sipush)
	  (#x99 :ifeq) (#x9A :ifne)
	  (#x9B :iflt) (#x9C :ifge) (#x9D :ifgt) (#x9E :ifle)
	  (#x9F :if_icmpeq) (#xA0 :if_icmpne)
	  (#xA1 :if_icmplt) (#xA2 :if_icmpge) (#xA3 :if_icmpgt) (#xA4 :if_icmple)
	  (#xA5 :if_acmpeq) (#xA6 :if_acmpne)
	  (#xA7 :goto)
	  (#xA8 :jsr)
	  (#xC6 :ifnull) (#xC7 :ifnonnull))))
  ;; u2 instructions
  (loop for (code instruction) in u2-instructions
	;; do (setf (gethash instruction *bytecode-encoders*)
	do (setf (aref *bytecode-decoders* code)
		 (lambda (bytes pool offset)
		   (declare (ignore pool offset))
		   (list instruction (parse-u2 bytes))))))

(let ((pool-instructions
	'(;; arbitrary
	  (#x13 :ldc_w)
	  (#x14 :ldc2_w)
	  ;; field descriptor
	  (#xB2 :getstatic) (#xB3 :putstatic)
	  (#xB4 :getfield #xB4) (#xB5 :putfield)
	  ;; method descriptor
	  (#xB6 :invokevirtual) (#xB7 :invokespecial) (#xB8 :invokestatic)
	  ;; class descriptor
	  (#xBB :new)
	  (#xBD :anewarray)
	  (#xC0 :checkcast) (#xC1 :instanceof))))
  )

;; (:ldc #x12)

;; xAA tableswitch
;; xAB lookupswitch

#|
(def-instruction :invokeinterface #xB9 (method count)
  (u2 method)
  (u1 count)
  (u1 0))

(def-instruction :invokedynamic #xBA (callsite)
  (u2 callsite)
  (u2 0))

;; wide #xC4

(def-instruction :multianewarray #xC5 (index dimensions)
  (u2 index)
  (u1 dimensions))

(:goto_w #xC8 (branch) (u4 branch))
(:jsr_w #xC9 (branch) (u4 branch))
|#

;; tableswitch will be difficult because of the padding

;; when encoding / decoding, I need ...
;; - constant pool
;; - bytecode offset for switch instructions / jumps
;; - label map

;; Instructions may have different length based on pool index & args
;; form, pool, labels

;; complex instructions

(defun encode-bytecode (instructions constant-pool)
  (let ((offset 0)
	(output '()))
    (loop for instruction in instructions do
      (let ((encoder (gethash (if (listp instruction)
				  (first instruction)
				  instruction)
			      *bytecode-encoders*)))
	(cond
	  ((functionp encoder)
	   (let ((bytes (funcall encoder constant-pool offset)))
	     (incf offset (length bytes))
	     (push bytes output)))
	  ((integerp encoder)
	   ;; simple instruction - encoder is the byte value
	   (incf offset)
	   (push encoder output))
	  (t (error 'class-format-error
		    :message (format nil "Unknown instruction ~A" instruction))))))
    (list (u4 offset)
	  (nreverse output))))

(defun decode-bytecode (class-bytes constant-pool)
  (let ((code-bytes (parse-bytes (parse-u4 class-bytes)
				 class-bytes))
	(output '())
	(offset 0))
    (loop while (with-slots (array index) code-bytes
		  (> (- (length array) index) 0))
	  do (let ((code (aref *bytecode-decoders* (parse-u1 code-bytes))))
	       (cond
		 ((keywordp code)
		  (push code output))
		 ((functionp code)
		   (multiple-value-bind (opcode new-bytes op-length)
		       (funcall code-bytes constant-pool offset)
		     (setf code-bytes new-bytes)
		     (incf offset op-length)
		     (push output opcode)))
		 (t (error 'class-format-error)))))
    (nreverse output)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-serialization bytecode (instructions) byte-stream
    `(encode-bytecode ,instructions (constant-pool))
    `(setf ,instructions (decode-bytecode ,byte-stream (pool-array)))))

(def-attribute code "Code" (max-stack max-locals bytecode exceptions attributes)
  (u2 max-stack)
  (u2 max-locals)
  ;; u4 length is calculated in encode / decode functions
  (bytecode bytecode)
  (with-length u2 exceptions (start-pc end-pc handler-pc catch-type)
    (u2 start-pc)
    (u2 end-pc)
    (u2 handler-pc)
    (u2 (class-info catch-type)))
  (with-length u2 attributes attribute
    (attribute attribute)))
