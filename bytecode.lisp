(in-package #:jclass)

(defparameter *bytecode-encoders*
  ;; 205 instructions used / 256 = 0.8008
  (make-hash-table :size 256 :rehash-threshold 0.85))

(defparameter *bytecode-decoders*
  ;; default value should be lambda that errors
  (make-array 256 :initial-element nil))

(dolist (simple-instruction
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
	   (#xCA :breakpoint) (#xFE :impdep1) (#xFF :impdep2)))
  (destructuring-bind (code instruction) simple-instruction
    (setf (gethash instruction *bytecode-encoders*) code)
    (setf (aref *bytecode-decoders* code) instruction)))

(dolist (u1-instruction
	 '((#x10 :bipush)
	   (#x15 :iload)  (#x16 :lload)  (#x17 :fload)  (#x18 :dload)  (#x19 :aload)
	   (#x36 :istore) (#x37 :lstore) (#x38 :fstore) (#x39 :dstore) (#x3A :astore)
	   (#xA9 :ret) (#xBC :newarray)))
  (destructuring-bind (code instruction) u1-instruction
    (setf (gethash instruction *bytecode-encoders*)
	  (lambda (form pool offset)
	    (declare (ignore pool offset))
	    (cons code (u1 (first form)))))
    (setf (aref *bytecode-decoders* code)
	  (lambda (bytes pool offset)
	    (declare (ignore pool offset))
	    (list instruction (parse-u1 bytes))))))
    
(dolist (u2-instruction
	 '((#x11 :sipush)
	   (#x99 :ifeq) (#x9A :ifne)
	   (#x9B :iflt) (#x9C :ifge) (#x9D :ifgt) (#x9E :ifle)
	   (#x9F :if_icmpeq) (#xA0 :if_icmpne)
	   (#xA1 :if_icmplt) (#xA2 :if_icmpge)
	   (#xA3 :if_icmpgt) (#xA4 :if_icmple)
	   (#xA5 :if_acmpeq) (#xA6 :if_acmpne)
	   (#xA7 :goto)
	   (#xA8 :jsr)
	   (#xC6 :ifnull) (#xC7 :ifnonnull)))
  (destructuring-bind (code instruction) u2-instruction
    (setf (gethash instruction *bytecode-encoders*)
	  (lambda (form pool offset)
	    (declare (ignore pool offset))
	    (cons code (u2 (first form)))))
    (setf (aref *bytecode-decoders* code)
	  (lambda (bytes pool offset)
	    (declare (ignore pool offset))
	    (list instruction (parse-u2 bytes))))))

(defmacro def-encoding (instruction opcode encoder decoder)
  `(progn
     (setf (gethash ,instruction *bytecode-encoders*)
	   (lambda (form pool offset)
	     (declare (ignorable pool offset))
	     ;; unhygenic: exposes form, pool, offset
	     (cons ,opcode ,encoder)))
     (setf (aref *bytecode-decoders* ,opcode)
	   (lambda (bytes pool offset)
	     (declare (ignorable pool offset))
	     ;; unhygenic: exposes bytes, pool, offset
	     (cons ,instruction ,decoder)))))

(def-encoding :ldc #x12
  ;; check for pool index < 256 ?
  (u1 (pool-index pool (second form)))
  (list (aref pool (parse-u1 bytes))))

(def-encoding :ldc_w #x13
  (u2 (pool-index pool (second form)))
  (list (aref pool (parse-u2 bytes))))

(def-encoding :ldc2_w #x14
  ;; check for long / double constant type?
  (u2 (pool-index pool (second form)))
  (list (aref pool (parse-u2 bytes))))

#|
(def-encoding :tableswitch #xAA
  ()
  ())

(def-encodnig :lookupswitch #xAB
  ()
  ())
|#

(dolist (field-instruction
	 '((#xB2 :getstatic)
	   (#xB3 :putstatic)
	   (#xB4 :getfield)
	   (#xB5 :putfield)))
  (destructuring-bind (code instruction) field-instruction
    (setf (gethash instruction *bytecode-encoders*)
	  (lambda (form pool offset)
	    (declare (ignore offset))
	    (destructuring-bind (class-name name type) (rest form)
	      (let* ((field-ref (make-field-ref-info class-name name type))
		     (field-index (pool-index pool field-ref)))
		(cons code (u2 field-index))))))
    (setf (aref *bytecode-decoders* code)
	  (lambda (bytes pool offset)
	    (declare (ignore offset))
	    (let ((field-ref (aref pool (parse-u2 bytes))))
	      (list instruction
		    (field-ref-info-class-name field-ref)
		    (field-ref-info-name field-ref)
		    (field-ref-info-type field-ref)))))))

(def-encoding :invokevirtual #xB6
  (destructuring-bind (class-name name type) (rest form)
    (u2 (pool-index pool (make-field-ref-info class-name name type))))
  (let ((method-ref (aref pool (parse-u2 bytes))))
    (list (method-ref-info-class-name method-ref)
	  (method-ref-info-name method-ref)
	  (method-ref-info-type method-ref))))

(def-encoding :invokespecial #xB7
  (u2 (pool-index pool (second form)))
  (let ((method-ref (aref pool (parse-u2 bytes))))
    ;; method-ref may be either a method-ref-info or interface-method-ref-info
    (list method-ref)))

(def-encoding :invokestatic #xB8
  (destructuring-bind (class-name name type) (rest form)
    (u2 (pool-index pool (make-field-ref-info class-name name type))))
  (let ((method-ref (aref pool (parse-u2 bytes))))
    (list (method-ref-info-class-name method-ref)
	  (method-ref-info-name method-ref)
	  (method-ref-info-type method-ref))))

#|
invokeinterface syntax from the JVM specification:
      
invokeinterface
indexbyte1
indexbyte2
count
0

Count is an unused byte that must not be zero.
javac emits a 1 for count, so we do the same here.
The 0 byte is also unused.
When decoding, we simply ignore the two unused bytes.
|#
(def-encoding :invokeinterface #xB9
  (destructuring-bind (class-name name type) (rest form)
    (let* ((method-ref (make-interface-method-ref-info class-name name type))
	   (index (u2 (pool-index pool method-ref))))
      (append index '(1 0))))
  (let ((method-ref (aref pool (parse-u2 bytes)))
	(_ (parse-u2 bytes))) ;; skip unused bytes
    (declare (ignore _))
    (list (interface-method-ref-info-class-name method-ref)
	  (interface-method-ref-info-name method-ref)
	  (interface-method-ref-info-type method-ref))))

#|
Like invokeinterface, invokedynamic has two unused bytes:

invokedynamic
indexbyte1
indexbyte2
0
0

Both must be zero and are reserved for future use by the JVM.
|#
(def-encoding :invokedynamic #xBA
  (destructuring-bind (index name type) (rest form)
    (let* ((dynamic-ref (make-invoke-dynamic-info index name type))
	   (index (u2 (pool-index pool dynamic-ref))))
      (append index '(0 0))))
  (let ((dynamic-ref (aref pool (parse-u2 bytes)))
	(_ (parse-u2 bytes))) ;; ignore unused bytes
    (declare (ignore _))
    (list (invoke-dynamic-info-bootstrap-index dynamic-ref)
	  (invoke-dynamic-info-name dynamic-ref)
	  (invoke-dynamic-info-type dynamic-ref))))

(dolist (class-instruction
	 '((#xBB :new)
	   (#xBD :anewarray)
	   (#xC0 :checkcast)
	   (#xC1 :instanceof)))
  (destructuring-bind (code instruction) class-instruction
    (setf (aref *bytecode-decoders* code)
	  (lambda (bytes pool offset)
	    (declare (ignore offset))
	    (list instruction
		  (class-info-name (aref pool (parse-u2 bytes))))))))

#|
(def-encoding :wide #xC4
  (cond
    ((eq (second form) :iinc)
     (destructuring-bind (index const) (rest (rest form))
       (append '(#x84) ; iinc opcode
	       (u2 index)
	       (u2 const))))
    ((member (second form) '(:iload  :fload  :aload  :lload  :dstore
			     :istore :fstore :astore :lstore :dstore :ret))
     (cons opcode (u2 index)))
    (t (error 'class-format-error
	      :message (format nil "Invalid wide instruction ~A" form))))
  (let ((op (parse-u1 bytes)))
    (cond
      ((= op #x84) ; iinc
       (list :iinc (parse-u2 bytes) (parse-u2 bytes)))
      ((check membership here)
       (list (opcode) (parse-u2 bytes)))
      (t (error 'class-format-error
		:message (format nil "Unknown wide operand ~A" op))))))
|#

(def-encoding :multianewarray #xC5
  (destructuring-bind (class-name dimensions) (rest form)
    (let* ((class-ref (make-class-info class-name))
	   (index (u2 (pool-index pool class-ref))))
      (append index dimensions)))
  (cons (class-info-name (aref pool (parse-u2 bytes)))
	(parse-u1 bytes)))

(def-encoding :goto_w #xC8
  (u4 (second form))
  (parse-u4 bytes))

(def-encoding :jsr_w #xC9
  (u4 (second form))
  (parse-u4 bytes))

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
	   (let ((bytes (funcall encoder instruction constant-pool offset)))
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

(defun decode-bytecode (code-bytes constant-pool)
  (let ((bytecode-length (parse-u4 code-bytes))
	(start-index (class-bytes-index code-bytes))
	(offset 0)
	(output '()))
    (loop do (setf offset (- (class-bytes-index code-bytes) start-index))
	  while (< (- (class-bytes-index code-bytes) start-index)
		   bytecode-length)
	  do (let* ((opcode (parse-u1 code-bytes))
		    (decoder (aref *bytecode-decoders* opcode)))
	       (cond
		 ((keywordp decoder)
		  (push decoder output))
		 ((functionp decoder)
		  (let ((opcode (funcall decoder code-bytes constant-pool offset)))
		    (push opcode output)))
		 (t (error 'class-format-error
			   :message (format nil "Unknown instruction ~A" opcode))))))
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
