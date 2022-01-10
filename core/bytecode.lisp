(in-package #:jclass)

;;; Bytecode instruction encoding and decoding

(defparameter *bytecode-encoders*
  ;; 205 instructions used / 256 = 0.8008
  (make-hash-table :size 256 :rehash-threshold 0.85))

(defparameter *bytecode-decoders*
  ;; default value should be lambda that errors
  (make-array 256 :initial-element nil))

(fresh-dolist (simple-instruction
	 '((#x00 :nop)
	   ;; constants
	   (#x01 :aconst-null)
	   (#x02 :iconst-m1)
	   (#x03 :iconst-0) (#x04 :iconst-1) (#x05 :iconst-2)
	   (#x06 :iconst-3) (#x07 :iconst-4) (#x08 :iconst-5)
	   (#x09 :lconst-0) (#x0A :lconst-1)
	   (#x0B :fconst-0) (#x0C :fconst-1) (#x0D :fconst-2)
	   (#x0E :dconst-0) (#x0F :dconst-1)
	   ;; loads
	   (#x1A :iload-0) (#x1B :iload-1) (#x1C :iload-2) (#x1D :iload-3)
	   (#x1E :lload-0) (#x1F :lload-1) (#x20 :lload-2) (#x21 :lload-3)
	   (#x22 :fload-0) (#x23 :fload-1) (#x24 :fload-2) (#x25 :fload-3)
	   (#x26 :dload-0) (#x27 :dload-1) (#x28 :dload-2) (#x29 :dload-3)
	   (#x2A :aload-0) (#x2B :aload-1) (#x2C :aload-2) (#x2D :aload-3)
	   (#x2E :iaload) (#x2F :laload) (#x30 :faload) (#x31 :daload)
	   (#x32 :aaload) (#x33 :baload) (#x34 :caload) (#x35 :saload)
	   ;; stores
	   (#x3B :istore-0) (#x3C :istore-1) (#x3D :istore-2) (#x3E :istore-3)
	   (#x3F :lstore-0) (#x40 :lstore-1) (#x41 :lstore-2) (#x42 :lstore-3)
	   (#x43 :fstore-0) (#x44 :fstore-1) (#x45 :fstore-2) (#x46 :fstore-3)
	   (#x47 :dstore-0) (#x48 :dstore-1) (#x49 :dstore-2) (#x4A :dstore-3)
	   (#x4B :astore-0) (#x4C :astore-1) (#x4D :astore-2) (#x4E :astore-3)
	   (#x4F :iastore) (#x50 :lastore) (#x51 :fastore) (#x52 :dastore)
	   (#x53 :aastore) (#x54 :bastore) (#x55 :castore) (#x56 :sastore)
	   ;; stack
	   (#x57 :pop) (#x58 :pop2)
	   (#x59 :dup)  (#x5A :dup-x1)  (#x5B :dup-x2)
	   (#x5C :dup2) (#x5D :dup2-x1) (#x5E :dup2-x2)
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
    (setf (gethash instruction *bytecode-encoders*) (list code))
    (setf (aref *bytecode-decoders* code) instruction)))

(defmacro define-encoding (instruction opcode encoder decoder)
  `(progn
     (setf (gethash ,instruction *bytecode-encoders*)
	   (lambda (operands pool offset)
	     (declare (ignorable pool offset))
	     ;; unhygenic: exposes operands, pool, offset
	     (cons ,opcode ,encoder)))
     (setf (aref *bytecode-decoders* ,opcode)
	   (lambda (bytes pool offset)
	     (declare (ignorable pool offset))
	     ;; unhygenic: exposes bytes, pool, offset
	     (cons ,instruction ,decoder)))))

;; The bipush operand is signed, unlike that of the other u1-instructions.
(define-encoding :bipush #x10
  ;; The function u1 automatically handles signed encoding.
  (u1 (first operands))
  (list (signed-8 (parse-u1 bytes))))

(fresh-dolist (u1-instruction
	 '((#x15 :iload)  (#x16 :lload)  (#x17 :fload)  (#x18 :dload)  (#x19 :aload)
	   (#x36 :istore) (#x37 :lstore) (#x38 :fstore) (#x39 :dstore) (#x3A :astore)
	   (#xA9 :ret) (#xBC :newarray)))
  (destructuring-bind (code instruction) u1-instruction
    (define-encoding instruction code
      (u1 (first operands))
      (list (parse-u1 bytes)))))

(define-encoding :sipush #x11
  (u2 (first operands))
  (list (signed-16 (parse-u2 bytes))))

(fresh-dolist (branch-instruction
	 '((#x99 :ifeq) (#x9A :ifne)
	   (#x9B :iflt) (#x9C :ifge) (#x9D :ifgt) (#x9E :ifle)
	   (#x9F :if-icmpeq) (#xA0 :if-icmpne)
	   (#xA1 :if-icmplt) (#xA2 :if-icmpge)
	   (#xA3 :if-icmpgt) (#xA4 :if-icmple)
	   (#xA5 :if-acmpeq) (#xA6 :if-acmpne)
	   (#xA7 :goto)
	   (#xA8 :jsr)
	   (#xC6 :ifnull) (#xC7 :ifnonnull)))
  (destructuring-bind (code instruction) branch-instruction
    (define-encoding instruction code
      (u2 (- (first operands) offset))
      (list (+ (signed-16 (parse-u2 bytes))
	       offset)))))

(define-encoding :ldc #x12
  ;; check for pool index < 256 ?
  (u1 (pool-index pool (first operands)))
  (list (aref pool (parse-u1 bytes))))

(define-encoding :ldc-w #x13
  (u2 (pool-index pool (first operands)))
  (list (aref pool (parse-u2 bytes))))

(define-encoding :ldc2-w #x14
  ;; check for long / double constant type?
  (u2 (pool-index pool (first operands)))
  (list (aref pool (parse-u2 bytes))))

(define-encoding :iinc #x84
  (append (u1 (first operands))
	  (u1 (second operands)))
  (list (parse-u1 bytes)
	(signed-8 (parse-u1 bytes))))

(define-encoding :tableswitch #xAA
  (destructuring-bind (default low high &rest offsets) operands
    (flatten
     (list
      ;; padding bytes
      (loop repeat (- 3 (mod offset 4))
	    collect 0)
      (u4 default)
      (u4 low)
      (u4 high)
      (mapcar #'u4 offsets))))
  (let* ((padding (loop repeat (- 3 (mod offset 4))
			do (parse-u1 bytes)))
	 (default (signed-32 (parse-u4 bytes)))
	 (low     (signed-32 (parse-u4 bytes)))
	 (high    (signed-32 (parse-u4 bytes))))
    (declare (ignore padding))
    (list* default
	   low
	   high
	   (loop repeat (- (1+ high) low)
		 collect (parse-u4 bytes)))))

(define-encoding :lookupswitch #xAB
  (destructuring-bind (default &rest match-offset-pairs) operands
    (flatten
     (list
      ;; padding bytes
      (loop repeat (- 3 (mod offset 4))
	    collect 0)
      (u4 default)
      (u4 (length match-offset-pairs))
      (loop for (key offset) in match-offset-pairs
	    collect (u4 key)
	    collect (u4 offset)))))
  (let ((padding (loop repeat (- 3 (mod offset 4))
		       do (parse-u1 bytes)))
	(default (signed-32 (parse-u4 bytes)))
	(length  (signed-32 (parse-u4 bytes))))
    (declare (ignore padding))
    (assert (>= length 0) (length) 'class-format-error
	    :message "Lookupswitch length must be zero or greater.")
    (cons default
	  (loop repeat length
		collect (list (signed-32 (parse-u4 bytes))
			      (signed-32 (parse-u4 bytes)))))))

(fresh-dolist (field-instruction
	 '((#xB2 :getstatic)
	   (#xB3 :putstatic)
	   (#xB4 :getfield)
	   (#xB5 :putfield)))
  (destructuring-bind (code instruction) field-instruction
    (define-encoding instruction code
      (destructuring-bind (class-name name type) operands
	(let* ((field-ref (make-field-ref-info class-name name type))
	       (field-index (pool-index pool field-ref)))
	  (u2 field-index)))
      (let ((field-ref (aref pool (parse-u2 bytes))))
	(list (field-ref-info-class-name field-ref)
	      (field-ref-info-name field-ref)
	      (field-ref-info-type field-ref))))))

(define-encoding :invokevirtual #xB6
  (destructuring-bind (class-name name type) operands
    (u2 (pool-index pool (make-method-ref-info class-name name type))))
  (let ((method-ref (aref pool (parse-u2 bytes))))
    (list (method-ref-info-class-name method-ref)
	  (method-ref-info-name method-ref)
	  (method-ref-info-type method-ref))))

(define-encoding :invokespecial #xB7
  (u2 (pool-index pool (first operands)))
  (let ((method-ref (aref pool (parse-u2 bytes))))
    ;; method-ref may be either a method-ref-info or interface-method-ref-info
    (list method-ref)))

(define-encoding :invokestatic #xB8
  (destructuring-bind (class-name name type) operands
    (u2 (pool-index pool (make-method-ref-info class-name name type))))
  (let ((method-ref (aref pool (parse-u2 bytes))))
    (list (method-ref-info-class-name method-ref)
	  (method-ref-info-name method-ref)
	  (method-ref-info-type method-ref))))

;; invokeinterface syntax from the JVM specification:
;;
;; invokeinterface
;; indexbyte1
;; indexbyte2
;; count
;; 0
;;
;; Count is the number of parameter slots the method takes.
;; Longs and doubles take two slots and other primitives and object references
;; take one. Note that the interface object itself takes up one slot.
;; This information is redudant and is calculated from the method type, so count
;; is ignored when decoding.
;; The zero byte is unused.

(defun parse-field-descriptor (string index)
  (cond
    ((char= (char string index) #\L)
     (loop for count from 1
	   when (char= (char string (+ index count)) #\;)
	     return (cons index (1+ count))))
    ((char= (char string index) #\[)
     (loop for count from 1
	   when (char/= (char string (+ index count)) #\[)
	     return (let* ((start (+ index count))
			   (descriptor (parse-field-descriptor string start))
			   (length (cdr descriptor)))
		      (cons index (+ count length)))))
    ((member (char string index)
	     '(#\B #\C #\D #\F #\I #\J #\S #\Z)
	     :test #'char=)
     (cons index 1))
    (t
     (error 'class-format-error
	    :message (format nil "Invalid method descriptor ~A" string)))))

(defun interface-count (string)
  (let ((descriptor '())
	(count (lambda (descriptor)
		 (let* ((index (car descriptor))
			(char (char string index)))
		   (if (member char '(#\D #\J) :test #'char=) 2 1)))))
    (loop for index = 1 then index
	  when (char= (char string index) #\))
	    ;; add one for the interface object itself
	    return (1+ (reduce #'+ (mapcar count pairs)))
	  do (setf descriptor (parse-field-descriptor string index))
	     (incf index (cdr descriptor))
	  collect descriptor into pairs)))

(define-encoding :invokeinterface #xB9
  (destructuring-bind (class-name name type) operands
    (let* ((method-ref (make-interface-method-ref-info class-name name type))
	   (index (u2 (pool-index pool method-ref))))
      (append index (list (interface-count type) 0))))
  (let ((method-ref (aref pool (parse-u2 bytes))))
    (parse-u2 bytes) ; skip unused bytes
    (list (interface-method-ref-info-class-name method-ref)
	  (interface-method-ref-info-name method-ref)
	  (interface-method-ref-info-type method-ref))))

;; Like invokeinterface, invokedynamic has two unused bytes:
;;
;; invokedynamic
;; indexbyte1
;; indexbyte2
;; 0
;; 0
;;
;; Both must be zero and are reserved for future use by the JVM.

(define-encoding :invokedynamic #xBA
  (destructuring-bind (index name type) operands
    (let* ((dynamic-ref (make-invoke-dynamic-info index name type))
	   (index (u2 (pool-index pool dynamic-ref))))
      (append index '(0 0))))
  (let ((dynamic-ref (aref pool (parse-u2 bytes))))
    (parse-u2 bytes) ; ignore unused bytes
    (list (invoke-dynamic-info-bootstrap-index dynamic-ref)
	  (invoke-dynamic-info-name dynamic-ref)
	  (invoke-dynamic-info-type dynamic-ref))))

(fresh-dolist (class-instruction
	 '((#xBB :new)
	   (#xBD :anewarray)
	   (#xC0 :checkcast)
	   (#xC1 :instanceof)))
  (destructuring-bind (code instruction) class-instruction
    (define-encoding instruction code
      (let ((class-ref (make-class-info (first operands))))
	(u2 (pool-index pool class-ref)))
      (list (class-info-name (aref pool (parse-u2 bytes)))))))

(defparameter *wide-instructions*
  '((#x84 :iinc)
    (#x15 :iload)  (#x16 :lload)  (#x17 :fload)  (#x18 :dload)  (#x19 :aload)
    (#x36 :istore) (#x37 :lstore) (#x38 :fstore) (#x39 :dstore) (#x3A :astore)))

(define-encoding :wide #xC4
  (let ((opcode (loop for (op instruction) in *wide-instructions*
		      when (eq instruction (first operands))
			return op)))
     (cond
       ((null opcode)
	(error 'class-format-error
		 :message (format nil "Invalid wide instruction ~A" operands)))
       ((= opcode #x84)	; iinc
	(destructuring-bind (index const) (rest operands)
	  (cons opcode (append (u2 index) (u2 const)))))
       (t (cons opcode (u2 (second operands))))))
  (let* ((opcode (parse-u1 bytes))
	 (instruction (loop for (op instruction) in *wide-instructions*
			    when (= op opcode)
			      return instruction)))
    (cond
      ((eq instruction :iinc)
       (list instruction
	     (parse-u2 bytes)
	     (signed-16 (parse-u2 bytes))))
      (instruction
       (list instruction (parse-u2 bytes)))
      (t (error 'class-format-error
		:message (format nil "Unknown wide operand ~A" opcode))))))

(define-encoding :multianewarray #xC5
  (destructuring-bind (class-name dimensions) operands
    (let* ((class-ref (make-class-info class-name))
	   (index (u2 (pool-index pool class-ref))))
      (list index dimensions)))
  (list (class-info-name (aref pool (parse-u2 bytes)))
	(parse-u1 bytes)))

(define-encoding :goto-w #xC8
  (u4 (first operands))
  (list (signed-32 (parse-u4 bytes))))

(define-encoding :jsr-w #xC9
  (u4 (first operands))
  (list (signed-32 (parse-u4 bytes))))

(defun encode-instruction (instruction constant-pool offset)
  "Encodes a single bytecode instruction."
  (let ((encoder (gethash (if (listp instruction)
			      (first instruction)
			      instruction)
			  *bytecode-encoders*)))
    (cond
      ((functionp encoder)
       ;; (rest instruction) removes the instruction name, which is known
       (funcall encoder (rest instruction) constant-pool offset))
      ((consp encoder) encoder)
      (t (error 'class-format-error
		:message (format nil "Unknown instruction ~A" instruction))))))

(defun instruction-length (instruction constant-pool offset)
  "Gets the length of an instruction in bytes."
  (let ((op (first instruction)))
    (cond
      ((member op '(:ifeq :ifne :iflt :ifge :ifgt :ifle
		    :if-icmpeq :if-icmpne
		    :if-icmplt :if-icmpge
		    :if-icmpgt :if-icmple
		    :if-acmpeq :if-acmpne
		    :goto :jsr
		    :ifnull :ifnonnull)
	       :test 'eq)
       3) ; 2 byte offset + 1 byte opcode
      ((member op '(:goto-w :jsr-w)
	       :test 'eq)
       5) ; 4 byte offset + 1 byte opcode
      ((eq op :tableswitch)
       (destructuring-bind (default low high &rest offsets) (rest instruction)
	 ;; let the encoder do the work of calculating padding
	 (length (encode-instruction
		  ;; replace label offsets with dummy integer offsets
		  (list* default low high (loop for offset in offsets
						collect 0))
		  constant-pool
		  offset))))
      ((eq op :lookupswitch)
       (destructuring-bind (default &rest match-offset-pairs) (rest instruction)
	 ;; just like :tableswitch
	 (length (encode-instruction
		  (cons default (loop for (key offset) in match-offset-pairs
				      collect (list key 0)))
		  constant-pool
		  offset))))
      (t (length (encode-instruction instruction constant-pool offset))))))

(defun label-offsets (instructions)
  "Calculates the bytecode offsets for all labels."
  ;; make a dummy constant pool for use in offset calculation
  (let ((pool (make-constant-pool))
	(label-table (make-hash-table :test 'equal)))
    (loop for instruction in instructions
	  for offset = 0 then offset
	  if (eq (first instruction) :label)
	    do (setf (gethash (second instruction) label-table) offset)
	  else
	    do (incf offset (instruction-length instruction pool offset))
	  finally (return label-table))))

(defgeneric encode-bytecode (instructions constant-pool)
  (:documentation "Encodes bytecode instructions to a list of bytes.")
  (:method ((instructions array) constant-pool)
    (coerce instructions 'list))
  (:method ((instructions list) constant-pool)
    (loop for instruction in instructions
	  for offset = 0 then offset
	  for bytes = (encode-instruction instruction constant-pool offset)
	  do (incf offset (if (integerp bytes) 1 (length bytes)))
	  collect bytes into output
	  finally (return (flatten output)))))

(defun decode-bytecode (code-bytes constant-pool)
  (let ((bytecode-length (parse-u4 code-bytes))
	(start-index (class-bytes-index code-bytes)))
    (loop for offset = (- (class-bytes-index code-bytes) start-index)
	  while (< offset bytecode-length)
	  collect (let* ((opcode (parse-u1 code-bytes))
			 (decoder (aref *bytecode-decoders* opcode)))
		    (cond
		      ((keywordp decoder) decoder)
		      ((functionp decoder)
		       (funcall decoder code-bytes constant-pool offset))
		      (t (error 'class-format-error
				:message (format nil "Unknown instruction ~A" opcode))))))))

