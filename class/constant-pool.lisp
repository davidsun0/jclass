(in-package #:jclass)

;;; Utility

(defmacro with-gensyms (symbols &body body)
  `(let ,(loop for s in symbols collect `(,s (gensym)))
     ,@body))

(defun flatten (tree)
  "Flattens a tree into a list of leaves. Treats nil as an empty list, not an atom."
  (let ((output '()))
    (labels ((traverse (tree)
	       (cond
		 ((null tree)) ;; ignore nil
		 ((atom tree) (push tree output))
		 (t (map nil #'traverse tree)))))
      (traverse tree)
      (nreverse output))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symbol-concatenate (&rest values)
    (intern (format nil "~{~A~}" values))))

;;; Disassembly

(define-condition class-format-error (error)
  ((message :initarg :message
	    :accessor message))
  (:report (lambda (condition stream)
	     (format stream "~A" (message condition)))))

;; Buffer that represents the elements of array starting at index and ending
;; at array's fill pointer.
(defstruct class-bytes
  array
  index)

;;; Byte manipulation

(defun u1 (n)
  "Lists the bytes of a 1 byte integer."
  (list (logand #xFF n)))

(defun u2 (n)
  "Lists the bytes of a 2 byte integer in big endian."
  (list (logand #xFF (ash n -8))
	(logand #xFF n)))

(defun u4 (n)
  "Lists the bytes of a 4 byte integer in big endian."
  (list (logand #xFF (ash n -24))
	(logand #xFF (ash n -16))
	(logand #xFF (ash n -8))
	(logand #xFF n)))

(defun parse-bytes (count bytes)
  (with-slots (array index) bytes
    (if (< (- (length array) index) count)
	(error 'class-format-error
	       :message "Unexpected end of byte stream")
	(prog1
	  (make-array count
		      :displaced-to array
		      :displaced-index-offset index
		      :element-type (array-element-type array)
		      :fill-pointer count)
	  (incf index count)))))

(declaim (inline parse-u1 parse-u2 parse-u4))

(defun parse-u1 (bytes)
  (aref (parse-bytes 1 bytes) 0))

(defun parse-u2 (bytes)
  (let ((u2-bytes (parse-bytes 2 bytes)))
    (logior (ash (aref u2-bytes 0) 8)
	    (aref u2-bytes 1))))

(defun parse-u4 (bytes)
  (let ((u4-bytes (parse-bytes 4 bytes)))
    (logior (ash (aref u4-bytes 0) 24)
	    (ash (aref u4-bytes 1) 16)
	    (ash (aref u4-bytes 2) 8)
	    (aref u4-bytes 3))))

(declaim (inline signed-8 signed-16 signed-32))

(defun signed-8 (byte)
  "Converts an (unsigned-byte 8) to a (signed-byte 8) with the same bitstring."
  (if (> byte #.(1- (expt 2 7)))
      (- byte #.(expt 2 8))
      byte))

(defun signed-16 (short)
  "Converts an (unsigned-byte 16) to a (signed-byte 16) with the same bitstring."
  (if (> short #.(1- (expt 2 15)))
      (- short #.(expt 2 16))
      short))

(defun signed-32 (int)
  "Converts an (unsigned-byte 32) to a (signed-byte 32) with the same bitstring."
  (if (> int #.(1- (expt 2 31)))
      (- int #.(expt 2 32))
      int))

(defun encode-modified-utf8 (string)
  "Encodes a string as a list of modified UTF-8 bytes."
  ;; See JVM Spec 4.4.7 CONSTANT_Utf8_info
  ;; String is encoded into UTF-16, then re-encoded to UTF-8
  (let ((output '()))
    (labels ((encode-code-point (code)
	       (cond
		 ((< 0 code #x80)
		  (push code output))
		 ((< code #x800)
		  (push (logior #b11000000 (ash code -6)) output)
		  (push (logior #b10000000 (logand #x3F code)) output))
		 ((< code #x10000)
		  (push (logior #b11100000 (ash code -12)) output)
		  (push (logior #b10000000 (logand #x3F (ash code -6))) output)
		  (push (logior #b10000000 (logand #x3F code)) output))
		 ((< code #x110000)
		  (let* ((code (- code #x10000))
			 ;; Decompose into UTF-16 surrogate pairs
			 (upper (ash code -10))
			 (lower (logand code #x3FF)))
		    (encode-code-point (+ #xD800 upper))
		    (encode-code-point (+ #xDC00 lower))))
		 ;; Implementations that can infer this is unreachable code
		 #-(or sbcl)
		 (t (error "Invalid Unicode code point: ~A" code)))))
      (loop for code across string
	    do (encode-code-point (char-code code)))
      (nreverse output))))

(defun decode-modified-utf8 (bytes)
  "Decodes a sequence of modified UTF-8 bytes into a string."
  ;; See JVM Spec 4.4.7 CONSTANT_Utf8_info
  (let ((input (coerce bytes 'list))
	(output '()))
    (labels ((extract-bits (mask shift byte)
	       (assert (= mask (ash byte (- shift))) (byte)
		       'class-format-error
		       :message (format nil "Invalid UTF-8 byte ~A" byte))
	       (logand (1- (ash 1 shift)) byte))
	     (decode-utf8 ()
	       (let ((byte (pop input)))
		 (cond
		   ((and (zerop (ash byte -7))
			 (not (zerop byte)))
		    byte)
		   ((= (ash byte -5) #b110)
		    (logior (ash (extract-bits #b110 5 byte) 6)
			    (extract-bits #b10 6 (pop input))))
		   ((= (ash byte -4) #b1110)
		    (logior (ash (extract-bits #b1110 4 byte) 12)
			    (ash (extract-bits #b10 6 (pop input)) 6)
			    (extract-bits #b10 6 (pop input))))
		   (t (error 'class-format-error
			     :message (format nil "Invalid modified UTF-8 sequence ~A"
					      bytes)))))))
      (loop while input do
	(push
	 (let ((code-point (decode-utf8)))
	   (cond
	     ((or (< code-point #xD800) (<= #xE000 code-point))
	      (code-char code-point))
	     ((<= #xD800 code-point #xDBFF)
	      ;; decode UTF-16 surrogate pair
	      (let ((upper code-point)
		    (lower (decode-utf8)))
		(assert (<= #xDC00 lower #xDFFF) (lower)
			'class-format-error
			:message (format nil "Invalid surrogate pair ~A ~A" upper lower))
		(code-char (+ #x10000
			      (logior (ash (- upper #xD800) 10)
				      (- lower #xDC00))))))
	     (t (error 'class-format-error
		       :message (format nil "Invalid modified UTF-8 sequence ~A"
					bytes)))))
	 output))
      (coerce (nreverse output) 'string))))

;;; Constant Pool

(defstruct constant-pool
  (size 0)
  ;; mapping from constants to pool index
  (table (make-hash-table :test 'equal)))

(defgeneric resolve-constant (pool constant-type &rest data)
  (:documentation "Recursively adds a constant struct's dependencies to a constant pool."))

(defgeneric constant-info-bytes (pool constant-type &rest data)
  (:documentation "Converts a constant struct to a list of bytes."))

(defun pool-index (pool constant)
  "Looks up the index of the constant in the constant pool. Inserts the constant
first if it does not already exist in the pool."
  (let ((index (gethash constant (constant-pool-table pool))))
    (cond
      (index)
      (t
       (setf (gethash constant (constant-pool-table pool))
	     (incf (constant-pool-size pool)))
       ;; The slot following an 8 byte constant is empty.
       (when (or (eq (first constant) 'long-info)
		 (eq (first constant) 'double-info))
	 (incf (constant-pool-size pool)))))))

(defun constant-pool-bytes (pool)
  "Converts a constant pool to a (nested) list of bytes."
  ;; resolve all constants
  (let ((pool-table (constant-pool-table pool)))
    ;; don't modify the hash table while iterating over it
    (loop for key in (loop for k being the hash-keys of pool-table
			   collect k)
	  do (apply #'resolve-constant pool key))
    ;; sort constants by index
    (let* ((index-constants (loop for k being the hash-keys of pool-table
				    using (hash-value v)
				  collect (cons v k)))
	   (sorted-constants (sort index-constants #'< :key #'car))
	   (pool-items (mapcar #'cdr sorted-constants)))
      ;; convert to bytes; length is 1+ actual number of constants
      (list (u2 (1+ (constant-pool-size pool)))
	    (loop for p in pool-items
		  collect (apply #'constant-info-bytes pool p))))))

(defmacro def-jconstant (name tag slots &body body)
  (with-gensyms (pool const-type)
    `(progn
       ;; each struct is a list with the constant type as the first value
       ;; this is so constants can be (portably) used as hash table keys
       (defstruct (,name (:type list) :named
			 (:constructor ,(symbol-concatenate "MAKE-" name)
			   ,slots))
	 ,@slots)

       ;; resolve constant specializes on the constant type 
       ;; via (apply pool #'resolve-constant constant-struct)
       (defmethod resolve-constant (,pool (,const-type (eql ',name)) &rest data)
	 (pool-index ,pool (cons ',name data))
	 ;; locally define u2-pool-index to resolve dependencies
	 (flet ((u2-pool-index (const)
		    (apply #'resolve-constant ,pool const)))
	   (declare (ignorable (function u2-pool-index)))
	   (destructuring-bind ,slots data
	     (declare (ignorable ,@slots))
	     ,@body)))

       (defmethod constant-info-bytes (,pool (,const-type (eql ',name)) &rest data)
	 ;; locally define u2-pool-index to get the dependency index
	 (flet ((u2-pool-index (const)
		  (u2 (pool-index ,pool const))))
	   (declare (ignorable (function u2-pool-index)))
	   (destructuring-bind ,slots data
	     (list ,tag ,@body)))))))

(def-jconstant utf8-info 1 (text)
  (let ((text-bytes (encode-modified-utf8 text)))
    (list (u2 (length text-bytes))
	  text-bytes)))

(def-jconstant integer-info 3 (value)
  (u4 value))

(def-jconstant float-info 4 (value)
  (u4 (float-features:single-float-bits value)))

;; pool-index and parse-constant-pool implement the fact that
;; long-info and double-info take two pool slots
(def-jconstant long-info 5 (value)
  (list
   (u4 (ash value -32))
   (u4 value)))

(def-jconstant double-info 6 (value)
  (let ((ieee-bits (float-features:double-float-bits value)))
    (list
     (u4 (ash ieee-bits -32))
     (u4 ieee-bits))))

(def-jconstant class-info 7 (name)
  (u2-pool-index (make-utf8-info name)))

(def-jconstant string-info 8 (text)
  (u2-pool-index (make-utf8-info text)))

(def-jconstant field-ref-info 9 (class-name name type)
  (u2-pool-index (make-class-info class-name))
  (u2-pool-index (make-name-and-type-info name type)))

(def-jconstant method-ref-info 10 (class-name name type)
  (u2-pool-index (make-class-info class-name))
  (u2-pool-index (make-name-and-type-info name type)))

(def-jconstant interface-method-ref-info 11 (class-name name type)
  (u2-pool-index (make-class-info class-name))
  (u2-pool-index (make-name-and-type-info name type)))

(def-jconstant name-and-type-info 12 (name type)
  (u2-pool-index (make-utf8-info name))
  (u2-pool-index (make-utf8-info type)))

(def-jconstant method-handle-info 15 (kind reference)
  (u1 kind)
  (u2-pool-index reference))

(def-jconstant method-type-info 16 (descriptor)
  (u2-pool-index (make-utf8-info descriptor)))

(def-jconstant dynamic-info 17 (bootstrap-index name type)
  (u2 bootstrap-index)
  (u2-pool-index (make-name-and-type-info name type)))

(def-jconstant invoke-dynamic-info 18 (bootstrap-index name type)
  (u2 bootstrap-index)
  (u2-pool-index (make-name-and-type-info name type)))

(def-jconstant module-info 19 (name)
  (u2-pool-index (make-utf8-info name)))

(def-jconstant package-info 20 (name)
  (u2-pool-index (make-utf8-info name)))

;; Constant pool disassembly

(defun allocate-constant (bytes)
  ;; first pass over constant pool: split into entries
  (let ((tag (parse-u1 bytes)))
    (cons
     tag
     (case tag
       ((1) ; UTF8
	(let ((length (parse-u2 bytes)))
	  (list (parse-bytes length bytes))))
       ((3 4) ; integer, float
	(list (parse-u4 bytes)))
       ((5 6) ; long, double
	(list (logior (ash (parse-u4 bytes) 32)
		      (parse-u4 bytes))))
       ((7 8 16 19 20) ; info types with one reference
	(list (parse-u2 bytes)))
       ((9 10 11 12 17 18) ; info types with two references
	(list (parse-u2 bytes)
	      (parse-u2 bytes)))
       ((15) ; method handle
	(list (parse-u1 bytes)
	      (parse-u2 bytes)))
       (t (error 'class-format-error
		 :message (format nil "Unknown tag ~A in constant pool" tag)))))))

(defun build-constant (pool index)
  ;; second pass over constant pool: build constants and resolve dependencies
  (let* ((constant (aref pool index))
	 (tag (first constant)))
    (if (symbolp tag) ; already built constants begin with symbols
	constant
	(setf
	 (aref pool index)
	 (case tag
	   ((1) (make-utf8-info    (decode-modified-utf8 (second constant))))
	   ((3) (make-integer-info (second constant)))
	   ((4) (make-float-info
		 (float-features:bits-single-float (second constant))))
	   ((5) (make-long-info    (second constant)))
	   ((6) (make-double-info
		 (float-features:bits-double-float (second constant))))
	   ((7 8 16 19 20)
	    (let* ((utf8-index (second constant))
		   (utf8-constant (build-constant pool utf8-index))
		   (text (utf8-info-text utf8-constant)))
	      (case tag
		((7)  (make-class-info       text))
		((8)  (make-string-info      text))
		((16) (make-method-type-info text))
		((19) (make-module-info      text))
		((20) (make-package-info     text)))))
	   ((9 10 11)
	    (destructuring-bind (class-index name-type-index) (rest constant)
	      (let* ((class-constant (build-constant pool class-index))
		     (class-name (class-info-name class-constant))
		     (name-type (build-constant pool name-type-index))
		     (name (name-and-type-info-name name-type))
		     (type (name-and-type-info-type name-type)))
		(case tag
		  ((9)  (make-field-ref-info            class-name name type))
		  ((10) (make-method-ref-info           class-name name type))
		  ((11) (make-interface-method-ref-info class-name name type))))))
	   ((12)
	    (destructuring-bind (name-index type-index) (rest constant)
	      (let ((name-utf8 (build-constant pool name-index))
		    (type-utf8 (build-constant pool type-index)))
		(make-name-and-type-info (utf8-info-text name-utf8)
					 (utf8-info-text type-utf8)))))
	   ((15)
	    (destructuring-bind (kind reference) (rest constant)
	      (make-method-handle-info kind (build-constant pool reference))))
	   ((17 18)
	    (destructuring-bind (bootstrap-index name-type-index) (rest constant)
	      (let* ((name-type (build-constant pool name-type-index))
		     (name (name-and-type-info-name name-type))
		     (type (name-and-type-info-type name-type)))
		(if (= tag 17)
		    (make-dynamic-info        bootstrap-index name type)
		    (make-invoke-dynamic-info bootstrap-index name type)))))
	   (t constant))))))

(defun parse-constant-pool (bytes)
  ;; constants are 1-indexed AND the size is 1 more than the actual count
  (let* ((size (1- (parse-u2 bytes)))
	 (pool (make-array (1+ size) :initial-element nil)))
    (loop for i from 1 upto size do
      (setf (aref pool i)
	    (let* ((constant (allocate-constant bytes))
		   (tag (first constant)))
	      ;; increment if long or double
	      (when (or (= tag 5) (= tag 6))
		(incf i))
	      constant)))
    ;; parse dependencies
    (loop for i from 1 upto size
	  do (build-constant pool i))
    pool))
