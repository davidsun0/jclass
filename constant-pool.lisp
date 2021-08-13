(defpackage #:jclass
  (:use :cl))
(in-package #:jclass)

;;; Utility functions

(defun flatten (tree &key (remove-nil nil))
  (if remove-nil
      (labels ((flatten-remove (tree)
		 (cond ((null tree) '())
		       ((atom tree) (list tree))
		       (t (loop for subtree in tree
				if (listp subtree)
				  append (flatten-remove subtree)
				else
				  collect subtree)))))
	(flatten-remove tree))
      (labels ((flatten-keep (tree)
		 (if (atom tree)
		     (list tree)
		     (loop for subtree in tree
			   append (flatten-keep subtree)))))
	(flatten-keep tree))))

;;; Disassembly

(define-condition class-format-error (error)
  ((message :initarg :message
	    :accessor message))
  (:report (lambda (condition stream)
	     (format stream "~A" (message condition)))))

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

(defun encode-modified-utf8 (character)
  "Encodes a character as a list of modified UTF-8 bytes.
Assumes char-code returns the Unicode code point.
See Java Virtual Machine specification 4.4.7 CONSTANT_Utf8_info."
  (labels ((encode-code-point (code)
	     (cond
	       ((< 0 code #x80) (list code))
	       ((< code #x800)
		(list (logior #b11000000 (ash code -6))
		      (logior #b10000000 (logand #x3F code))))
	       ((< code #x10000)
		(list (logior #b11100000 (ash code -12))
		      (logior #b10000000 (logand #x3F (ash code -6)))
		      (logior #b10000000 (logand #x3F code))))
	       (t
		(let* ((code (- code #x10000))
		       (upper (ash code -10))
		       (lower (logand code #x3FF)))
		  ;; encode as UTF-16 surrogate pair
		  (concatenate 'list
			       (encode-code-point (+ #xD800 upper))
			       (encode-code-point (+ #xDC00 lower))))))))
    (encode-code-point (char-code character))))

(defun decode-modified-utf8 (bytes)
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
      (coerce output 'string))))

;; See *class-modifiers*, *method-modifiers*, *field-modifiers*, etc.
(defun access-modifiers (mod-list mod-map)
  (let ((flags (mapcar (lambda (x) (second (assoc x mod-map)))
		       mod-list)))
    (reduce #'logior flags)))

(defun access-flag-lookup (mod-list flags)
  (loop for modifier in mod-list
	when (not (zerop (logand (second modifier) flags)))
	  collect (first modifier)))

;;; Constant Pool

(defstruct constant-pool
  (size 0)
  ;; mapping from constants to pool index
  (table (make-hash-table :test 'equal)))

(defgeneric resolve-constant (pool constant-type &rest data)
  (:documentation "Recursively adds a constant struct's dependencies to a constant pool."))

(defgeneric constant-info-bytes (pool constant-type &rest data)
  (:documentation "Converts a constant struct to a list of bytes."))

(defun pool-index (pool constant &optional (insert t))
  "Inserts a constant into a constant pool."
  (let ((index (gethash constant (constant-pool-table pool))))
    (cond
      ;; optional constants use 0 to represent no constant
      ((null constant) 0)
      (index)
      (insert
       ;; each constant maps to its index in the pool
       (setf (gethash constant (constant-pool-table pool))
	     (incf (constant-pool-size pool)
		   ;; 8 byte constants take two slots
		   (if (or (eq (first constant) 'long-info)
			   (eq (first constant) 'double-info))
		       2 1))))
      (t (error "Constant ~A does not exist in the pool" constant)))))

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
  (let ((pool (gensym))
	(const-type (gensym)))
    `(progn
       ;; each struct is a list with the constant type as the first value
       ;; this is so constants can be (portably) used as hash table keys
       (defstruct (,name (:type list) :named
			 (:constructor ,(symbol-concatenate "MAKE-" name)
			   ,slots))
	 ,@slots)

       ;; resolve constant specializes on the constant type via
       ;; (apply pool #'resolve-constant constant-struct)
       (defmethod resolve-constant (,pool (,const-type (eql ',name)) &rest data)
	 (pool-index ,pool (cons ',name data))
	 ;; define u2-pool-index to resolve dependencies
	 (flet ((u2-pool-index (const)
		    (apply #'resolve-constant ,pool const)))
	   (declare (ignorable (function u2-pool-index)))
	   (destructuring-bind ,slots data
	     (declare (ignorable ,@slots))
	     ,@body)))

       (defmethod constant-info-bytes (,pool (,const-type (eql ',name)) &rest data)
	 ;; define u2-pool-index to get the dependency index
	 (flet ((u2-pool-index (const)
		  (u2 (pool-index ,pool const nil))))
	   (declare (ignorable (function u2-pool-index)))
	   (destructuring-bind ,slots data
	     (list ,tag ,@body)))))))

(def-jconstant utf8-info 1 (text)
  (let ((text-bytes (flatten (map 'list #'encode-modified-utf8 text))))
    (list (u2 (length text-bytes))
	  text-bytes)))

(def-jconstant integer-info 3 (value)
  (u4 value))

(def-jconstant float-info 4 (ieee-bits)
  (u4 ieee-bits))

;; pool-index implements long-info and double-info taking two pool slots
(def-jconstant long-info 5 (value)
  (u4 (ash value -32))
  (u4 value))

(def-jconstant double-info 6 (ieee-bits)
  (u4 (ash ieee-bits -32))
  (u4 ieee-bits))

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

;; Constatnt pool disassembly

(defun allocate-constant (bytes)
  ;; first pass over constant pool: split into entries
  (let ((tag (parse-u1 bytes)))
    (cons
     tag
     (case tag
       ((1)
	(let ((length (parse-u2 bytes)))
	  (list (parse-bytes length bytes))))
       ((3 4)
	(list (parse-u4 bytes)))
       ((5 6)
	(list (logior (ash (parse-u4 bytes) 32)
		      (parse-u4 bytes))))
       ((7 8 16 19 20)
	(list (parse-u2 bytes)))
       ((9 10 11 12 17 18)
	(list (parse-u2 bytes)
	      (parse-u2 bytes)))
       ((15)
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
	   ((4) (make-float-info   (second constant)))
	   ((5 6)
	    (destructuring-bind (high-bytes low-bytes) (rest constant)
	      (let ((value (logior (ash high-bytes 32) low-bytes)))
		(if (= tag 5)
		    (make-long-info   value)
		    (make-double-info value)))))
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
	 (pool (make-array (+ 2 size) :initial-element nil)))
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
	  do (format t "~A: ~A~%" i (build-constant pool i)))
    pool))

