(in-package #:jclass)

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
