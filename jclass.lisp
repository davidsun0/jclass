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

(defun access-modifiers (mod-list mod-map)
  (let ((flags (mapcar (lambda (x) (second (assoc x mod-map)))
		       mod-list)))
    (reduce #'logior flags)))

(defparameter *inner-class-modifiers*
  '((:public       #x0001)
    (:private      #x0002)
    (:protected    #x0004)
    (:static       #x0008)
    (:final        #x0010)
    (:interface    #x0200)
    (:abstract     #x0400)
    (:synthetic    #x1000)
    (:annotation   #x2000)
    (:enum         #x4000)))

(defparameter *class-modifiers*
  '((:public       #x0001)
    (:final        #x0010)
    (:super        #x0020)
    (:interface    #x0200)
    (:abstract     #x0400)
    (:synthetic    #x1000)
    (:annotation   #x2000)
    (:enum         #x4000)
    (:module       #x8000)))

(defparameter *method-modifiers*
  '((:public       #x0001)
    (:private      #x0002)
    (:protected    #x0004)
    (:static       #x0008)
    (:final        #x0010)
    (:synchronized #x0020)
    (:bridge       #x0040)
    (:varargs      #x0080)
    (:native       #x0100)
    (:abstract     #x0400)
    (:strict       #x0800)
    (:synthetic    #x1000)))

(defparameter *field-modifiers*
  '((:public       #x0001)
    (:private      #x0002)
    (:protected    #x0004)
    (:static       #x0008)
    (:final        #x0010)
    (:volatile     #x0040)
    (:transient    #x0080)
    (:synthetic    #x1000)
    (:enum         #x4000)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symbol-concatenate (&rest values)
    (intern (format nil "~{~A~}" values))))

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
  (u2 (length text))
  ;; todo: add support for UTF-8, which Java does
  ;; note the special encoding for u+0000, which is #xC080
  (map 'list #'char-code text))

(def-jconstant integer-info 3 (value)
  (u4 value))

(def-jconstant float-info 4 (ieee-bits)
  (u4 ieee-bits))

;; the fact that long-info and double-info take two slots is described in pool-index
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

(defgeneric byte-list (pool struct)
  (:documentation "Constructs the binary form of a JVM structure."))

(defmacro def-jstruct (name slots &body body)
  (let ((struct-obj (gensym))
	(pool (gensym)))
    `(progn
       (defstruct (,name (:constructor ,(symbol-concatenate "MAKE-" name) ,slots))
	 ,@slots)
       (defmethod byte-list (,pool (,struct-obj ,name))
	 (flet ((u2-pool-index (const)
		  (u2 (pool-index ,pool const)))
		(constant-pool () ,pool)
		(substructs (sub-list)
		  ;;Inserts a list of structures into the outer structure
		  (mapcar (lambda (f) (byte-list f ,pool))
			  sublist)))
	   (declare (ignorable (function u2-pool-index)
			       (function constant-pool)
			       (function substructs)))
	   (with-slots ,slots ,struct-obj
	     (list ,@body)))))))

(defmacro inner-structs (name slots &body body)
  "Defines an anonymous inner struct which is built by destructuring a list."
  (let ((inner-form (gensym)))
    `(mapcar (lambda (,inner-form)
	       (destructuring-bind ,slots ,inner-form
		 (list ,@body)))
	     ,name)))

;;; Attribute structures

(defmacro def-attribute (name name-string slots &body body)
  `(def-jstruct ,name ,slots
     ;; all attributes have this structure: u2 name, u4 length, bytes
     (u2-pool-index (make-utf8-info ,name-string))
     (let ((body-bytes (flatten (list ,@body) :remove-nil t)))
       (list (u4 (length body-bytes))
	     body-bytes))))

(def-attribute bootstrap-methods "BootstrapMethods" (methods)
  (u2 (length methods))
  (inner-structs methods (method-ref arguments)
    (u2-pool-index method-ref)
    (u2 (length arguments))
    (substructs arguments)))

(def-attribute inner-classes "InnerClasses" (classes)
  (u2 (length classes))
  (inner-structs classes (inner-class outer-class name flags)
    (u2-pool-index inner-class)
    (u2-pool-index outer-class)
    (u2-pool-index name)
    (u2 (access-modifiers flags *inner-class-modifiers*))))

;;; Fields, Methods, and Classes

(def-jstruct method-info (flags name descriptor attributes)
  (u2 (access-modifiers flags *method-modifiers*))
  (u2-pool-index (make-utf8-info name))
  (u2-pool-index (make-utf8-info descriptor))
  (u2 (length attributes))
  (substructs attributes))

(def-jstruct field-info (flags name descriptor attributes)
  (u2 (access-modifiers flags *field-modifiers*))
  (u2-pool-index (make-utf8-info name))
  (u2-pool-index (make-utf8-info descriptor))
  (u2 (length attributes))
  (substructs attributes))

(def-jstruct java-class
    (major-version minor-version flags name parent interfaces fields methods attributes)
  (u2 minor-version)
  (u2 major-version)
  (u2 (access-modifiers flags *class-modifiers*))
  (u2-pool-index (make-class-info name))
  (u2-pool-index (make-class-info parent))
  (u2 (length interfaces))
  (substructs interfaces)
  (u2 (length fields))
  (substructs fields)
  (u2 (length methods))
  (substructs methods)
  (u2 (length attributes))
  (substructs attributes))

;; Java version

(defun java-class-bytes (java-class)
  (let* ((pool (make-constant-pool))
	 (bytes (byte-list pool java-class)))
    (flatten (list
	      (u4 #xCAFEBABE)		; magic number
	      (subseq bytes 0 4)	; class version
	      (constant-pool-bytes pool)
	      (subseq bytes 4))
	     :remove-nil t)))
