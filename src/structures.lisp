(in-package #:jclass)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *serializers*
    (make-hash-table :test 'eq))

  (defparameter *deserializers*
    (make-hash-table :test 'eq))

  (defun expand-serializer (form)
    (if (symbolp form)
	form
	(let ((serial-fn (gethash (car form) *serializers*)))
	  (if serial-fn
	      (funcall serial-fn (cdr form))
	      (error "Can't find serializer for ~A" form)))))

  (defun expand-deserializer (form &rest args)
    (if (symbolp form)
	(destructuring-bind (value) args
	  `(setf ,form ,value))
	(let ((deserial-fn (gethash (car form) *deserializers*)))
	  (if deserial-fn
	      (apply deserial-fn (cdr form) args)
	      (error "Can't find deserializer for ~A: ~A" form args)))))

  (defmacro def-serialization (name
			       lambda-list
			       deserialization-input
			       serialization-body
			       deserialization-body)
    (with-gensyms (argument)
      `(progn
	 (setf (gethash ',name *serializers*)
	       (lambda (,argument)
		 (destructuring-bind ,lambda-list ,argument
		   ,serialization-body)))
	 (setf (gethash ',name *deserializers*)
	       (lambda (&rest ,argument)
		 (destructuring-bind (,lambda-list ,deserialization-input)
		     ,argument
		   ,deserialization-body))))))

  (def-serialization with-length (unit list fields &rest body) stream
    (with-gensyms (term)
      `(list
	(,unit (length ,list))
	(mapcar (lambda (&rest ,term)
		  (destructuring-bind (,fields) ,term
		    (list ,@(mapcar #'expand-serializer body))))
		,list)))
    (with-gensyms (item-count)
      `(let ((,item-count ,(list (ccase unit
				   (u1 'parse-u1)
				   (u2 'parse-u2)
				   (u4 'parse-u4))
				 stream)))
	 (setf ,list
	       (loop repeat ,item-count collect
		     (let ,(if (symbolp fields) `(,fields) fields)
		       ,@(loop for form in body
			       collect (expand-deserializer form stream))
		       ,(if (symbolp fields) fields `(list ,@fields))))))))

  ;;; Byte manipulation

  (def-serialization u1 (value) byte-stream
    `(u1 ,(expand-serializer value))
    (expand-deserializer value `(parse-u1 ,byte-stream)))
  
  (def-serialization u2 (value) byte-stream
    `(u2 ,(expand-serializer value))
    (expand-deserializer value `(parse-u2 ,byte-stream)))

  (def-serialization u4 (value) byte-stream
    `(u4 ,(expand-serializer value))
    (expand-deserializer value `(parse-u4 ,byte-stream)))

  ;;; Constant pool references

  ;; utf8-info and class-info constants may be optional.
  ;; When nil is supplied, use pool-index zero.
  ;;
  ;; As of Java 17:
  ;;
  ;; Places where class-info is optional:
  ;; - super-class in java-class for java/lang/Object
  ;; - catch-type in Code for catching all throwables
  ;; - outer-class-index in InnerClasses for anonymous inner classes
  ;;
  ;; Places where utf8-info is optional:
  ;; - inner-name-index in InnerClasses for anonymous inner classes
  ;; - name-index in MethodParameters for anonymous parameters
  ;; - module-version-index in Module for no version information
  ;; - requires-version-index in Module for no version information

  (def-serialization utf8-info (text) utf8-index
    ;; optional utf8-info if text is nil
    `(if ,text (pool-index (make-utf8-info, text)) 0)
    `(setf ,text (utf8-info-text (aref (pool-array) ,utf8-index))))

  (def-serialization class-info (name) class-index
    ;; optional class-info if name is nil
    `(if ,name (pool-index (make-class-info ,name)) 0)
    `(setf ,name (class-info-name (aref (pool-array) ,class-index))))

  (def-serialization name-and-type-info (name type) nti-index
    `(pool-index (make-name-and-type-info ,name ,type))
    (with-gensyms (info-ref)
      `(let ((,info-ref (aref (pool-array) ,nti-index)))
	 (setf ,name (name-and-type-info-name ,info-ref))
	 (setf ,type (name-and-type-info-type ,info-ref)))))

  (def-serialization method-handle-info (kind reference) mhi-index
    `(pool-index (make-method-handle-info ,kind ,reference))
    (with-gensyms (handle-ref)
      `(let ((,handle-ref (aref (pool-array) ,mhi-index)))
	 (setf ,kind      (method-handle-info-kind      ,handle-ref))
	 (setf ,reference (method-handle-info-reference ,handle-ref)))))

  (def-serialization module-info (name) module-index
    `(pool-index (make-module-info ,name))
    `(setf ,name (make-module-info (aref (pool-array) ,module-index))))

  (def-serialization package-info (name) package-index
    `(pool-index (make-package-info ,name))
    `(setf ,name (make-package-info (aref (pool-array) ,package-index))))

  ;;; Special (de)serialization functions

  (def-serialization access-modifiers (flags modifier-list) bytes
    `(access-modifiers ,flags ,modifier-list)
    `(setf ,flags (access-flag-lookup ,bytes ,modifier-list)))

  (def-serialization modified-utf8 (string) byte-stream
    `(encode-modified-utf8 ,string)
    (with-gensyms (byte-list)
      `(setf ,string
	     (let ((,byte-list ,byte-stream)) ; prevent double evaluation
	       (decode-modified-utf8
		(parse-bytes (- (length (class-bytes-array ,byte-list))
				(class-bytes-index ,byte-list))
			     ,byte-list))))))

  ;;; Pool index with unspecified type
  (def-serialization pool-index (constant) index
    `(pool-index ,constant)
    `(setf ,constant (aref (pool-array) ,index)))

  ;;; Structures

  (def-serialization field (field) byte-stream
    `(serialize (constant-pool) ,field)
    `(setf ,field (parse-field-info ,byte-stream (pool-array))))
  
  (def-serialization method (method) byte-stream
    `(serialize (constant-pool) ,method)
    `(setf ,method (parse-method-info ,byte-stream (pool-array))))

  (def-serialization attribute (attribute) byte-stream
    `(serialize (constant-pool) ,attribute)
    `(setf ,attribute (parse-attribute ,byte-stream (pool-array))))

  ) ; end eval-when

(defclass java-structure ()
  ()
  (:documentation "Data structure of a Java class file"))

(defclass attribute (java-structure)
  ()
  (:documentation "Java-structure attribute data"))

(defgeneric serialize (pool struct)
  (:documentation "Formats a structure into a list of bytes."))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun slot-definition (name)
    `(,(symbol-concatenate name '%)
      :initarg ,(intern (symbol-name name)
			(find-package :keyword))
      :accessor ,name))

  (defun serializer-body (pool object slots body)
    `(flet ((constant-pool () ,pool)
	    (pool-index (constant)
	      (pool-index ,pool constant)))
       (declare (ignorable (function constant-pool)
			   (function pool-index)))
       (with-accessors ,(mapcar (lambda (x) (list x x)) slots)
	   ,object
	 (list ,@(mapcar #'expand-serializer body)))))

  (defun deserializer-body (name byte-stream pool-array slots body)
    `(flet ((pool-array () ,pool-array))
       (declare (ignorable (function pool-array)))
       (let ,slots
	 ,@(mapcar
	    (lambda (form)
	      (expand-deserializer form byte-stream))
	    body)
	 (make-instance
	  ',name
	  ,@(loop for slot in slots
		  collect (intern (symbol-name slot)
				  (find-package :keyword))
		  collect slot)))))

  ) ; end eval-when

(defmacro def-jstruct (name slots &body structure)
  (with-gensyms (pool byte-stream struct-obj)
    `(progn
       (defclass ,name (java-structure)
	 ,(mapcar #'slot-definition slots))

       (defmethod serialize (,pool (,struct-obj ,name))
	 ,(serializer-body pool struct-obj slots structure))

       (defun ,(symbol-concatenate 'parse- name) (,byte-stream ,pool)
	 ,(deserializer-body name byte-stream pool slots structure)))))

(defgeneric attribute-name (attribute)
  (:documentation "Reader for the JVM specified attribute name string.")
  (:method (object)
    (error "Object ~A does not have an attribute name." object)))

(defmethod serialize :around (pool (attribute attribute))
  ;; Common header for all attributes
  (let ((body (flatten (call-next-method))))
    (list
     ;; Every attribute has a header containing the name and length in bytes.
     (u2 (pool-index pool (make-utf8-info (attribute-name attribute))))
     (u4 (length body))
     body)))

(defmacro def-attribute (name name-string slots &body structure)
  (with-gensyms (pool byte-stream struct-obj)
    `(progn
       (defclass ,name (attribute)
	 ((attribute-name%
	   :initform ,name-string
	   :reader attribute-name
	   :allocation :class)
	  ,@(mapcar #'slot-definition slots)))

       (defmethod serialize (,pool (,struct-obj ,name))
	 ,(serializer-body pool struct-obj slots structure))

       (setf (gethash ,name-string *attribute-parsers*)
	     (lambda (,byte-stream ,pool)
	       (declare (ignorable ,byte-stream))
	       ,(deserializer-body name byte-stream pool slots structure))))))

(defparameter *attribute-parsers*
  (make-hash-table :test 'equal))

(defun parse-attribute (bytes pool-array)
  (let* ((name-index (parse-u2 bytes))
	 (name       (aref pool-array name-index))
	 (length     (parse-u4 bytes))
	 (body       (parse-bytes length bytes)))
    ;; this is the outer struct / attribute's problem
    (assert (utf8-info-p name) (name-index)
	    'class-format-error
	    :message "Attribute name is not a UTF-8 constant")
    (let* ((error-symbol (gensym))
	   (name-string (utf8-info-text name))
	   (default-parser (lambda (bytes pool-array)
			     (declare (ignore bytes pool-array))
			     (error 'class-format-error
				    :message (format nil "Unknown attribute ~A"
						     (utf8-info-text name)))))
	   (parser (gethash name-string *attribute-parsers* default-parser)))
      (restart-case
	  ;; return results of normal parsing or raw bytes from restart
	  (catch error-symbol
	    (funcall parser
		     (make-class-bytes :array body :index 0)
		     pool-array))
	(skip-attribute ()
	  :report "Use the attribute's raw byte array"
	  ;; abandon parsing and throw raw bytes
	  (throw error-symbol
	    (list name-string body)))))))

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

(def-jstruct field-info (flags name descriptor attributes)
  (u2 (access-modifiers flags *field-modifiers*))
  (u2 (utf8-info name))
  (u2 (utf8-info descriptor))
  (with-length u2 attributes attribute
    (attribute attribute)))

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

(def-jstruct method-info (flags name descriptor attributes)
  (u2 (access-modifiers flags *method-modifiers*))
  (u2 (utf8-info name))
  (u2 (utf8-info descriptor))
  (with-length u2 attributes attribute
    (attribute attribute)))

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

(def-jstruct java-class
    (minor-version major-version flags name parent interfaces fields methods attributes)
  ;; handle version number independently for easy constant pool manipulation
  (u2 (access-modifiers flags *class-modifiers*))
  (u2 (class-info name))
  (u2 (class-info parent))
  (with-length u2 interfaces interface
    (u2 (class-info interface)))
  (with-length u2 fields field
    (field field))
  (with-length u2 methods method
    (method method))
  (with-length u2 attributes attribute
    (attribute attribute)))

;;; Attributes by JVM Specification order

(def-attribute constant-value "ConstantValue" (value)
  (u2 (pool-index value)))

;; StackMapTable serialization

(defun verification-bytes (verification constant-pool)
  (cond
    ((eq verification :top)     0)
    ((eq verification :integer) 1)
    ((eq verification :float)   2)
    ((eq verification :long)    3)
    ((eq verification :double)  4)
    ((eq verification :null)    5)
    ((eq verification :uninitialized-this) 6)
    ((stringp verification)
     ;; object
     (let* ((class-info (make-class-info verification))
	    (index (pool-index constant-pool class-info)))
       (cons 7 (u2 index))))
    ((integerp verification)
     ;; uninitialized variable
     (cons 8 (u2 verification)))
    (t (error 'class-format-error
	      :message (format nil "Invalid verification info ~A" verification)))))

(defun parse-verification (byte-stream pool-array)
  (let ((tag (parse-u1 byte-stream)))
    (case tag
      ((0) :top)
      ((1) :integer)
      ((2) :float)
      ((3) :long)
      ((4) :double)
      ((5) :null)
      ((6) :uninitialized-this)
      ((7) (let* ((index (parse-u2 byte-stream))
		  (class-info (aref pool-array index)))
	     (class-info-name class-info)))
      ((8) (parse-u2 byte-stream))
      (t (error 'class-format-error
		:message (format nil "Unknown verification_type_info tag ~A" tag))))))

(defun stack-map-frame-bytes (frame constant-pool)
  (let ((type (first frame)))
    (cond
      ;; same frame
      ((<= 0 type 63) type)
      ;; same locals 1 stack item frame
      ((<= 64 type 127)
       (list type
	     (verification-bytes (second frame) constant-pool)))
      ;; same locals 1 stack item frame extended
      ((= type 247)
       (destructuring-bind (offset verification) (rest frame)
	 (list type
	       (u2 offset)
	       (verification-bytes verification constant-pool))))
      ;; chop frame, same frame extended
      ((<= 248 type 251)
       (list type
	     (u2 (second frame))))
      ;; append frame
      ((<= 252 type 254)
       (destructuring-bind (offset &rest verifications) (rest frame)
	 (assert (= (length verifications)
		    (- type 251))
		 (verifications)
		 'class-format-error
		 :message "StackMapFrame has incorrect number of verification infos")
	 (list* type
		(u2 offset)
		(loop for v in verifications
		      collect (verification-bytes v constant-pool)))))
      ;; full frame
      ((= type 255)
       (destructuring-bind (offset local-count locals stack-count stacks)
	   (rest frame)
	 (assert (and (= (length locals) local-count)
		      (= (length stacks) stack-count))
		 (locals stacks)
		 'class-format-error
		 :message "StackMapFrame has incorrect number of verification infos")
	 (list
	  type
	  (u2 offset)
	  (u2 (length locals))
	  (loop for l in locals
		collect (verification-bytes l constant-pool))
	  (u2 (length stacks))
	  (loop for s in stacks
		collect (verification-bytes s constant-pool))))))))

(defun parse-stack-map-frame (byte-stream pool-array)
  (let ((type (parse-u1 byte-stream)))
    (cond
      ;; same frame
      ((<= 0 type 63)
       (list type))
      ;; same locals 1 stack item frame
      ((<= 64 type 127)
       (list type
	     (parse-verification byte-stream pool-array)))
      ;; same locals 1 stack item frame extended
      ((= type 247)
       (list type
	     (parse-u2 byte-stream)
	     (parse-verification byte-stream pool-array)))
      ;; chop frame, same frame extended
      ((<= 248 type 251)
       (list type
	     (parse-u2 byte-stream)))
      ;; append frame
      ((<= 252 type 254)
       (list* type
	      (parse-u2 byte-stream)
	      (loop repeat (- type 251)
		    collect (parse-verification byte-stream pool-array))))
      ((= type 255)
       (list (parse-u2 byte-stream)
	     (let ((local-count (parse-u2 byte-stream)))
	       (loop repeat local-count
		     collect (parse-verification byte-stream pool-array)))
	     (let ((stack-count (parse-u2 byte-stream)))
	       (loop repeat stack-count
		     collect (parse-verification byte-stream pool-array)))))
      (t
       (error 'class-format-error
	      :message (format nil "Invalid StackMapFrame type ~A" type))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-serialization stack-map-frame (frame) byte-stream
    `(stack-map-frame-bytes ,frame (constant-pool))
    `(setf ,frame (parse-stack-map-frame ,byte-stream (pool-array)))))

(def-attribute stack-map-table "StackMapTable" (entries)
  (with-length u2 entries frame
    (stack-map-frame frame)))

(def-attribute exceptions "Exceptions" (exceptions)
  (with-length u2 exceptions ex
    (u2 (class-info ex))))

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

(def-attribute inner-classes "InnerClasses" (classes)
  (with-length u2 classes (inner outer name access)
    (u2 (class-info inner))
    (u2 (class-info outer))
    (u2 (utf8-info name))
    (u2 (access-modifiers *inner-class-modifiers* access))))

(def-attribute enclosing-method "EnclosingMethod" (class name type)
  (u2 (class-info class))
  (u2 (name-and-type-info name type)))

(def-attribute synthetic "Synthetic" ())

(def-attribute signature "Signature" (signature)
  (u2 (utf8-info signature)))

(def-attribute source-file "SourceFile" (name)
  (u2 (utf8-info name)))

(def-attribute source-debug-extension "SourceDebugExtension" (debug)
  (modified-utf8 debug))

(def-attribute line-number-table "LineNumberTable" (line-numbers)
  (with-length u2 line-numbers (start-pc line-number)
    (u2 start-pc)
    (u2 line-number)))

(def-attribute local-variable-table "LocalVariableTable" (local-variables)
  (with-length u2 local-variables (start-pc length name descriptor index)
    (u2 start-pc)
    (u2 length)
    (u2 (utf8-info name))
    (u2 (utf8-info descriptor))
    (u2 index)))

(def-attribute local-variable-type-table "LocalVariableTypeTable" (local-variables)
  (with-length u2 local-variables (start-pc length name signature index)
    (u2 start-pc)
    (u2 length)
    (u2 (utf8-info name))
    (u2 (utf8-info signature))
    (u2 index)))

(def-attribute deprecated "Deprecated" ())

;; Annotation serialization

(defun write-element-value (tag value pool)
  (list
   (u1 (char-code tag))
   (ccase tag
     ((#\B #\I #\S #\Z)
      (u2 (pool-index pool (make-integer-info value))))
     (#\C (u2 (pool-index pool (make-integer-info (char-code value)))))
     (#\D (u2 (pool-index pool (make-double-info value))))
     (#\F (u2 (pool-index pool (make-float-info value))))
     (#\L (u2 (pool-index pool (make-long-info value))))
     (#\s (u2 (pool-index pool (make-utf8-info value))))
     (#\e (destructuring-bind (type const) value
	    (list
	     (u2 (pool-index pool (make-utf8-info type)))
	     (u2 (pool-index pool (make-utf8-info const))))))
     (#\@ (serialize value pool))
     (#\[ (let ((values value))
	    (list
	     (u2 (length values))
	     (loop for (tag value) in values
		   collect (write-element-value tag value pool))))))))

(defun parse-element-value (byte-stream pool-array)
  (let ((tag (code-char (parse-u1 byte-stream)))
	(value))
    (flet ((pool-lookup ()
	     (aref pool-array (parse-u2 byte-stream))))
      (setf value
	    (ccase tag
	      ((#\B #\I #\S #\Z)
	       (integer-info-value (pool-lookup)))
	      (#\C (code-char (integer-info-value (pool-lookup))))
	      (#\D (double-info-ieee-bits (pool-lookup)))
	      (#\F (float-info-ieee-bits (pool-lookup)))
	      (#\J (long-info-value (pool-lookup)))
	      (#\s (string-info-text (pool-lookup)))
	      (#\e (list
		    (utf8-info-text (pool-lookup))
		    (utf8-info-text (pool-lookup))))
	      (#\@ (parse-annotation byte-stream pool-array))
	      (#\[ (let ((count (parse-u2 byte-stream)))
		     (loop repeat count
			   collect (parse-element-value byte-stream pool-array))))))
      (list tag value))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-serialization element-value (tag value) byte-stream
    `(write-element-value ,tag ,value (constant-pool))
    `(destructuring-bind (tag% value%)
	 (parse-element-value ,byte-stream (pool-array))
       (setf ,tag tag%
	     ,value value%)))

  (def-jstruct annotation (type element-value-pairs)
    (u2 (utf8-info type))
    (with-length u2 element-value-pairs (name tag value)
      (u2 (utf8-info name))
      (element-value tag value)))

  (def-serialization annotation (annotation) byte-stream
    `(serialize ,annotation (constant-pool))
    `(setf ,annotation (parse-annotation ,byte-stream (pool-array)))))

(def-attribute runtime-visible-annotations
    "RuntimeVisibleAnnotations"
    (annotations)
  (with-length u2 annotations annotation
    (annotation annotation)))

(def-attribute runtime-invisible-annotations
    "RuntimeInvisibleAnnotations"
    (annotations)
  (with-length u2 annotations annotation
    (annotation annotation)))

(def-attribute runtime-visible-parameter-annotations
    "RuntimeVisibleParameterAnnotations"
    (parameters)
  (with-length u1 parameters annotations
    (with-length u2 annotations annotation
      (annotation annotation))))

(def-attribute runtime-invisible-parameter-annotations
    "RuntimeInvisibileParameterAnnotations"
    (parameters)
  (with-length u1 parameters annotations
    (with-length u2 annotations annotation
      (annotation annotation))))

(defun target-bytes (target-type target-info)
  (cons
   target-type
   (case target-type
     ;; empty target
     ((#x13 #x14 #x15) '())
     ;; type parameter target, formal parameter target
     ((#x00 #x01 #x16)
      (destructuring-bind (index) target-info
	(u1 index)))
     ;; supertype, throws target, catch target, offset target
     ((#x10 #x17 #x42 #x43 #x44 #x45)
      (destructuring-bind (index) target-info
	(u2 index)))
     ;; type parameter bound target
     ((#x11 #x12)
      (destructuring-bind (type-parameter-index bound-index) target-info
	(cons (u1 type-parameter-index)
	      (u1 bound-index))))
     ;; localvar target
     ((#x40 #x41)
      (macrolet ((with-length (&whole form &rest rest)
		   (declare (ignore rest))
		   `,(expand-serializer form)))
       (with-length u2 target-info (start-pc length index)
	 (u2 start-pc)
	 (u2 length)
	 (u2 index))))
     ;; type argument target
     ((#x47 #x48 #x49 #x4A #x4B)
      (destructuring-bind (offset type-argument-index) target-info
	(list
	 (u2 offset)
	 (u1 type-argument-index))))
     (t (error 'class-format-error
	       :message "Unknown type annotation target type")))))

(defun parse-target (bytes)
  (let ((target-type (parse-u1 bytes)))
    (cons
     target-type
     (case target-type
       ;; empty target
       ((#x13 #x14 #x15) '())
       ;; type parameter target, formal parameter target
       ((#x00 #x01 #x16)
	(list (parse-u1 bytes)))
       ;; supertype, throws target, catch target, offset target
       ((#x10 #x17 #x42 #x43 #x44 #x45)
	(list (parse-u2 bytes)))
       ;; type parameter bound target
       ((#x11 #x12)
	(list (parse-u1 bytes)
	      (parse-u1 bytes)))
       ;; localvar target
       ((#x40 #x41)
	(let ((target-info))
	  (macrolet ((with-length (&whole form &rest rest)
		       (declare (ignore rest))
		       `,(expand-deserializer form 'bytes)))
	    (with-length u2 target-info (start-pc length index)
	      (u2 start-pc)
	      (u2 length)
	      (u2 index)))
	  target-info))
       ;; type argument target
       ((#x47 #x48 #x49 #x4A #x4B)
	(list (parse-u2 bytes)
	      (parse-u1 bytes)))
       (t (error 'class-format-error
		 :message "Unknown type annotation target type"))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-serialization target-info (type info) byte-stream
    `(target-bytes ,type ,info)
    `(let ((target (parse-target ,byte-stream)))
       (setf ,type (first target)
	     ,info (rest target))))

  (def-jstruct type-path (paths)
    (with-length u1 paths (kind argument-index)
      (u1 kind)
      (u1 argument-index)))

  (def-serialization type-path (type-path) byte-stream
    `(serialize ,type-path (constant-pool))
    `(setf ,type-path (parse-type-path ,byte-stream (pool-array))))

  (def-jstruct type-annotation
      (target-type target-info target-path type element-value-pairs)
    (target-info target-type target-info)
    (type-path target-path)
    (u2 (utf8-info type))
    (with-length u2 element-value-pairs (name tag value)
      (u2 (utf8-info name))
      (element-value tag value)))

  (def-serialization type-annotation (annotation) byte-stream
    `(serialize ,annotation (constant-pool))
    `(setf ,annotation (parse-type-annotation ,byte-stream (pool-array)))))

(def-attribute runtime-visible-type-annotations
    "RuntimeVisibleTypeAnnotations"
    (annotations)
  (with-length u2 annotations annotation
    (type-annotation annotation)))

(def-attribute runtime-invisible-type-annotations
    "RuntimeInvisibleTypeAnnotations"
    (annotations)
  (with-length u2 annotations annotation
    (type-annotation annotation)))

(def-attribute annotation-default "AnnotationDefault" (tag value)
  (element-value tag value))

(def-attribute bootstrap-methods "BootstrapMethods" (methods)
  (with-length u2 methods (kind reference arguments)
    (u2 (method-handle-info kind reference))
    (with-length u2 arguments argument
      (u2 (pool-index argument)))))

(defparameter *parameter-modifiers*
  '((:final        #x0010)
    (:synthetic    #x1000)
    (:mandated     #x8000)))

(def-attribute method-parameters "MethodParameters" (parameters)
  (with-length u1 parameters (name access-flags)
    (u2 (utf8-info name))
    (u2 (access-modifiers access-flags *parameter-modifiers*))))

(defparameter *module-modifiers*
  '((:open         #x0020)
    (:synthetic    #x1000)
    (:mandated     #x8000)))

(defparameter *require-modifiers*
  '((:transitive   #x0020)
    (:static-phase #x0040)
    (:synthetic    #x1000)
    (:mandated     #x8000)))

(defparameter *exports-modifiers*
  '((:synthetic    #x1000)
    (:mandated     #x8000)))

(defparameter *opens-modifiers*
  '((:synthetic    #x1000)
    (:mandated     #x8000)))

(def-attribute module "Module" (name flags version requires exports opens uses provides)
  (u2 (utf8-info name))
  (u2 (access-modifiers flags *module-modifiers*))
  (u2 (utf8-info version))
  (with-length u2 requires (module flags version)
    (u2 (module-info module))
    (u2 (access-modifiers flags *require-modifiers*))
    (u2 (utf8-info version)))
  (with-length u2 exports (package flags exports-to)
    (u2 (package-info package))
    (u2 (access-modifiers flags *exports-modifiers*))
    (with-length u2 exports-to exports-to
      (u2 (module-info exports-to))))
  (with-length u2 opens (package flags opens-to)
    (u2 (package-info package))
    (u2 (access-modifiers flags *opens-modifiers*))
    (with-length u2 opens-to opens-to
      (u2 (module-info opens-to))))
  (with-length u2 uses uses
    (u2 (class-info uses)))
  (with-length u2 provides (name provides-with)
    (u2 (class-info name))
    (with-length u2 provides-with name
      (u2 (class-info name)))))

(def-attribute module-packages "ModulePackages" (packages)
  (with-length u2 packages package
    (u2 (package-info package))))

(def-attribute module-main-class "ModuleMainClass" (main-class)
  (u2 (class-info main-class)))

(def-attribute nest-host "NestHost" (host-class)
  (u2 (class-info host-class)))

(def-attribute nest-members "NestMembers" (classes)
  (with-length u2 classes class
    (u2 (class-info class))))

(def-attribute record "Record" (components)
  (with-length u2 components (name descriptor attributes)
    (u2 (utf8-info name))
    (u2 (utf8-info descriptor))
    (with-length u2 attributes attribute
      (attribute attribute))))

(def-attribute permitted-subclasses "PermittedSubclasses" (classes)
  (with-length u2 classes class
    (u2 (class-info class))))
