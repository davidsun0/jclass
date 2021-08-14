(in-package #:jclass)

(defmacro define-serializer (fn lambda-list &body body)
  (with-gensyms (argument)
    `(setf (gethash ',fn *serializers*)
	   (lambda (,argument)
	     (destructuring-bind ,lambda-list ,argument
	       ,@body)))))

(defmacro define-deserializer (fn lambda-list &body body)
  (with-gensyms (argument)
    `(setf (gethash ',fn *deserializers*)
	   (lambda (&rest ,argument)
	     (destructuring-bind ,lambda-list ,argument
	       ,@body)))))

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

  (define-serializer with-length (unit list fields &rest body)
    (with-gensyms (term)
      `(list
	(,unit (length ,list))
	(mapcar (lambda (&rest ,term)
		  (destructuring-bind (,fields) ,term
		    (list ,@(mapcar #'expand-serializer body))))
		,list))))

  (define-deserializer with-length ((unit list fields &rest body) stream)
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

  ;; Byte manipulation

  (define-serializer u1 (inner) `(u1 ,(expand-serializer inner)))
  (define-serializer u2 (inner) `(u2 ,(expand-serializer inner)))
  (define-serializer u4 (inner) `(u4 ,(expand-serializer inner)))

  (define-deserializer u1 ((inner) stream)
    (expand-deserializer inner `(parse-u1 ,stream)))
  (define-deserializer u2 ((inner) stream)
    (expand-deserializer inner `(parse-u2 ,stream)))
  (define-deserializer u4 ((inner) stream)
    (expand-deserializer inner `(parse-u4 ,stream)))

  ;; Constant pool references

  (define-serializer utf8-info  (text)
    `(pool-index (make-utf8-info ,text)))

  (define-deserializer utf8-info ((place) argument)
    `(setf ,place (utf8-info-text (aref (pool-array) ,argument))))
  
  (define-serializer class-info (name)
    `(pool-index (make-class-info ,name)))

  (define-deserializer class-info ((place) argument)
    `(setf ,place (class-info-name (aref (pool-array) ,argument))))

  (define-serializer name-and-type-info (name type)
    `(pool-index (make-name-and-type-info ,name ,type)))

  (define-deserializer name-and-type-info ((name type) argument)
    (with-gensyms (info-ref)
      `(let ((,info-ref (aref (pool-array) ,argument)))
	 (setf ,name (name-and-type-info-name ,info-ref))
	 (setf ,type (name-and-type-info-type ,info-ref)))))

  (define-serializer method-handle-info (kind reference)
    `(pool-index (make-method-handle-info ,kind ,reference)))

  (define-deserializer method-handle-info ((kind reference) argument)
    (with-gensyms (handle-ref)
      `(let ((,handle-ref (aref (pool-array) ,argument)))
	 (setf ,kind      (method-handle-info-kind      ,handle-ref))
	 (setf ,reference (method-handle-info-reference ,handle-ref)))))

  (define-serializer module-info (name)
    `(pool-index (make-module-info ,name)))

  (define-deserializer module-info ((place) argument)
    `(setf ,place (make-module-info (aref (pool-array) ,argument))))

  (define-serializer package-info (name)
    `(pool-index (make-package-info ,name)))

  (define-deserializer package-info ((place) argument)
    `(setf ,place (make-package-info (aref (pool-array) ,argument))))
  
  ;; Advanced

  (define-serializer access-modifiers (mod-list mod-map)
    `(access-modifiers ,mod-list ,mod-map))

  (define-deserializer access-modifiers ((place mod-map) bytes)
    `(setf ,place (access-flag-lookup ,bytes ,mod-map)))

  (define-serializer raw-bytes (bytes)
    `(coerce 'list ,bytes))

  (define-deserializer raw-bytes ((place) stream)
    (let ((rbytes (gensym)))
      `(setf ,place
	     (let ((,rbytes ,stream))
	       (with-slots (array index) ,rbytes
		 (parse-bytes (- (length array) index)
			      ,rbytes))))))

  ;; Pool index with unspecified type

  (define-serializer pool-index (constant)
    `(pool-index ,constant))

  (define-deserializer pool-index ((place) index)
    `(setf ,place (aref (pool-array) ,index)))

  ;; Structures

  (define-serializer field    (field) `(byte-list ,field  (constant-pool)))
  (define-serializer method  (method) `(byte-list ,method (constant-pool)))
  (define-serializer attribute (attr) `(byte-list ,attr   (constant-pool)))

  (define-deserializer field ((field) stream)
    `(setf ,field (parse-field-info ,stream (pool-array))))

  (define-deserializer method ((method) stream)
    `(setf ,method (parse-method-info ,stream (pool-array))))

  (define-deserializer attribute ((attr) stream)
    `(setf ,attr (parse-attribute ,stream (pool-array))))

  ) ; end eval-when

(defgeneric byte-list (pool struct)
  (:documentation "Constructs the binary form of a JVM structure."))

(defmacro def-jstruct (name slots &body body)
  (let ((struct-obj (gensym))
	(pool (gensym)))
    `(progn
       (defstruct (,name (:constructor ,(symbol-concatenate 'make- name) ,slots))
	 ,@slots)

       (defmethod byte-list (,pool (,struct-obj ,name))
	 (flet ((constant-pool () ,pool)
		(pool-index (constant) (pool-index ,pool constant)))
	   (declare (ignorable (function constant-pool)
			       (function pool-index)))
	   (with-slots ,slots ,struct-obj
	     (list ,@(mapcar #'expand-serializer body)))))

       (defun ,(symbol-concatenate 'parse- name) (,struct-obj ,pool)
	 (flet ((pool-array () ,pool))
	   (declare (ignorable (function pool-array)))
	   (let ,slots
	     ,@(loop for form in body
		     collect (expand-deserializer form struct-obj))
	     (,(symbol-concatenate 'make- name) ,@slots)))))))

(defmacro def-attribute (name name-string slots &body body)
  (with-gensyms (struct-obj pool byte-stream)
    `(progn
       (defstruct (,name (:constructor ,(symbol-concatenate 'make- name) ,slots))
	 ,@slots)

       (defmethod byte-list (,pool (,struct-obj ,name))
	 (flet ((constant-pool () ,pool)
		(pool-index (constant) (pool-index ,pool constant)))
	   (declare (ignorable (function constant-pool)
			       (function pool-index)))
	   (flet ((serialize-body ()
		    (with-slots ,slots ,struct-obj
		      (list ,@(mapcar #'expand-serializer body)))))
	     (let ((body (flatten (serialize-body) :remove-nil t)))
	       (list
		(u2 (pool-index (make-utf8-info ,name-string)))
		(u4 (length body))
		body)))))

       (setf (gethash ,name-string *attribute-parsers*)
	     (lambda (,byte-stream ,pool)
	       (declare (ignorable ,byte-stream))
	       (flet ((pool-array () ,pool))
		 (declare (ignorable (function pool-array)))
		 (let ,slots
		   ,@(loop for form in body
			   collect (expand-deserializer form byte-stream))
		   (,(symbol-concatenate 'make- name) ,@slots))))))))

(defparameter *attribute-parsers*
  (make-hash-table :test 'equal))

(defun parse-attribute (bytes pool-array &rest arguments)
  (let* ((name-index (parse-u2 bytes))
	 (name       (aref pool-array name-index))
	 (length     (parse-u4 bytes))
	 (body       (parse-bytes length bytes)))
    (assert (utf8-info-p name) (name-index)
	    'class-format-error
	    :message "Attribute name is not a UTF-8 constant")
    (restart-case
	(apply (gethash (utf8-info-text name) *attribute-parsers*
			(lambda (bytes pool-array)
			  (declare (ignore bytes pool-array))
			  (error 'class-format-error
				 :message (format nil "Unknown attribute ~A"
						  (utf8-info-text name)))))
	       (make-class-bytes :array body :index 0)
	       pool-array
	       arguments)
      (ignore-attribute ()
	:report "Use the raw attribute byte array"
	(list (utf8-info-text name) body)))))

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

(defun java-class-bytes (java-class &optional (pool (make-constant-pool)))
  ;; resolve the constants first
  (let ((bytes (byte-list pool java-class)))
    (flatten (list*
	      (u4 #xCAFEBABE) ; file magic number
	      (u2 (java-class-minor-version java-class))
	      (u2 (java-class-major-version java-class))
	      (constant-pool-bytes pool)
	      bytes)
	     :remove-nil t)))

(defun disassemble-jclass (bytes)
  (let* ((cbytes (make-class-bytes :array bytes :index 0))
	 (magic  (when (/= (parse-u4 cbytes) #xCAFEBABE)
		   (error 'class-format-error
			  :message
			  "Not a Java class file: missing magic number 0xCAFEBABE")))
	 (minor-version (parse-u2 cbytes))
	 (major-version (parse-u2 cbytes))
	 (pool-array    (parse-constant-pool cbytes))
	 (jclass        (parse-java-class cbytes pool-array)))
    (declare (ignore magic))
    (setf (java-class-minor-version jclass) minor-version)
    (setf (java-class-major-version jclass) major-version)
    (values jclass pool-array)))

(defun disassemble-file (path)
  (with-open-file (stream path
			  :direction :input
			  :element-type '(unsigned-byte 8)
			  :if-does-not-exist :error)
    (let* ((length (file-length stream))
	   (buffer (make-array length :element-type '(unsigned-byte 8))))
      (read-sequence buffer stream)
      (disassemble-jclass buffer))))

(def-attribute constant-value "ConstantValue" (value)
  (u2 (utf8-info value)))

;; Code

;; StackMapTable

(def-attribute exceptions "Exceptions" (exceptions)
  (with-length u2 exceptions (ex)
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
  (raw-bytes debug))

;; LineNumberTable

;; LocalVariableTable

;; LocalVariableTypeTable

(def-attribute deprecated "Deprecated" ())

#|
(def-attribute runtime-visible-annotations
    "RuntimeVisibleAnnotations"
    (annotations)
  (with-length u2 anotations (type element-value-pairs)
    (u2 (utf8-info type))
    (with-length u2 element-value-pairs ((element-name value))
      (u2 (utf8-info element))
      ;; TODO: complicated parsing
      )))

(defun write-annotation (annotation pool)
  )

(defun write-element-value (type value pool)
  (list
   (u1 (char-code type))
   (ccase type
     (#\B (u2 (pool-index pool (make-integer-info value))))
     (#\C (u2 (pool-index pool (make-integer-info (char-code value)))))
     (#\D (u2 (pool-index pool (make-double-info value))))
     (#\F (u2 (pool-index pool (make-float-info value))))
     (#\I (u2 (pool-index pool (make-integer-info value))))
     (#\L (u2 (pool-index pool (make-long-info value))))
     (#\S (u2 (pool-index pool (make-integer-info value))))
     (#\Z (u2 (pool-index pool (make-integer-info (if value 1 0)))))
     (#\s (u2 (pool-index pool (make-utf8-info value))))
     (#\e (destructuring-bind (type const) value
	    (list
	     (u2 (pool-index pool (make-utf8-info type)))
	     (u2 (pool-index pool (make-utf8-info const))))))
     (#\@ (write-annotation value pool))
     (#\[ (let ((values value))
	    (list
	     (u2 (length values))
	     (mapcar #'element-value values)))))))

(def-attribute runtime-invisible-annotations
    "RuntimeInvisibleAnnotations"
    (annotations)
  (with-length u2 anotations (annotation)
    (annotation annotation)))

(def-attribute runtime-visible-parameter-annotations
    "RuntimeVisibleParameterAnnotations"
    (parameters)
  (with-length u1 parameters (annotations)
    (with-length u2 anotations (annotation)
      (annotation annotation))))

(def-attribute runtime-invisible-parameter-annotations
    "RuntimeInvisibileParameterAnnotations"
    (parameters)
  (with-length u1 parameters (annotations)
    (with-length u2 anotations (annotation)
      (annotation annotation))))

;; TODO: parse type annotations

(def-attribute runtime-visible-type-annotations
    "RuntimeVisibleTypeAnnotations"
    (annotations)
  (with-length u2 annotations (annotation)
    (type-annotation annotation)))

(def-attribute runtime-invisible-type-annotations
    "RuntimeInvisibleTypeAnnotations"
    (annotations)
  (with-length u2 annotations (annotation)
    (type-annotation annotation)))

(def-attribute annotation-default "AnnotationDefault" (element-value)
  (element-value element-value))
|#

;; needs debugging
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
    (with-length u2 exports-to (exports-to)
      (u2 (module-info exports-to))))
  (with-length u2 opens (package flags opens-to)
    (u2 (package-info package))
    (u2 (access-modifiers flags *opens-modifiers*))
    (with-length u2 opens-to (opens-to)
      (u2 (module-info opens-to))))
  (with-length u2 uses (uses)
    (u2 (class-info uses)))
  (with-length u2 provides (name provides-with)
    (u2 (class-info name))
    (with-length u2 provides-with (name)
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
