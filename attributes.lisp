(in-package #:jclass)

(defmacro def-attribute (name name-string slots &body body)
  `(def-jstruct ,name ,slots
     ;; all attributes have this structure: u2 name, u4 length, bytes
     (u2-pool-index (make-utf8-info ,name-string))
     (let ((body-bytes (flatten (list ,@body) :remove-nil t)))
       (list (u4 (length body-bytes))
	     body-bytes))))

(defmacro def-attribute-parser (attribute-name lambda-list &body body)
  `(setf (gethash attribute-name *attribute-parsers)
	 (lambda ,lambda-list
	   ,@body)))

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

