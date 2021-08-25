(in-package :cl-user)

(defpackage #:jclass
  (:use :cl)
  (:export #:encode-modified-utf8
	   #:decode-modified-utf8

	   ;; constant pool
	   #:make-utf8-info
	   #:utf8-info-text
	   #:utf8-info-p

	   #:make-integer-info
	   #:integer-info-value
	   #:integer-info-p

	   #:make-float-info
	   #:float-info-ieee-bits
	   #:float-info-p
	   
	   #:make-long-info
	   #:long-info-value
	   #:long-info-p

	   #:make-double-info
	   #:double-info-ieee-bits
	   #:double-info-p

	   #:make-class-info
	   #:class-info-name
	   #:class-info-p
	   
	   #:make-string-info
	   #:string-info-text
	   #:string-info-p

	   #:make--ref-info
	   #:-ref-info-class-name
	   #:-ref-info-name
	   #:-ref-info-type
	   #:-ref-info-p

	   #:make-field-ref-info
	   #:field-ref-info-class-name
	   #:field-ref-info-name
	   #:field-ref-info-type
	   #:field-ref-info-p

	   #:make-method-ref-info
	   #:method-ref-info-class-name
	   #:method-ref-info-name
	   #:method-ref-info-type
	   #:method-ref-info-p

	   #:make-interface-method-ref-info
	   #:interface-method-ref-info-class-name
	   #:interface-method-ref-info-name
	   #:interface-method-ref-info-type
	   #:interface-method-ref-info-p

	   #:make-name-and-type-info
	   #:name-and-type-info-name
	   #:name-and-type-info-type
	   #:name-and-type-info-p

	   #:make-method-handle-info
	   #:method-handle-info-kind
	   #:method-handle-info-reference
	   #:method-handle-info-p

	   #:make-method-type-info
	   #:method-type-info-descriptor
	   #:method-type-info-p

	   #:make-dynamic-info
	   #:dynamic-info-bootstrap-index
	   #:dynamic-info-name
	   #:dynamic-info-type
	   #:dynamic-info-p

	   #:make-invoke-dynamic-info
	   #:invoke-dynamic-info-bootstrap-index
	   #:invoke-dynamic-info-name
	   #:invoke-dynamic-info-type
	   #:invoke-dynamic-info-p

	   #:make-module-info
	   #:module-info-name
	   #:module-info-p

	   #:make-package-info
	   #:package-info-name
	   #:package-info-p

	   ;; fields
	   #:make-field-info
	   #:field-info-flags
	   #:field-info-name
	   #:field-info-descriptor
	   #:field-info-attributes
	   #:field-info-p

	   ;; methods
	   #:make-method-info
	   #:method-info-flags
	   #:method-info-name
	   #:method-info-descriptor
	   #:method-info-attributes
	   #:method-info-p

	   ;; classes
	   #:make-java-class
	   #:java-class-minor-version
	   #:java-class-major-version
	   #:java-class-flags
	   #:java-class-name
	   #:java-class-parent
	   #:java-class-interfaces
	   #:java-class-fields
	   #:java-class-methods
	   #:java-class-attributes
	   #:java-class-p

	   ;; attributes

	   ;; top level
	   #:java-class-bytes
	   #:disassemble-jclass
	   #:disassemble-file
	   #:class-format-error
	   ))
