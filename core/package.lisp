(defpackage #:jclass
  (:use :cl)
  (:shadow #:class
	   #:debug
	   #:type)
  (:export #:encode-modified-utf8
	   #:decode-modified-utf8
	   #:class-format-error

	   ;; constant pool
	   #:utf8-info
	   #:make-utf8-info
	   #:utf8-info-text
	   #:utf8-info-p

	   #:integer-info
	   #:make-integer-info
	   #:integer-info-value
	   #:integer-info-p

	   #:float-info
	   #:make-float-info
	   #:float-info-value
	   #:float-info-p

	   #:long-info
	   #:make-long-info
	   #:long-info-value
	   #:long-info-p

	   #:double-info
	   #:make-double-info
	   #:double-info-value
	   #:double-info-p

	   #:class-info
	   #:make-class-info
	   #:class-info-name
	   #:class-info-p

	   #:string-info
	   #:make-string-info
	   #:string-info-text
	   #:string-info-p

	   #:field-ref-info
	   #:make-field-ref-info
	   #:field-ref-info-class-name
	   #:field-ref-info-name
	   #:field-ref-info-type
	   #:field-ref-info-p

	   #:method-ref-info
	   #:make-method-ref-info
	   #:method-ref-info-class-name
	   #:method-ref-info-name
	   #:method-ref-info-type
	   #:method-ref-info-p

	   #:interface-method-ref-info
	   #:make-interface-method-ref-info
	   #:interface-method-ref-info-class-name
	   #:interface-method-ref-info-name
	   #:interface-method-ref-info-type
	   #:interface-method-ref-info-p

	   #:name-and-type-info
	   #:make-name-and-type-info
	   #:name-and-type-info-name
	   #:name-and-type-info-type
	   #:name-and-type-info-p

	   #:method-handle-info
	   #:make-method-handle-info
	   #:method-handle-info-kind
	   #:method-handle-info-reference
	   #:method-handle-info-p

	   #:method-type-info
	   #:make-method-type-info
	   #:method-type-info-descriptor
	   #:method-type-info-p

	   #:dynamic-info
	   #:make-dynamic-info
	   #:dynamic-info-bootstrap-index
	   #:dynamic-info-name
	   #:dynamic-info-type
	   #:dynamic-info-p

	   #:invoke-dynamic-info
	   #:make-invoke-dynamic-info
	   #:invoke-dynamic-info-bootstrap-index
	   #:invoke-dynamic-info-name
	   #:invoke-dynamic-info-type
	   #:invoke-dynamic-info-p

	   #:module-info
	   #:make-module-info
	   #:module-info-name
	   #:module-info-p

	   #:package-info
	   #:make-package-info
	   #:package-info-name
	   #:package-info-p

	   ;; structures
	   #:java-structure
	   #:java-class
	   #:field-info
	   #:method-info

	   ;; attributes
	   #:attribute
	   #:code
	   #:constant-value
	   #:stack-map-table
	   #:exceptions
	   #:inner-classes
	   #:enclosing-method
	   #:synthetic
	   #:signature
	   #:source-file
	   #:source-debug-extension
	   #:line-number-table
	   #:local-variable-table
	   #:local-variable-type-table
	   #:deprecated
	   #:annotation
	   #:runtime-visible-annotations
	   #:runtime-invisible-annotations
	   #:runtime-visible-parameter-annotations
	   #:runtime-invisible-parameter-annotations
	   #:type-path
	   #:type-annotation
	   #:runtime-visible-type-annotations
	   #:runtime-invisible-type-annotations
	   #:annotation-default
	   #:bootstrap-methods
	   #:method-parameters
	   #:module
	   #:module-packages
	   #:module-main-class
	   #:nest-host
	   #:nest-members
	   #:record
	   #:permitted-subclasses

	   ;; accessors
	   #:annotations
	   #:attributes
	   #:bytecode
	   #:classes
	   #:components
	   #:debug
	   #:descriptor
	   #:element-value-pairs
	   #:entries
	   #:exports
	   #:fields
	   #:flags
	   #:host-class
	   #:interfaces
	   #:line-numbers
	   #:local-variables
	   #:main-class
	   #:major-version
	   #:max-stack
	   #:max-locals
	   #:methods
	   #:minor-version
	   #:name
	   #:opens
	   #:packages
	   #:parameters
	   #:parent
	   #:paths
	   #:provides
	   #:requires
	   #:tag
	   #:target-info
	   #:target-path
	   #:target-type
	   #:type
	   #:type-path
	   #:uses
	   #:value
	   #:version

	   ;; top level
	   #:assemble-jclass
	   #:disassemble-jclass
	   #:disassemble-file))
