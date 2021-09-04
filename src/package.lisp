(in-package :cl-user)

(defpackage #:jclass
  (:use :cl)
  (:export #:encode-modified-utf8
	   #:decode-modified-utf8

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
	   #:float-info-ieee-bits
	   #:float-info-p

	   #:long-info
	   #:make-long-info
	   #:long-info-value
	   #:long-info-p

	   #:double-info
	   #:make-double-info
	   #:double-info-ieee-bits
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

	   ;; fields
	   #:field-info
	   #:make-field-info
	   #:field-info-flags
	   #:field-info-name
	   #:field-info-descriptor
	   #:field-info-attributes
	   #:field-info-p

	   ;; methods
	   #:method-info
	   #:make-method-info
	   #:method-info-flags
	   #:method-info-name
	   #:method-info-descriptor
	   #:method-info-attributes
	   #:method-info-p

	   ;; classes
	   #:java-class
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
	   #:constant-value
	   #:make-constant-value
	   #:constant-value-value
	   #:constant-value-p

	   #:stack-map-table
	   #:make-stack-map-table
	   #:stack-map-table-entries
	   #:stack-map-table-p

	   #:exceptions
	   #:make-exceptions
	   #:exceptions-exceptions
	   #:exceptions-p

	   #:inner-classes
	   #:make-inner-classes
	   #:inner-classes-classes
	   #:inner-classes-p

	   #:enclosing-method
	   #:make-enclosing-method
	   #:enclosing-method-class
	   #:enclosing-method-name
	   #:enclosing-method-type
	   #:enclosing-method-p

	   #:synthetic
	   #:make-synthetic
	   #:synthetic-p

	   #:signature
	   #:make-signature
	   #:signature-signature
	   #:signature-p

	   #:source-file
	   #:make-source-file
	   #:source-file-name
	   #:source-file-p

	   #:source-debug-extension
	   #:make-source-debug-extension
	   #:source-debug-extension-debug
	   #:source-debug-extension-p

	   #:line-number-table
	   #:make-line-number-table
	   #:line-number-table-line-numbers
	   #:line-number-table-p

	   #:local-variable-table
	   #:make-local-variable-table
	   #:local-variable-table-local-variables
	   #:local-variable-table-p

	   #:local-variable-type-table
	   #:make-local-variable-type-table
	   #:local-variable-type-table-local-variables
	   #:local-variable-type-table-p

	   #:deprecated
	   #:make-deprecated
	   #:deprecated-p

	   #:annotation
	   #:make-annotation
	   #:annotation-type
	   #:annotation-element-value-pairs
	   #:annotation-p

	   #:runtime-visible-annotations
	   #:make-runtime-visible-annotations
	   #:runtime-visible-annotations-annotations
	   #:runtime-visible-annotations-p

	   #:runtime-invisible-annotations
	   #:make-runtime-invisible-annotations
	   #:runtime-invisible-annotations-annotations
	   #:runtime-invisible-annotations-p

	   #:runtime-visible-parameter-annotations
	   #:make-runtime-visible-parameter-annotations
	   #:runtime-visible-parameter-annotations-annotations
	   #:runtime-visible-parameter-annotations-p

	   #:runtime-invisible-parameter-annotations
	   #:make-runtime-invisible-parameter-annotations
	   #:runtime-invisible-parameter-annotations-annotations
	   #:runtime-invisible-parameter-annotations-p

	   #:type-path
	   #:make-type-path
	   #:type-path-paths
	   #:type-path-p

	   #:type-annotation
	   #:make-type-annotation
	   #:type-annotation-target-type
	   #:type-annotation-target-info
	   #:type-annotation-target-path
	   #:type-annotation-type
	   #:type-annotation-element-value-pairs
	   #:type-annotation-p

	   #:runtime-visible-type-annotations
	   #:make-runtime-visible-type-annotations
	   #:runtime-visible-type-annotations-annotations
	   #:runtime-visible-type-annotations-p

	   #:runtime-invisible-type-annotations
	   #:make-runtime-invisible-type-annotations
	   #:runtime-invisible-type-annotations-annotations
	   #:runtime-invisible-type-annotations-p

	   #:annotation-default
	   #:make-annotation-default
	   #:annotation-default-tag
	   #:annotation-default-value
	   #:annotation-default-p

	   #:method-parameters
	   #:make-method-parameters
	   #:method-parameters-parameters
	   #:method-parameters-p

	   #:module
	   #:make-module
	   #:module-name
	   #:module-flags
	   #:module-version
	   #:module-requires
	   #:module-exports
	   #:module-opens
	   #:module-uses
	   #:module-provides
	   #:module-p

	   #:module-packages
	   #:make-module-packages
	   #:module-packages-packages
	   #:module-packages-p

	   #:module-main-class
	   #:make-module-main-class
	   #:module-main-class-main-class
	   #:module-main-class-p

	   #:nest-host
	   #:make-nest-host
	   #:nest-host-host-class
	   #:nest-host-p

	   #:nest-members
	   #:make-nest-members
	   #:nest-members-classes
	   #:nest-members-p

	   #:record
	   #:make-record
	   #:record-components
	   #:record-p

	   #:permitted-subclasses
	   #:make-permitted-subclasses
	   #:permitted-subclasses-classes
	   #:permitted-subclasses-p

	   ;; top level
	   #:java-class-bytes
	   #:disassemble-jclass
	   #:disassemble-file
	   #:class-format-error))
