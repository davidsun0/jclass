(in-package #:jclass)

(define-condition class-format-error (error)
  ((message :initarg :message
	    :accessor message))
  (:report (lambda (condition stream)
	     (format stream "~A" (message condition)))))

(defstruct class-bytes
  array
  index)

(defun parse-bytes (count bytes)
  (with-slots (array index) bytes
    (if (< (- (length array) index) count)
	(error 'class-format-error)
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

(defun decode-modified-utf8 (bytes)
  (flet ((extract-bits (mask shift byte)
	   (assert (= mask (ash byte (- shift)))
		   (byte) "~A ~A ~A" mask shift byte)
	   (logand (1- (ash 1 shift)) byte)))
    (let ((input (coerce bytes 'list)))
      (loop while input
	    collect
	    (code-char
	     (let ((code (pop input)))
	       (cond
		 ((zerop (ash code -7)) code)
		 ((= (ash code -5) #b110)
		  (logior (ash (extract-bits #b110 5 code) 6)
			  (extract-bits #b10 6 (pop input))))
		 ((= code #b11101101)
		  (logior (ash (1+ (extract-bits #b1010 4 (pop input))) 16)
			  (ash (extract-bits #b10 6 (pop input)) 10)
			  (* 0 (pop input))
			  (ash (extract-bits #b1011 4 (pop input)) 6)
			  (extract-bits #b10 6 (pop input))))
		 ((= (ash code -4) #b1110)
		  (logior (ash (extract-bits #b1110 4 code) 12)
			  (ash (extract-bits #b10 6 (pop input)) 6)
			  (extract-bits #b10 6 (pop input))))
		 (t (error 'class-format-error :message "Invalid modified UTF-8 sequence")))))))))

(defun allocate-constant (bytes)
  (let ((tag (parse-u1 bytes)))
    (cons tag
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
  (let* ((constant (aref pool index))
	 (tag (first constant)))
    (if (symbolp tag) ; already built constants begin with symbols
	constant
	(setf
	 (aref pool index)
	 (case tag
	   ((1)
	    (make-utf8-info (map 'string #'code-char (second constant))))
	   ((3) (make-integer-info (second constant)))
	   ((4) (make-float-info (second constant)))
	   ((5 6)
	    (destructuring-bind (high-bytes low-bytes) (rest constant)
	      (let ((value (logior (ash high-bytes 32) low-bytes)))
		(if (= tag 5)
		    (make-long info value)
		    (make-double-info value)))))
	   ((7 8 16 19 20)
	    (let* ((utf8-index (second constant))
		   (utf8-constant (build-constant pool utf8-index))
		   (text (utf8-info-text utf8-constant)))
	      (case tag
		((7)  (make-class-info text))
		((8)  (make-string-info text))
		((16) (make-method-type-info text))
		((19) (make-module-info text))
		((20) (make-package-info text)))))
	   ((9 10 11)
	    (destructuring-bind (class-index name-type-index) (rest constant)
	      (let* ((class-constant (build-constant pool class-index))
		     (class-name (class-info-name class-constant))
		     (name-type (build-constant pool name-type-index))
		     (name (name-and-type-info-name name-type))
		     (type (name-and-type-info-type name-type)))
		(case tag
		  ((9) (make-field-ref-info class-name name type))
		  ((10) (make-method-ref-info class-name name type))
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
		    (make-dynamic-info bootstrap-index name type)
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

(defun disassemble-class (byte-array)
  (let ((cbytes (make-class-bytes :array byte-array :index 0)))
    ;; magic number
    (when (/= (parse-u4 cbytes) #xCAFEBABE)
      (error 'class-format-error :message "File is not a Java class file"))
    ;; minor, major versions
    (print (parse-u2 cbytes))
    (print (parse-u2 cbytes))
    ;; constant pool
    (parse-constant-pool cbytes)
    ;; fields
    ;; methods
    ;; attributes
    ;; (print (class-bytes-index cbytes))
    ))

(defun disassemble-file (path)
  (with-open-file (stream path
			  :direction :input
			  :element-type '(unsigned-byte 8)
			  :if-does-not-exist :error)
    (let* ((length (file-length stream))
	   (buffer (make-array length :element-type '(unsigned-byte 8))))
      (read-sequence buffer stream)
      (disassemble-class buffer))))
