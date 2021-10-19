(in-package #:jclass)

(defun java-class-bytes (java-class &optional (pool (make-constant-pool)))
  "Converts a java-class object into its class file bytes."
  ;; resolve the constants first
  (let ((bytes (serialize pool java-class)))
    (flatten (list*
	      (u4 #xCAFEBABE) ; file magic number
	      (u2 (minor-version java-class))
	      (u2 (major-version java-class))
	      (constant-pool-bytes pool)
	      bytes))))

(defun disassemble-jclass (bytes)
  "Builds a java-class struct from an array of bytes."
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
    (setf (minor-version jclass) minor-version)
    (setf (major-version jclass) major-version)
    (values jclass pool-array)))

(defun disassemble-file (path)
  "Builds a java-class struct from a file."
  (with-open-file (stream path
			  :direction :input
			  :element-type '(unsigned-byte 8)
			  :if-does-not-exist :error)
    (let* ((length (file-length stream))
	   (buffer (make-array length :element-type '(unsigned-byte 8))))
      (read-sequence buffer stream)
      (disassemble-jclass buffer))))
