(in-package #:jclass)

(defstruct class-bytes
  array
  index)

(define-condition class-format-error (error)
  ())

(defun parse-bytes (count bytes)
  (with-slots (array index) bytes
    (if (< (- (length array) index) count)
	(error 'class-format-error)
	(prog1 (make-array count
			   :displaced-to array
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

(defun disassemble-class (bytes)
  (let ((cbytes (make-class-bytes :array bytes :index 0)))
  ;; magic number
  (assert (= (parse-u4 cbytes) #xCAFEBABE))
  ;; minor, major versions
  (print (parse-u2 cbytes))
  (print (parse-u2 cbytes))
  ;; constant pool
  ;; fields
  ;; methods
  ;; attributes
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
