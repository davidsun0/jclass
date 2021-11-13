(in-package #:jclass)

(defmacro with-gensyms (symbols &body body)
  `(let ,(loop for s in symbols collect `(,s (gensym)))
     ,@body))

(defun flatten (tree)
  "Flattens a tree into a list of leaves. Treats nil as an empty list, not an atom."
  (let ((output '()))
    (labels ((traverse (tree)
	       (cond
		 ((null tree)) ;; ignore nil
		 ((atom tree) (push tree output))
		 (t (map nil #'traverse tree)))))
      (traverse tree)
      (nreverse output))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symbol-concatenate (&rest values)
    (intern (format nil "~{~A~}" values))))

;;; Disassembly

(define-condition class-format-error (error)
  ((message :initarg :message
	    :accessor message))
  (:report (lambda (condition stream)
	     (format stream "~A" (message condition)))))

;; Buffer that represents the elements of array starting at index and ending
;; at array's fill pointer.
(defstruct class-bytes
  array
  index)

;;; Byte manipulation

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

(defun parse-bytes (count bytes)
  (with-slots (array index) bytes
    (if (< (- (length array) index) count)
	(error 'class-format-error
	       :message "Unexpected end of byte stream")
	(prog1
	  (make-array count
		      :displaced-to array
		      :displaced-index-offset index
		      :element-type (array-element-type array)
		      :fill-pointer count)
	  (incf index count)))))

(declaim (inline parse-u1 parse-u2 parse-u4))

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

(declaim (inline signed-8 signed-16 signed-32))

(defun signed-8 (byte)
  "Converts an (unsigned-byte 8) to a (signed-byte 8) with the same bitstring."
  (if (> byte #.(1- (expt 2 7)))
      (- byte #.(expt 2 8))
      byte))

(defun signed-16 (short)
  "Converts an (unsigned-byte 16) to a (signed-byte 16) with the same bitstring."
  (if (> short #.(1- (expt 2 15)))
      (- short #.(expt 2 16))
      short))

(defun signed-32 (int)
  "Converts an (unsigned-byte 32) to a (signed-byte 32) with the same bitstring."
  (if (> int #.(1- (expt 2 31)))
      (- int #.(expt 2 32))
      int))

(defun encode-modified-utf8 (string)
  "Encodes a string as a list of modified UTF-8 bytes."
  ;; See JVM Spec 4.4.7 CONSTANT_Utf8_info
  ;; String is encoded into UTF-16, then re-encoded to UTF-8
  (let ((output '()))
    (labels ((encode-code-point (code)
	       (cond
		 ((< 0 code #x80)
		  (push code output))
		 ((< code #x800)
		  (push (logior #b11000000 (ash code -6)) output)
		  (push (logior #b10000000 (logand #x3F code)) output))
		 ((< code #x10000)
		  (push (logior #b11100000 (ash code -12)) output)
		  (push (logior #b10000000 (logand #x3F (ash code -6))) output)
		  (push (logior #b10000000 (logand #x3F code)) output))
		 ((< code #x110000)
		  (let* ((code (- code #x10000))
			 ;; Decompose into UTF-16 surrogate pairs
			 (upper (ash code -10))
			 (lower (logand code #x3FF)))
		    (encode-code-point (+ #xD800 upper))
		    (encode-code-point (+ #xDC00 lower))))
		 ;; Implementations that can infer this is unreachable code
		 #-(or sbcl)
		 (t (error "Invalid Unicode code point: ~A" code)))))
      (loop for code across string
	    do (encode-code-point (char-code code)))
      (nreverse output))))

(defun decode-modified-utf8 (bytes)
  "Decodes a sequence of modified UTF-8 bytes into a string."
  ;; See JVM Spec 4.4.7 CONSTANT_Utf8_info
  (let ((input (coerce bytes 'list))
	(output '()))
    (labels ((extract-bits (mask shift byte)
	       (assert (= mask (ash byte (- shift))) (byte)
		       'class-format-error
		       :message (format nil "Invalid UTF-8 byte ~A" byte))
	       (logand (1- (ash 1 shift)) byte))
	     (decode-utf8 ()
	       (let ((byte (pop input)))
		 (cond
		   ((and (zerop (ash byte -7))
			 (not (zerop byte)))
		    byte)
		   ((= (ash byte -5) #b110)
		    (logior (ash (extract-bits #b110 5 byte) 6)
			    (extract-bits #b10 6 (pop input))))
		   ((= (ash byte -4) #b1110)
		    (logior (ash (extract-bits #b1110 4 byte) 12)
			    (ash (extract-bits #b10 6 (pop input)) 6)
			    (extract-bits #b10 6 (pop input))))
		   (t (error 'class-format-error
			     :message (format nil "Invalid modified UTF-8 sequence ~A"
					      bytes)))))))
      (loop while input do
	(push
	 (let ((code-point (decode-utf8)))
	   (cond
	     ((or (< code-point #xD800) (<= #xE000 code-point))
	      (code-char code-point))
	     ((<= #xD800 code-point #xDBFF)
	      ;; decode UTF-16 surrogate pair
	      (let ((upper code-point)
		    (lower (decode-utf8)))
		(assert (<= #xDC00 lower #xDFFF) (lower)
			'class-format-error
			:message (format nil "Invalid surrogate pair ~A ~A" upper lower))
		(code-char (+ #x10000
			      (logior (ash (- upper #xD800) 10)
				      (- lower #xDC00))))))
	     (t (error 'class-format-error
		       :message (format nil "Invalid modified UTF-8 sequence ~A"
					bytes)))))
	 output))
      (coerce (nreverse output) 'string))))
