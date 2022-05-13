(in-package #:jclass)

(defmacro with-gensyms (symbols &body body)
  `(let ,(loop for s in symbols collect `(,s (gensym)))
     ,@body))

(defmacro fresh-dolist ((var list-form) &body body)
  "A simple version of dolist that also guarantees a fresh binding of `var`."
  (with-gensyms (fresh)
  `(loop for ,fresh in ,list-form do
    (let ((,var ,fresh))
      ,@body))))

(defun flatten (tree)
  "Flattens a tree into a list of leaves. Treats nil as an empty list, not an atom."
  (let ((output '()))
    (labels ((traverse (tree)
	       (cond
		 ((null tree)) ; ignore nil
		 ((atom tree) (push tree output))
		 (t (map nil #'traverse tree)))))
      (traverse tree)
      (nreverse output))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symbol-concatenate (&rest values)
    (intern (format nil "~{~A~}" values))))

;;; Byte manipulation

(declaim (inline u1 u2 u4 signed-8 signed-16 signed-32))

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

(defun signed-8 (byte)
  "Sign extends an (unsigned-byte 8) to a (signed-byte 8)."
  (logior byte (- (mask-field (byte 1 7) byte))))

(defun signed-16 (short)
  "Sign extends an (unsigned-byte 16) to a (signed-byte 16)."
  (logior short (- (mask-field (byte 1 15) short))))

(defun signed-32 (int)
  "Sign extends an (unsigned-byte 32) to a (signed-byte 32)."
  (logior int (- (mask-field (byte 1 31) int))))

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

(defun encode-modified-utf8 (string)
  "Encodes a string as a list of modified UTF-8 bytes."
  ;; See JVM Spec 4.4.7 CONSTANT_Utf8_info
  ;; String is encoded into UTF-16, then re-encoded to UTF-8
  (flet ((utf16->utf8 (code)
	   (cond
	     ((< code #x80)
	      (list code))
	     ((< code #x800)
	      (list (dpb (ash code -6) (byte 6 0) #b11000000)
		    (dpb code (byte 6 0) #b10000000)))
	     (t
	      (list (dpb (ash code -12) (byte 4 0) #b11100000)
		    (dpb (ash code  -6) (byte 6 0) #b10000000)
		    (dpb code (byte 6 0) #b10000000))))))
    (loop for code in (map 'list #'char-code string)
	  if (>= code #x10000)
	    ;; Decompose into surrogate pairs.
	    collect (+ #xD800 (ldb (byte 10 10) (- code #x10000))) into utf16
	    and collect (+ #xDC00 (ldb (byte 10 0) code)) into utf16
	  else
	    collect code into utf16
	  finally (return (apply #'append (mapcar #'utf16->utf8 utf16))))))

(defun decode-modified-utf8 (bytes)
  "Interprets a sequence of (unsigned-byte 8) as a modified UTF-8 string."
  ;; See JVM Spec 4.4.7 CONSTANT_Utf8_info
  (let ((byte-list (coerce bytes 'list)))
    (labels ((extract-bits (lead width integer shift)
	       (assert (= (ash integer (- width)) lead) (integer)
		       "Unexpected modified UTF-8 byte ~A." integer)
	       (ash (ldb (byte width 0) integer) shift))
	     (decode-utf8 ()
	       (let ((byte (pop byte-list)))
		 (cond
		   ((not (or (zerop byte)
			     (logbitp 7 byte)))
		    byte)
		   ((= (ldb (byte 3 5) byte) #b110)
		    (logior (extract-bits #b110 5 byte 6)
			    (extract-bits #b10  6 (pop byte-list) 0)))
		   ((= (ldb (byte 4 4) byte) #b1110)
		    (logior (extract-bits #b1110 4 byte 12)
			    (extract-bits #b10   6 (pop byte-list) 6)
			    (extract-bits #b10   6 (pop byte-list) 0)))
		   (t (error "Invalid modified UTF-8 sequence ~A." bytes)))))
	     (decode-utf16 ()
	       (let ((code (decode-utf8)))
		 (cond
		   ((or (< code #xD800) (>= code #xE000))
		    code)
		   ((<= #xD800 code #xDBFF)
		    (let ((upper code)
			  (lower (decode-utf8)))
		      (assert (<= #xDC00 lower #xDFFF) (lower)
			      "Invalid UTF-16 surrogate pair ~A ~A." upper lower)
		      (+ #x10000 (logior (extract-bits #b110110 10 upper 10)
					 (extract-bits #b110111 10 lower 0)))))
		   (t (error "Invalid UTF-16 code point ~A." code))))))
      (loop while byte-list
	    collect (decode-utf16) into chars
	    finally (return (map 'string #'code-char chars))))))
