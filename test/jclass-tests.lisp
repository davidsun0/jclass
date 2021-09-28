;;;; jclass unit and integration tests
(defpackage #:jclass/tests
  (:use :cl)
  (:export #:all-tests))
(in-package #:jclass/tests)

(fiveam:def-suite all-tests
  :description "jclass test suite")
(fiveam:in-suite all-tests)

(defun all-tests ()
  (fiveam:run! 'all-tests))

;;; Modified UTF-8

(fiveam:test ascii-tests
  ;; check that char-code and code-char use ASCII
  (fiveam:is (= 97 (char-code #\a)))
  (fiveam:is (char= #\a (code-char 97)))
  ;; sanity check
  (fiveam:is (equal '(97 98 99) (jclass:encode-modified-utf8 "abc")))
  ;; string concatenation with invokedynamic produces
  ;; strings with unprintable characters
  (let ((utf-str (jclass:decode-modified-utf8 #(1 58 49))))
    (fiveam:is (= 3 (length utf-str)))
    (fiveam:is (= 1 (char-code (aref utf-str 0))))))

(defun encode-char (char)
  (jclass:encode-modified-utf8 (string char)))

(fiveam:test unicode-encoding
  ;; check that Unicode is preserved
  (fiveam:is (= #x5496 (char-code (code-char #x5496))))
  ;; two byte UTF-8 encoding
  (fiveam:is (equal '(#xC2 #xA2)
		    (encode-char (code-char #xA2))))
  ;; three byte UTF-8 encoding
  (fiveam:is (equal '(#xE0 #xA4 #xB9)
		    (encode-char (code-char #x939))))
  ;; astral plane characters are first broken into UTF-16 surrogate pairs
  ;; U+10348 is broken into #xD800 and #xDF48
  (fiveam:is (equal '(#xED #xA0 #x80 #xED #xBD #x88)
		    (encode-char (code-char #x10348)))))

(defun decode-first-char (bytes)
  (let* ((string (jclass:decode-modified-utf8 bytes)))
    (aref string 0)))

(fiveam:test unicode-decoding
  (fiveam:is (char= #\a (decode-first-char #(97))))
  ;; two byte char
  (fiveam:is (= #xA2 (char-code (decode-first-char #(#xC2 #xA2)))))
  ;; three byte char
  (fiveam:is (= #x939 (char-code (decode-first-char #(#xE0 #xA4 #xB9)))))
  ;; six byte char
  (fiveam:is (= #x10348 (char-code (decode-first-char
				    #(#xED #xA0 #x80 #xED #xBD #x88))))))

;;; Class Components

(fiveam:test access-flags
  (let* ((keywords '(:public :super :abstract))
	 (flags (jclass::access-modifiers keywords jclass::*class-modifiers*)))
    (fiveam:is (= #x0421 flags)))
  (let* ((inverse (jclass::access-flag-lookup #x0421 jclass::*class-modifiers*)))
    (fiveam:is (equal '(:public :super :abstract) inverse))))

;; Used in invokeinterface encoding
(fiveam:test interface-count
  (fiveam:is (= 1 (jclass::interface-count "()V")))
  (fiveam:is (= 3 (jclass::interface-count "(D)V")))
  (fiveam:is (= 3 (jclass::interface-count "(J)V")))
  (fiveam:is (= 2 (jclass::interface-count "(Ljava/lang/Object;)V")))
  (fiveam:is (= 2 (jclass::interface-count "([D)V")))
  (fiveam:is (= 2 (jclass::interface-count "([[[Ljava/lang/Object;)V"))))
