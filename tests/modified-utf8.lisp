;;;; jclass unit and integration tests
(in-package #:jclass/tests)
(fiveam:in-suite all-tests)

;;; Modified UTF-8 tests

(fiveam:test ascii-tests
  ;; Check that char-code and code-char use ASCII.
  (fiveam:is (= 97 (char-code #\a)))
  (fiveam:is (char= #\a (code-char 97)))
  (fiveam:is (equal '(97 98 99) (jclass:encode-modified-utf8 "abc")))
  ;; String concatenation with invokedynamic produces strings with unprintable
  ;; characters.
  (let ((utf-str (jclass:decode-modified-utf8 #(1 58 49))))
    (fiveam:is (= 3 (length utf-str)))
    (fiveam:is (= 1 (char-code (aref utf-str 0))))))

(defun encode-char (char)
  (jclass:encode-modified-utf8 (string char)))

(fiveam:test unicode-encoding
  ;; Check that Unicode is preserved.
  (fiveam:is (= #x5496 (char-code (code-char #x5496))))
  ;; Chars with two byte UTF-8 encoding
  (fiveam:is (equal '(#xC2 #xA2)
		    (encode-char (code-char #xA2))))
  ;; Chars with three byte UTF-8 encoding
  (fiveam:is (equal '(#xE0 #xA4 #xB9)
		    (encode-char (code-char #x939))))
  ;; Astral plane characters are first broken into UTF-16 surrogate pairs:
  ;; U+10348 is broken into #xD800 and #xDF48.
  (fiveam:is (equal '(#xED #xA0 #x80 #xED #xBD #x88)
		    (encode-char (code-char #x10348)))))

(defun decode-first-char (bytes)
  (let* ((string (jclass:decode-modified-utf8 bytes)))
    (aref string 0)))

(fiveam:test unicode-decoding
  ;; Modified UTF-8 encodings must be 1, 2, 3, or 6 bytes long.
  (fiveam:is (char= #\a (decode-first-char #(97))))
  (fiveam:is (= #xA2 (char-code (decode-first-char #(#xC2 #xA2)))))
  (fiveam:is (= #x939 (char-code (decode-first-char #(#xE0 #xA4 #xB9)))))
  (fiveam:is (= #x10348 (char-code (decode-first-char
				    #(#xED #xA0 #x80 #xED #xBD #x88))))))
