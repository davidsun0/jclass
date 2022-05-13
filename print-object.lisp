;;; Making the objects a bit easier on the eyes.

(in-package #:jclass)

(defmethod print-object ((object java-class) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (name object))))

(defmethod print-object ((object field-info) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A ~A" (name object) (descriptor object))))

(defmethod print-object ((object method-info) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A ~A" (name object) (descriptor object))))
