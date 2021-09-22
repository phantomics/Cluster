(cl:in-package #:cluster)

(defclass data-command (command)
  ((%data-bytes :initarg :data-bytes :reader data-bytes)))

(defmethod compute-encoding ((item data-command))
  (data-bytes item))

;;; Return a list of SIZE integers of type (UNSIGNED-BYTE 8) making up
;;; the representation of VALUE in a little-endian encoding, i.e., the
;;; bytes in the resulting list are ordered from least to most
;;; significant.
(defun encode-integer (value size)
  (loop for position from 0 by 8
	repeat size
	collect (ldb (byte 8 position) value)))

(defmethod preliminary-size ((item data-command))
  (length (data-bytes item)))
