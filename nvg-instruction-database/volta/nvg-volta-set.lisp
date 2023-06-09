(cl:in-package #:cluster)

;;; LOCK is annotative, it doesn't change the instruction descriptor
;;; that is used. We also don't have a way to show LOCK
;;; in code-commands in Cluster either at the moment.
#+ (or)
(define-modifier-prefix lock lock #xF0)
(define-modifier-prefix operand-size-override
  operand-size-override #x66)

(define-range-prefix rex (#x40 #x4F)
  (#b1000 rex.w))

(define-instruction-set nvg-volta
    (:modifier-prefixes (operand-size-override)
     :range-prefixes (rex)))

(defclass nvg-instruction-descriptor ()
  ((%mnemonic
    :initarg :mnemonic
    :reader mnemonic)
   (%operands
    :initarg :operands
    :reader operands)
   (%opcode
    :initarg :opcode
    :reader opcode)
   ;; (%opcode-extension
   ;;  :initform nil
   ;;  :initarg :opcode-extension
   ;;  :reader opcode-extension)
   (%encoding
    :initarg :encoding
    :reader encoding)
   (%negate-flags
    :initarg :negate-flags
    :reader negate-flags)
   (%reuse-flags
    :initarg :reuse-flags
    :reader reuse-flags)
   (%register-factors
    :initarg :register-factors
    :reader idesc-register-factors)
   (%predicate-factor
    :initarg :predicate-factor
    :reader idesc-predicate-factor)
   ;; (%lock
   ;;  :initform nil
   ;;  :initarg :lock
   ;;  :reader lock)
   ;; (%operand-size-override
   ;;  :initform nil
   ;;  :initarg :operand-size-override
   ;;  :reader operand-size-override)
   ;; (%rex.w
   ;;  :initform nil
   ;;  :initarg :rex.w
   ;;  :reader rex.w)
   ))

(defmacro define-instruction (mnemonic &key
                                         modes
                                         operands
                                         opcode
                                         ;; opcode-extension
                                         encoding
                                         negate-flags
                                         reuse-flags
                                         register-factors
                                         predicate-factor)
  `(let ((instruction-descriptor
           (make-instance 'nvg-instruction-descriptor
                          :mnemonic ,mnemonic
                          ;; :modes ',modes
                          :operands ',operands
                          :opcode ',opcode
                          ;; :opcode-extension ,opcode-extension
                          :encoding ',encoding
                          :negate-flags ',negate-flags
                          :reuse-flags ',reuse-flags
                          :register-factors ',register-factors
                          :predicate-factor ',predicate-factor)))
     (push instruction-descriptor
           (gethash ,mnemonic *instruction-descriptors*))
     ;; (push instruction-descriptor
     ;;       (aref *instruction-descriptors-by-first-opcode*
     ;;             ',opcode))
     ))


(defclass nv-gpr-operand (gpr-operand)
  ((%negated
    :initarg :negated
    :reader nv-gpr-negated
    :initform nil)
   (%reused
    :initarg :reused
    :reader nv-gpr-reused
    :initform nil)))

;;; This is used to determine if the mnemonic
;;; is an alias and the instructions are encoded
;;; exactly the same way
(defun instruction-descriptor-equal (desc1 desc2)
  (and
   ;; (equal (modes desc1)                 (modes desc2))
   (equal (operands desc1)              (operands desc2))
   (equal (opcodes desc1)               (opcodes desc2))
   ;; (equal (opcode-extension desc1)      (opcode-extension desc2))
   (equal (encoding desc1)              (encoding desc2))
   ;; (equal (lock desc1)                  (lock desc2))
   (equal (operand-size-override desc1) (operand-size-override desc2))
   ;; (equal (rex.w desc1)                 (rex.w desc2))
   ))

;; (defmacro define-instruction (mnemonic &key operands opcodes encoding)
;;   `(let ((instruction-descriptor
;;            (make-instance 'instruction-descriptor
;;                           :mnemonic ,mnemonic
;;                           :operands ',operands
;;                           :opcodes ',opcodes
;;                           :encoding ',encoding)))
;;      (push instruction-descriptor
;;            (gethash ,mnemonic *instruction-descriptors*))
;;      (push instruction-descriptor
;;            (aref *instruction-descriptors-by-first-opcode*
;;                  (first ',opcodes)))))


(defun encode-instruction (desc &rest operands)
  (let ((pref (the (unsigned-byte 64) 0))
        (inst (the (unsigned-byte 64) 0))
        (iobj (first (gethash (mnemonic desc)
                              *instruction-descriptors*)))
        (out-list))

    (flet ((add-bytes (input)
             (if (numberp input)
                 (incf inst input)
                 (when (listp input)
                   (incf pref (first input))
                   (incf inst (second input))))))

      (unless (loop :for op :in operands :always (typep op 'nv-gpr-operand))
        (error "All arguments must be NVidia GPR operands."))

      (add-bytes (opcode iobj))
      
      (loop :for op :in operands :for rgf :in (idesc-register-factors iobj)
            :for nf :in (negate-flags iobj) :for rf :in (reuse-flags iobj)
            :do (when (nv-gpr-negated op)
                  (if (numberp nf) (add-bytes nf)
                      (error "This operand cannot be negated.")))
                (when (nv-gpr-reused op)
                  (if (numberp rf) (add-bytes rf)
                      (error "This operand cannot be reused.")))
                (add-bytes (* rgf (code-number op))))
      
      (loop :for i :below 8 :do (push (ldb (byte 8 0) inst) out-list)
                                (setf inst (ash inst -8)))
      (loop :for i :below 8 :do (push (ldb (byte 8 0) pref) out-list)
                                (setf pref (ash pref -8)))
      out-list)))

(defun assemble (items)
  (let ((encodings (loop for item in items collect (apply #'encode-instruction
                                                          item (operands item)))))
    (coerce (reduce #'append encodings :from-end t)
            '(simple-array (unsigned-byte 8) (*)))))

#|
(defun encode-instruction (desc operands)
  (ecase (length operands)
    (0 (opcodes desc))
    (1 (encode-instruction-1 desc (first operands)))
    (2 (encode-instruction-2 desc (first operands) (second operands)))))
|#

;; (defmethod encode-instruction-1 (desc (operand label))
;;   ;; (let ((type (first (encoding desc))))
;;   ;; (ecase type
;;   ;;   (label
;;   ;;    (let* ((rex-p (rex.w desc)))
;;   `(;; ,@(if (operand-size-override desc) '(#x66) '())
;;     ;; ,@(if rex-p '(#x48) '())
;;     0 0 0 0
;;     ,@(opcodes desc)
;;     ;; ,@(encode-integer (- (gethash operand *addresses*)
;;     ;;                      *instruction-pointer*)
;;     ;;                   4)
;;     ))

;; (defmethod encode-instruction-2 (desc (operand1 nv-gpr-operand) (operand2 nv-gpr-operand))
;;   (let ((prefix (the (unsigned-byte 32) 0))
;;         (opcode (the (unsigned-byte 64) 0))
;;         (out-list))

;;     (loop :for i :below 8 :do (push (ldb (byte 8 0) opcode) out-list)
;;                               (setf opcode (ash opcode -8)))
;;     (loop :for i :below 3 :do (push (ldb (byte 8 0) prefix) out-list)
;;                               (setf prefix (ash prefix -8)))
;;     out-list))
  
;; (defmethod encode-instruction-2 (desc (operand1 nv-gpr-operand) (operand2 nv-gpr-operand))
;;   (print (list :dd desc))
;;   `( 0 0 0
;;        ,@(print (opcodes desc))
;;        0 0 0
;;        ,(code-number operand2)
;;        ,(code-number operand2)
;;        ,(code-number operand1)
;;        ))
