(in-package #:cluster)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mnemonic MOV

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To an 8-bit GPR or memory location (destination), move the
;;; contents of an 8-bit GPR (source).
;;;
;;; Opcodes: 88

;;; To a 32-bit GPR (destination), move the contents of a 32-bit
;;; GPR (source).
(define-instruction "IADD"
  :operands ((gpr 32) (gpr 32) (gpr 32))
  :opcode #x5c10000000000000
  :negate-flags (nil
                 #x2000000000000
                 #x1000000000000)
  :reuse-flags (nil
                (#x20000 0) ; 000000000000000
                (#x40000 0) ; 000000000000000
                )
  :predicate-factor #x10000
  :register-factors (     #x1
                        #x100
                     #x100000)
  :encoding (modrm reg)
  )

(define-instruction "IADD3"
  :operands ((gpr 32) (gpr 32) (gpr 32) (gpr 32))
  :opcode (#x7ffe000 0)
  :negate-flags (nil
                 #x1000000000000000000
                    #x8000000000000000
                 #x8000000000000000000)
  :reuse-flags (nilx
                 #x4000000000000000000000000000000
                 #x8000000000000000000000000000000
                #x10000000000000000000000000000000)
  :predicate-factor #x1000
  :register-factors (    #x10000
                       #x1000000
                     #x100000000
             #x10000000000000000)
  :encoding (modrm reg)
  )
