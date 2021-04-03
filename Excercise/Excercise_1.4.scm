;; Observe that our model of evaluation allows
;; for combinations whose operators are compound expres-
;; sions. Use this observation to describe the behavior of the
;; following procedure:

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(a-plus-abs-b 1 2)
(a-plus-abs-b 1 -2)

;; if b is positive use the plus operation with the parameters a and b
;; otherwise use the minus opperation
