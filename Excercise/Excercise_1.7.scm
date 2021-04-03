;; The good-enough? test used in computing
;; square roots will not be very eﬀective for ﬁnding the square
;; roots of very small numbers. Also, in real computers, arith-
;; metic operations are almost always performed with lim-
;; ited precision. is makes our test inadequate for very large
;; numbers. Explain these statements, with examples showing
;; how the test fails for small and large numbers. An alterna-
;; tive strategy for implementing good-enough? is to watch
;; how guess changes from one iteration to the next and to
;; stop when the change is a very small fraction of the guess.
;; Design a square-root procedure that uses this kind of end
;; test. Does this work beer for small and large numbers?


(define (improve guess x)
  (average guess (/ x guess)))

;; allow percentual error
(define (good-enough? guess x)
  (< (abs (- (/ (square guess) x) 1)) 0.001))

;; (define (good-enough? guess x)
;;   (< (abs (- (square guess) x)) 0.001))

(define (average x y)
  (/ (+ x y) 2))


(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

