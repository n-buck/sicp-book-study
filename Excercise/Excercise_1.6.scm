;; Alyssa P. Hacker doesn’t see why if needs to
;; be provided as a special form. “Why can’t I just deﬁne it as
;; an ordinary procedure in terms of cond ?” she asks. Alyssa’s
;; friend Eva Lu Ator claims this can indeed be done, and she
;; deﬁnes a new version of if :


(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

;; Eva demonstrates the program for Alyssa:

(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (average x y)
  (/ (+ x y) 2))


(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

;; Delighted, Alyssa uses new-if to rewrite the square-root
;; program:
(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

;; The difference is, that the built in if will not evaluate the last
;; else-statement, but the new-if will evaluate it!
;; Therefore it will at least make one iteration to much. In special-cases it may
;; even not return when the normal rqrt-iter would!
