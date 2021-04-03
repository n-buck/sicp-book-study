;; Newton’s method for cube roots is based on
;; the fact that if y is an approximation to the cube root of x,
;; then a beer approximation is given by the value
;; x/y^2 + 2y
;; ----------
;;     3
;; Use this formula to implement a cube-root procedure anal-
;; ogous to the square-root procedure. (In Section 1.3.4 we will
;; see how to implement Newton’s method in general as an
;; abstraction of these square-root and cube-root procedures.)


(define (improve y x)
  (/ (+ (/ x (* y y)) (* 2 y))
     3))

(define (good-enough? guess x)
  (< (abs (- (/ (* guess guess guess) x) 1)) 0.01))

(define (cube-root-iter guess x)
  (if (good-enough? guess x)
    guess
    (cube-root-iter (improve guess x) x)))

(+ 2 3)
(cube-root-iter 1.0 27)

