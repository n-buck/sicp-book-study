;; Define a procedure that takes three numbers as arguments and returns the sum
;; of the squares of the two larger numbers.

(define (sum_of_squares a b)
  (+ (* a a) (* b b)))

(define (sum_of_two_bigger_squares a b c)
  (cond ((and (> a c) (> b c))	(sum_of_squares a b))
        ((and (> a b) (< b c))	(sum_of_squares a c))
        (else (sum_of_squares b c))))

(sum_of_squares 2 3)
(sum_of_two_bigger_squares 1 2 3)
