(load '(
        "../simply_scheme/simply.scm"
	"../simply_scheme/database.scm"
        "../simply_scheme/functions.scm"
        "../simply_scheme/match.scm"
        "../simply_scheme/newttt.scm"
        "../simply_scheme/spread.scm"
        "../simply_scheme/ttt.scm"
	))
;; 1. Abelson & Sussman, exercises 1.31(a), 1.32(a), 1.33, 1.40, 1.41, 1.43, 1.46
;; (Pay attention to footnote 51; you’ll need to know the ideas in these exercises later in the
;; semester.)


;; 1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
     result
     (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))
(sum-cubes 1 10)
;; 3025


;; 1.31
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
     result
     (iter (next a) (* (term a) result))))
  (iter a 1))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (factorial a b)
  (product (lambda (x) x) a inc b))
(factorial 1 10)

(define (approx_pi a b)
  (* 4 (product
   (lambda (x)
     (if (even? x)
	 (/ x (+ x 1))
	 (/ (+ x 1) x)))
   a
   inc
   b)))
(approx_pi 2.0 1000.0)

;; 1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))
(define (product term a next b)
  (accumulate * 1 term a next b))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(approx_pi 2.0 1000.0)
(sum-cubes 1 10)

;; 1.33
(define (filtered-accumulate filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (if (filter a)
	  (combiner (term a) (filtered-accumulate filter combiner null-value  term (next a) next b))
	  (filtered-accumulate filter combiner null-value term (next a) next b))))
  
(define (product term a next b)
  (filtered-accumulate even? * 1 term a next b))

(define (sum term a next b)
  (filtered-accumulate even? + 0 term a next b))

(approx_pi 2.0 1000.0)
(sum-cubes 1 10)

;; 1.40

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(define dx 0.00001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define (cube x) (* x x x))
((deriv cube) 5)

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
(define (sqrt x)
  (newtons-method
   (lambda (y) (- (square y) x)) 1.0))
(sqrt 9)

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))
(newtons-method (cubic 1 1 1) 1)

;; 1.41
(define (double f)
  (lambda (n) (f (f n))))
(((double (double double)) inc) 5)
(((double double) (double inc)) 5)

;; 1.43
(define (repeated f n)
  (lambda (x)
    (if (> n 0)
	((repeated f (- n 1)) (f x))
	x
	)))
((repeated square 2) 5)
;; 625

;; 1.46
(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
	guess
        (iter (improve guess))))
  (lambda (guess) (iter guess)))
     
(define (sqrt x)
  (define (improve guess)
    (/ (+ guess (/ x guess)) 2))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.0001))
  ((iterative-improve good-enough? improve) 1.0))
(sqrt 9)

   
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

;; 2. Last week you wrote procedures squares, that squared each number in its argument
;; sentence, and saw pigl-sent, that pigled each word in its argument sentence. Generalize
;; this pattern to create a higher-order procedure called every that applies an arbitrary
;; procedure, given as an argument, to each word of an argument sentence. This procedure
;; is used as follows:
(every square '(1 2 3 4))
;; (1 4 9 16)
(every first '(nowhere man))
;; (n m)

;; 3. Our Scheme library provides versions of the every function from the last exercise and
;; the keep function shown in lecture. Get familiar with these by trying examples such as
;; the following:
(every (lambda (letter) (word letter letter)) 'purple)
(every (lambda (number) (if (even? number) (word number number) number))
       '(781 5 76 909 24))
(keep even? '(781 5 76 909 24))
(keep (lambda (letter) (member? letter 'aeiou)) 'bookkeeper)
(keep (lambda (letter) (member? letter 'aeiou)) 'syzygy)
(keep (lambda (letter) (member? letter 'aeiou)) '(purple syzygy))
(keep (lambda (wd) (member? 'e wd)) '(purple syzygy))

;; Extra for experts:
;; In principle, we could build a version of Scheme with no primitives except lambda. Every-
;; thing else can be defined in terms of lambda, although it’s not done that way in practice
;; because it would be so painful. But we can get a sense of the flavor of such a language by
;; eliminating one feature at a time from Scheme to see how to work around it.
;; In this problem we explore a Scheme without define. We can give things names by using
;; argument binding, as let does, so instead of
(define (sumsq a b)
  (define (square x) (* x x))
  (+ (square a) (square b)))
(sumsq 3 4)
;; we can say
((lambda (a b)
   ((lambda (square)
      (+ (square a) (square b)))
    (lambda (x) (* x x))))
 3 4)

;; This works fine as long as we don’t want to use recursive procedures. But we can’t replace
(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))
(fact 5)
;; by
((lambda (n)
   (if ...))
 5)

;; because what do we do about the invocation of fact inside the body?
;; Your task is to find a way to express the fact procedure in a Scheme without any way to
;; define global names.

((lambda (n)
   (let ((f (lambda (func x acc)
	      (if (= 0 x)
		  acc
		  (func func (- x 1) (* x acc))))))
     (f f 5 1)))
 5)

(((lambda (f)
    (lambda (n) (f f n)))
  (lambda (fun x)
    (if (equal? 0 x)
	1
	(* x (fun fun (- x 1))))))
 5)



;; Unix feature of the week: pine, mail, firefox
;; Emacs feature of the week: M-x info, C-x u (undo)
