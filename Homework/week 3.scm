(load '(
        "../simply_scheme/simply.scm"
        "../simply_scheme/database.scm"
        "../simply_scheme/functions.scm"
        "../simply_scheme/match.scm"
        "../simply_scheme/newttt.scm"
        "../simply_scheme/spread.scm"
        "../simply_scheme/ttt.scm"
        ))
;; 1. Abelson & Sussman, exercises 1.16, 1.35, 1.37, 1.38

;; Exercise 1.16: Design a procedure that evolves an itera-
;; tive exponentiation process that uses successive squaring
;; and uses a logarithmic number of steps, as does fast-expt.
;; (Hint: Using the observation that (b n/2 )2 = (b 2 )n/2 , keep,
;; along with the exponent n and the base b, an additional
;; state variable a, and deﬁne the state transformation in such
;; a way that the product ab n is unchanged from state to state.
;; At the beginning of the process a is taken to be 1, and the
;; answer is given by the value of a at the end of the process.
;; In general, the technique of deﬁning an invariant quantity
;; that remains unchanged from state to state is a powerful
;; way to think about the design of iterative algorithms.)

(define (even? n)
  (= (remainder n 2) 0))
(define (expt b n)
  (expt-iter b n 1 0))
(define (expt-iter b n product counter)
  (if (= counter n)
      product
      (if (and (not (= 0 counter)) (> n (* 2 counter)))
	  (expt-iter b n (* product product) (* 2 counter))
	  (expt-iter b n (* b product) (+ counter 1)))))
(expt 2 1000)

;; Exercise 1.35: Show that the golden ratio ϕ (Section 1.2.2)
;; is a fixed point of the transformation x → 1 + 1/x , and
;; use this fact to compute ϕ by means of the fixed-point
;; procedure.

;; f(x) = x --> Fix-Point
;; x^2 = x + 1
(fixed-point (lambda (x) (+ 1(/ 1 x))) 1)


;; Exercise 1.37:
;; a. An infnite continued fraction is an expression of the
;; form
;;          N1
;;  f =--------------
;;              N2
;;    D1 + -----------
;;                N3
;;      D2 + --------
;;        D3 + . . .
;; As an example, one can show that the inﬁnite con-
;; tinued fraction expansion with the Ni and the Di all
;; equal to 1 produces 1/ϕ, where ϕ is the golden ratio
;; (described in Section 1.2.2). One way to approximate
;; an inﬁnite continued fraction is to truncate the expan-
;; sion aer a given number of terms. Such a truncation—
;; a so-called k-term ﬁnite continued fraction—has the form
;;          N1
;; -----------------
;; D1 + ------------
;;     .
;;      .     Nk
;;       . + ----
;;            Dk

;; Suppose that n and d are procedures of one argument
;; (the term index i) that return the Ni and Di of the
;; terms of the continued fraction. Deﬁne a procedure
;; cont-frac such that evaluating (cont-frac n d k)
;; computes the value of the k-term ﬁnite continued frac-
;; tion. Check your procedure by approximating 1/ϕ
;; using
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 12)
(define (cont-frac d n k)
  (define (helper depth)
    (if (= depth k)
	(/ (n depth) (d depth))
	(/ (n depth) (+ (d depth) (helper (+ depth 1))))))
  (/ (n 1) (+ (d 1) (helper 2))))

;; for successive values of k. How large must you make
;; k in order to get an approximation that is accurate to
;; 4 decimal places?
;; -> 12

;; b. If your cont-frac procedure generates a recursive pro-
;; cess, write one that generates an iterative process. If
;; it generates an iterative process, write one that gen-
;; erates a recursive process.
(define (cont-frac d n k depth)
  (if (= k depth)
      (/ (n depth) (d depth))
      (/ (n depth) (+ (d depth) (cont-frac d n k (+ depth 1))))))
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 12 1)

;; Exercise 1.38:
;; In 1737, the Swiss mathematician Leonhard
;; Euler published a memoir De Fractionibus Continuis, which
;; included a continued fraction expansion for e − 2, where
;; e is the base of the natural logarithms. In this fraction, the
;; Ni are all 1, and the Di are successively 1, 2, 1, 1, 4, 1, 1,
;; 6, 1, 1, 8, . . .. Write a program that uses your cont-frac
;; procedure from Exercise 1.37 to approximate e, based on
;; Euler’s expansion.
(define (Di x)
  (if (= (remainder (- x 2) 3) 0)
      (+ 2 (* (/ (- x 2) 3) 2))
      1))
(define (euler depth)
  (+ 2 (cont-frac
   Di
   (lambda (i) 1.0)
   depth
   1)))
(euler 20)

;; 2. A “perfect number” is defined as a number equal to the sum of all its factors less than
;; itself. For example, the first perfect number is 6, because its factors are 1, 2, 3, and 6,
;; and 1+2+3=6. The second perfect number is 28, because 1+2+4+7+14=28. What is
;; the third perfect number? Write a procedure (next-perf n) that tests numbers starting
;; with n and continuing with n+1, n+2, etc. until a perfect number is found. Then you
;; can evaluate (next-perf 29) to solve the problem. Hint: you’ll need a sum-of-factors
;; subprocedure.
(define (devidors x)
  (define (get-devidors devidor devidors)
    (if (= devidor 0)
	devidors
	(if (= (remainder x devidor) 0)
	    (get-devidors (- devidor 1) (cons devidor devidors))
	    (get-devidors (- devidor 1) devidors))))
  (get-devidors (- x 1) '()))
(devidors 15)
  
(define (sum-of-devidors x)
  (define (add xs sum)
    (if (empty? xs)
	sum
	(add (bf xs) (+ sum (first xs)))))
  (add (devidors x) 0))
(sum-of-devidors 123)

(define (next-perfect x)
  (if (= x (sum-of-devidors x))
      x
      (next-perfect (+ 1 x))))
(next-perfect 29)

;; [Note: If you run this program when the system is heavily loaded, it may take half an hour
;; to compute the answer! Try tracing helper procedures to make sure your program is on
;; track, or start by computing (next-perf 1) and see if you get 6.]

;; 3. Explain the effect of interchanging the order in which the base cases in the cc procedure
;; on page 41 of Abelson and Sussman are checked. That is, describe completely the set of
;; arguments for which the original cc procedure would return a different value or behave
;; differently from a cc procedure coded as given below, and explain how the returned values
;; would differ.
(define (cc amount kinds-of-coins)
  (cond
   ((or (< amount 0) (= kinds-of-coins 0)) 0)
   ((= amount 0) 1)
   (else ... ) ) )
					; as in the original version
;; Changing the order shall not impact the result, but may have an impact of the runtime:
;; if the smal numbers are taken into account first, one will end up with more calls that
;; try to subtract i.e. 50 from 2...
;;
;; 6 --> 5 (1)
;;       |--> 5 (-4) --> 0
;;       |--> 2 (-1) --> 0
;;       |--> 1 (0) --> 1
;; 6 --> 2 (4)
;;       |--> 2 (2)
;;       |    |--> 2 (0) 1
;;       |    |--> 1 (1)
;;       |    |    | --> 1 (0) 1
;;       |--> 1 (3)
;;       |    |--> 1 (2)
;;       |    |    |--> 1 (1)
;;       |    |    |    |--> 1 (0) 1
;; 6 --> 1 (5)
;;       |--> 1 (4)
;;       |    |--> 1 (3)
;;       |    |    |    |--> 1 (2)
;;       |    |    |    |    |--> 1 (1)
;;       |    |    |    |    |    |--> 1 (0) 1

;; 6 --> 1 (5)
;;       |--> 1 (4)
;;       |    |--> 1 (3)
;;       |    |    |    |--> 1 (2)
;;       |    |    |    |    |--> 1 (1)
;;       |    |    |    |    |    |--> 1 (0) 1
;;       |    |    |    |    |    2 (-1) 0
;;       |    |    |    |    |    5 (-4) 0
;;       |    |    |    |    2 (0) 1
;;       |    |    |    |    5 (-3) 0
;;       |    |    |--> 2 (1)
;;       |    |    |    |    |--> 1 (0) 1
;;       |    |    |    |    |--> 2 (-1) 0
;;       |    |    |    |    |--> 5 (-4) 0
;;       |    |    |--> 5 (-4) 0
;;       |    |--> 2 (1)
;;       |    |    |--> 1 (0) 1
;;       |    |    |--> 2 (-1) 0
;;       |    |    |--> 5 (-4) 0
;; .....
;; We see that we have a lot more calculations, that return 0.


;; 4. Give an algebraic formula relating the values of the parameters b, n, counter, and
;; product of the expt and exp-iter procedures given near the top of page 45 of Abelson
;; and Sussman. (The kind of answer we’re looking for is “the sum of b, n, and counter times
;; product is always equal to 37.”)
;; ???

;; Extra for Experts
;; 1. The partitions of a positive integer are the different ways to break the integer into
;; pieces. The number 5 has seven partitions:
;; 5       (one piece)
;; 4, 1      (two pieces)
;; 3, 2      (two pieces)
;; 3, 1, 1     (three pieces)
;; 2, 2, 1     (three pieces)
;; 2, 1, 1, 1    (four pieces)
;; 1, 1, 1, 1, 1   (five pieces)


;; The order of the pieces doesn’t matter, so the partition 2, 3 is the same as the partition
;; 3, 2 and thus isn’t counted twice. 0 has one partition.
;; Write a procedure number-of-partitions that computes the number of partitions of its
;; nonnegative integer argument.

;; 2. Compare the number-of-partitions procedure with the count-change procedure by
;; completing the following statement:
;; Counting partitions is like making change, where the coins are ...

;; 3. (Much harder!) Now write it to generate an iterative process; every recursive call must
;; be a tail call.
;; Unix feature of the week: mkdir, cd, pwd, ls
;; Emacs feature of the week: C-M-f, C-M-b, C-M-n, C-M-p (move around Scheme code)
