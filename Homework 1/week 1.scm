(load '(
        "../simply_scheme/simply.scm"
	"../simply_scheme/database.scm"
        "../simply_scheme/functions.scm"
        "../simply_scheme/match.scm"
        "../simply_scheme/newttt.scm"
        "../simply_scheme/spread.scm"
        "../simply_scheme/ttt.scm"
	))
;; 2.
;; Write a procedure squares that takes a sentence of numbers as its argument and
;; returns a sentence of the squares of the numbers:

(define (squares xs)
  (define (square x)
    (* x x))
  (if (empty? xs)
      '()
      (cons
       (square (first xs))
       (squares (bf xs)))))

(squares ’(2 3 4 5))

;; 3.
;; Write a procedure switch that takes a sentence as its argument and returns a sentence
;; in which every instance of the words I or me is replaced by you, while every instance of
;; you is replaced by me except at the beginning of the sentence, where it’s replaced by I.
;; (Don’t worry about capitalization of letters.) Example:

(switch '(You told me that I should wake you up))

(define (switch xs)
  (define (replace x first)
    (define (isIOrMe x first)
      (if first
	  (or (equal? x 'i) (equal? x 'I))
	  (or (equal? x 'me) (equal? x 'Me))))
    (define (isYou x)
      (or (equal? x 'you) (equal? x 'You)))

    (cond
     ((isIOrMe x first)  'you)
     ((isYou x) (if first 'I 'me ))
     (else x)))

  (define (switch-iter xs isfirst)
    (if (empty? xs)
	'()
	(cons
	 (replace (first xs) isfirst)
	 (switch-iter (bf xs) false))))

  (switch-iter xs true))

;; 4.
;; Write a predicate ordered? that takes a sentence of numbers as its argument and
;; returns a true value if the numbers are in ascending order, or a false value otherwise.

(define (ordered xs)
  (if (or (empty? xs) (empty? (bf xs)))
      #t
      (if (> (first xs) (first (bf xs)))
	     #f
	     (ordered (bf xs)))))

;; 5.
;; Write a procedure ends-e that takes a sentence as its argument and returns a sentence
;; containing only those words of the argument whose last letter is E:
(define (ends-e xs)
  (define (filter predicate aggregates xs)
    (if (empty? xs)
	aggregates
	(if (predicate (first xs))
	    (filter predicate (cons (first xs) aggregates) (bf xs))
	    (filter predicate aggregates (bf xs)))))
  (define (predicate x)
    (equal? 'e (last x)))
  (filter predicate '() xs))

(ends-e '(please put the salami above the blue elephant))
;; (please the above the blue)

;; 6.
;; Most versions of Lisp provide and and or procedures like the ones on page 19. In
;; principle there is no reason why these can’t be ordinary procedures, but some versions of
;; Lisp make them special forms. Suppose, for example, we evaluate
(or (= x 0) (= y 0) (= z 0))
;; If or is an ordinary procedure, all three argument expressions will be evaluated before or
;; is invoked. But if the variable x has the value 0, we know that the entire expression has
;; to be true regardless of the values of y and z. A Lisp interpreter in which or is a special
;; form can evaluate the arguments one by one until either a true one is found or it runs out
;; of arguments.
;; Your mission is to devise a test that will tell you whether Scheme’s and and or are special
;; forms or ordinary functions. This is a somewhat tricky problem, but it’ll get you thinking
;; about the evaluation process more deeply than you otherwise might.
;; Why might it be advantageous for an interpreter to treat or as a special form and evaluate
;; its arguments one at a time? Can you think of reasons why it might be advantageous to
;; treat or as an ordinary function?

;;
(define (X)
  (X))
;; test will run for ever if or is no special form.
(or #t (X))
;; test will run for ever if and is no special form.
(and #f (X))
;; The speed is better if and/or is a special form
;; Adv. if no special form: Maybe some bugs are detectet more easy, since the path is always evaluated.

;; Unix feature of the week: man
;; Emacs feature of the week: C-g, M-x apropos
;; There will be a “feature of the week” each week. These first features come first because they
;; are the ones that you use to find out about the other ones: Each provides documentation
;; of a Unix or Emacs feature. This week, type man man as a shell command to see the Unix
;; manual page on the man program. Then, in Emacs, type M-x (that’s meta-X, or ESC X if
;; you prefer) describe-function followed by the Return or Enter key, then apropos to see
;; how the apropos command works. If you want to know about a command by its keystroke
;; form (such as C-g) because you don’t know its long name (such as keyboard-quit), you
;; can say M-x describe-key then C-g.
;; You aren’t going to be tested on these system features, but it’ll make the rest of your life
;; a lot easier if you learn about them
