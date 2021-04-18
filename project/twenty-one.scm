(load '(
        "../simply_scheme/simply.scm"
        "../simply_scheme/database.scm"
        "../simply_scheme/functions.scm"
        "../simply_scheme/match.scm"
        "../simply_scheme/newttt.scm"
        "../simply_scheme/spread.scm"
        "../simply_scheme/ttt.scm"
        ))


(define (twenty-one strategy)
  (define (play-dealer customer-hand dealer-hand-so-far rest-of-deck)
    (cond ((> (best-total dealer-hand-so-far) 21) 1)
	  ((< (best-total dealer-hand-so-far) 17)
	   (play-dealer customer-hand
			(se dealer-hand-so-far (first rest-of-deck))
			(bf rest-of-deck)))
	  ((< (best-total customer-hand) (best-total dealer-hand-so-far)) -1)
	  ((= (best-total customer-hand) (best-total dealer-hand-so-far)) 0)
	  (else 1)))

  (define (play-customer customer-hand-so-far dealer-up-card rest-of-deck)
    (cond ((> (best-total customer-hand-so-far) 21) -1)
	  ((strategy customer-hand-so-far dealer-up-card)
	   (play-customer (se customer-hand-so-far (first rest-of-deck))
			  dealer-up-card
			  (bf rest-of-deck)))
	  (else
	   (play-dealer customer-hand-so-far
			(se dealer-up-card (first rest-of-deck))
			(bf rest-of-deck)))))

  (let ((deck (make-deck)))
    (play-customer (se (first deck) (first (bf deck)))
		   (first (bf (bf deck)))
		   (bf (bf (bf deck))))) )

(define (make-ordered-deck)
  (define (make-suit s)
    (every (lambda (rank) (word rank s)) '(A 2 3 4 5 6 7 8 9 10 J Q K)) )
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C)) )

(define (make-deck)
  (define (shuffle deck size)
    (define (move-card in out which)
      (if (= which 0)
	  (se (first in) (shuffle (se (bf in) out) (- size 1)))
	  (move-card (bf in) (se (first in) out) (- which 1)) ))
    (if (= size 0)
	deck
    	(move-card deck '() (random size)) ))
  (shuffle (make-ordered-deck) 52) )

(define (max-card-value? card)
  (cond
   ((number? (first card)) (first card))
   ((equal? 'a (first card)) 11)
   (else 10)))

(define (best-total hand)
  (define (number-of-aces? hand)
    (if (empty? hand)
	0
	(if (equal? 'a (first (first hand)))
	    (+ 1 (number-of-aces? (bf hand)))
	    (number-of-aces? (bf hand)))))
  (define (max-value? hand)
    (if (empty? hand)
	0
	(+ (max-card-value? (first hand)) (max-value? (bf hand)))))
  (define (iter value aces)
    (if (and (> value 21) (> aces 0))
	(iter (- value 10) (- aces 1))
	value))
  (iter (max-value? hand) (number-of-aces? hand)))
(best-total '(ah 3h))
(best-total '(ad 8s))
(best-total '(ad 8s 5h))
(best-total '(ad as 9h))
(best-total '(7d 7s 9h))



	
(define (stop-at-17 hand dealer-card)
  (> 17 (best-total hand)))
(twenty-one stop-at-17)

(define (play-n strategy n)
  (define (iter state count)
    (if (>= count n)
	state
	(iter
	 (+ state (twenty-one strategy))
	 (+ count 1))))
  (iter 0 0))

(play-n stop-at-17 100)

(define (dealer-sensitive hand dealer-card)
  (or (and (stop-at-17 hand dealer-card)
	   (>= (max-card-value? dealer-card) 7))
      (< (best-total hand) 12)))
(dealer-sensitive '(ah 3h) 'ah)

(play-n dealer-sensitive 100)

(define (stop-at n)
  (lambda (hand dealer-card)
    (> n (best-total hand))))

(define (valentine hand dealer-card)
  (define (has-heart xs)
    (cond ((empty? xs) #f)
	  ((equal? (first (bf (first xs))) 'h) #t)
	  (else (has-heart (bf xs)))))
  (if (has-heart hand)
      ((stop-at 17) hand dealer-card)
      ((stop-at 19) hand dealer-card)))

(play-n valentine 100)

