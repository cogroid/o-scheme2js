(define-macro (test . L)
   `(begin
;       (print ',@L)
       (write ,@L)
       (print)))

;; all tests from R5RS

(define (part1)
   (test (+ 3 4))
   (test (+ 3))
   (test (+))
   (test (* 4))
   (test (*))
   (print)
   (test (- 3 4))
   (test (- 3 4 5))
   (test (- 3))
   (test (/ 3 4 5))
   (test (/ 3))
   (print)
   (test (abs -7))
   ;; scheme2js is known to be broken here...
   (test (modulo 13 4))
   (test (remainder 13 4))
   (test (modulo -13 4))
   (test (remainder -13 4))
   (test (modulo 13 -4))
   (test (remainder 13 -4))
   (test (modulo -13 -4))
   (test (remainder -13 -4))
   (print)
   (test (gcd 32 -36))
   (test (gcd))
   (test (lcm 32 -36))
   (test (lcm))
   ;; bigloo and scheme2js are known not to comply
   ;(print)
   ;(test (numerator (/ 6 4)))
   ;(test (denominator (/ 6 4)))
   (print)
   (test (floor -4.3))
   (test (ceiling -4.3))
   (test (truncate -4.3))
   (test (round -4.3))
   (test (floor 3.5))
   (test (ceiling 3.5))
   (test (truncate 3.5))
   (test (round 3.5))
   ;; known not to work
   ;(test (round 7/2))
   (test (round 7))
   ;; rationalize known not to work
   (test (string->number "100"))
   (test (string->number "100" 16))
   (test (string->number "1e2"))
   ;; known not to work
   ;(test (string->number "15##"))
   (test #t)
   (test #f)
   (test '#f)
   (print)
   (test (not #t))
   (test (not 3))
   (test (not (list 3)))
   (test (not #f))
   (test (not '()))
   (test (not (list)))
   (test (not 'nil))
   (print)
   (test (boolean? #f))
   (test (boolean? 0))
   (test (boolean? '()))
   (print)
   )
(part1)

(define x (list 'a 'b 'c))
(define y x)
(test y)
(test (list? y))
(set-cdr! x 4)
(test x)
(test (eqv? x y))
(test y)
(test (list? y))
(set-cdr! x x)
(test (list? x))
(test (list? '(1)))
(test (list? '(1 2)))
(test (list? '((1 2) (3 4))))
(test (list? '(1 2 3 . 4)))
(test (list? 5))
(print)
(test (pair? '(a . b)))
(test (pair? '(a b c)))
(test (pair? '()))
(test (pair? '#(a b)))
(print)
(test (cons 'a '()))
(test (cons '(a) '(b c d)))
(test (cons "a" '(b c)))
(test (cons 'a 3))
(test (cons '(a b) 'c))
(print)
(test (car '(a b c)))
(test (car '((a) b c d)))
(test (car '(1 . 2)))
(test (with-handler
       (lambda (exception)
	  "expected exception")
       (car '())
       "BAD BAD (no exception)"))
(print)
(test (cdr '((a) b c d)))
(test (cdr '(1 . 2)))
(test (with-handler
       (lambda (exception)
	  write "expected exception")
       (cdr '())
       "BAD BAD (no exception)"))
(print)
; difficult to test
;(define (f) (list 'not-a-constant-list))
;(define (g) '(constant-list))
;(test (set-car! (f) 3))
;(test (set-car! (g) 3))
(test (list? '(a b c)))
(test (list? '()))
(test (list? '(a . b)))
(test (let ((x (list 'a)))
	 (set-cdr! x x)
	 (list? x)))
(print)
(test (list 'a (+ 3 4) 'c))
(test (list))
(print)
(test (length '(a b c)))
(test (length '(a (b) (c d e))))
(test (length '()))
(print)
(test (append '(x) '(y)))
(test (append '(a) '(b c d)))
(test (append '(a (b)) '((c))))
(test (append '(a b) '(c . d)))
(test (append '() 'a))
(print)
(test (reverse '(a b c)))
(test (reverse '(a (b c) d (e (f)))))
(print)
(test (list-ref '(a b c d) 2))
(print)
(test (memq 'a '(a b c)))
(test (memq 'b '(a b c)))
(test (memq 'a '(b c d)))
(test (memq (list 'a) '(b (a) c)))
(test (member (list 'a)
	      '(b (a) c)))
;; don't test unspecified
(memq 101 '(100 101 102))
(test (memv 101 '(100 101 102)))
(print)

(define (part3)
   (define e '((a 1) (b 2) (c 3)))
   (test (assq 'a e))
   (test (assq 'b e))
   (test (assq 'd e))
   (test (assq (list 'a) '(((a)) ((b)) ((c)))))
   (test (assoc (list 'a) '(((a)) ((b)) ((c)))))
   ;; don't test unspecified
   (assq 5 '((2 3) (5 7) (11 13)))
   (test (assv 5 '((2 3) (5 7) (11 13))))
   (print)
   (test (symbol? 'foo))
   (test (symbol? (car '(a b))))
   (test (symbol? "bar"))
   (test (symbol? 'nil))
   (test (symbol? '()))
   (test (symbol? #f))
   (print)
   (test (symbol->string 'flying-fish))
   (test (symbol->string 'Martin))
   (test (symbol->string
	  (string->symbol "Malvina")))
   (print)
   ;; scheme2js is known to fail here
   ;(test (eq? 'mISSISSIppi 'mississippi))
   (test (string->symbol "mISSISSIppi"))
   (test (eq? 'bitBlt (string->symbol "bitBlt")))
   (test (eq? 'JollyWog
	      (string->symbol
	       (symbol->string 'JollyWog))))
   (test (string=? "K. Harper, M.D."
		   (symbol->string
		    (string->symbol "K. Harper, M.D."))))
   (print)
   )
(part3)

(define (part4)
   (define (f) (make-string 3 #\*))
   (define (g) "***")
   ; don't test string-sets, as they are not implemented with all scheme2js flags
   ; and often are either unspecified or throw errors
   ;(string-set! (f) 0 #\?)
   ;(string-set! (g) 0 #\?)
   ;(string-set! (symbol->string 'immutable)
   ;             0
   ;             #\?)
   (test (vector 'a 'b 'c))
   (test (vector-ref '#(1 1 2 3 5 8 13 21)
		     5))
   (test (vector-ref '#(1 1 2 3 5 8 13 21)
		     (let ((i (round (* 2 (acos -1)))))
			(if (inexact? i)
			    (inexact->exact i)
			    i))))
   (print)
   (test (let ((vec (vector 0 '(2 2 2 2) "Anna")))
	    (vector-set! vec 1 '("Sue" "Sue"))
	    vec))
   ;; don't test for constant vector error
   ;(test (vector-set! '#(0 1 2) 1 "doe"))
   (print)
   (test (vector->list '#(dah dah didah)))
   (test (list->vector '(dididit dah)))
   (print)
   (test (procedure? car))
   (test (procedure? 'car))
   (test (procedure? (lambda (x) (* x x))))
   (test (procedure? '(lambda (x) (* x x))))
   ;; known not to work yet.
   ;; TODO: add call/cc
   ;(test (call-with-current-continuation procedure?))
   )
(part4)
