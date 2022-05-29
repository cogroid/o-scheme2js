(define-macro (test . L)
   `(begin
       ;(print ',@L)
       (write ,@L)
       (print)))

;; all tests from R5RS

(define y 28)
(test y)

(test (quote a))
(test (quote #(a b c)))
(test (quote (+ 1 2)))
(print)
(test 'a)
(test '#(a b c))
(test '())
(test '(+ 1 2))
(test '(quote a))
(test ''a)
(print)
(test '"abc")
(test "abc")
(test '145932)
(test 145932)
(test '#t)
(test #t)
(print)
(test (+ 3 4))
(test ((if #f + *) 3 4))
(print)
;; don't test unspecified output
;(test (lambda (x) (+ x x)))
(test ((lambda (x) (+ x x)) 4))
(print)
(define reverse-subtract
   (lambda (x y) (- y x)))
(test (reverse-subtract 7 10))
(define add4
   (let ((x 4))
      (lambda (y) (+ x y))))
(test (add4 6))
(print)
(test ((lambda x x) 3 4 6 7))
(test ((lambda (x y . z) z) 3 4 5 6))
(print)
(test (if (> 3 2) 'yes 'no))
(test (if (> 2 3) 'yes 'no))
(test (if (> 3 2)
	   (- 3 2)
	   (+ 3 2)))
(print)
(define x 2)
(test (+ x 1))
(set! x 4) ;; don't test, as comparison would be difficult for unspecified value.
(test (+ x 1))
(print)
(test (cond ((> 3 2) 'greater)
	     ((< 3 2) 'less)))
(test (cond ((> 3 3) 'greater)
	     ((< 3 3) 'less)
	     (else 'equal)))
(test (cond ((assv 'b '((a 1) (b 2))) => cadr)
	     (else #f)))
(print)
(test (case (* 2 3)
	  ((2 3 5 7) 'prime)
	  ((1 4 6 8 9) 'composite)))
;; don't test for unspecified
(case (car '(c d))
   ((a) 'a)
   ((b) 'b))
(test (case (car '(c d))
	  ((a e i o u) 'vowel)
	  ((w y) 'semivowel)
	  (else 'consonant)))
(print)
(test (and (= 2 2) (> 2 1)))
(test (and (= 2 2) (< 2 1)))
(test (and 1 2 'c '(f g)))
(test (and))
(print)
(test (or (= 2 2) (> 2 1)))
(test (or (= 2 2) (< 2 1)))
(test (or #f #f #f))
(test (or (memq 'b '(a b c))
	   (/ 3 0)))
(print)
(test (let ((x 2) (y 3))
	  (* x y)))
(test (let ((x 2) (y 3))
	  (let ((x 7)
		(z (+ x y)))
	     (* z x))))
(print)
(test (let ((x 2) (y 3))
	  (let* ((x 7)
		 (z (+ x y)))
	     (* z x))))
(print)
(test (letrec ((even?
		 (lambda (n)
		    (if (zero? n)
			#t
			(odd? (- n 1)))))
		(odd?
		 (lambda (n)
		    (if (zero? n)
			#f
			(even? (- n 1))))))
	  (even? 88)))
(print)