(define (t10)
   (define (some-fun)
      'do-nothing)

   (define (test-map)
      (let ((done '())
	    (saved '())
	    (map_ map))
	 (map_ (lambda (n)
		  (unless (member n done)
		     (set! done (cons n done))
		     (call/cc
		      (lambda (k)
			 (set! saved (cons k saved)))))
		  (print n))
	       '(1 2 3 4))
	 (print "after map")
	 (unless (null? saved)
	    (let ((f (car saved)))
	       (set! saved (cdr saved))
	       (f 0)))))
   
   (define (test-map2)
      (let* ((kk #f)
	     (first #t)
	     (map_ map)
	     (res (map_ (lambda (n)
			   (when (= n 2)
			      (call/cc
			       (lambda (k)
				  (set! kk k))))
			   n)
			'(1 2 3 4))))
	 (print "after map2" res)
	 (when first
	    (set! first #f)
	    (kk #t))
	 (print res)))

   (define (test-map!)
      (let ((done '())
	    (saved '())
	    (map_ map!))
	 (map_ (lambda (n)
		  (unless (member n done)
		     (set! done (cons n done))
		     (call/cc
		      (lambda (k)
			 (set! saved (cons k saved)))))
		  (print n)
		  n)
	       '(1 2 3 4))
	 (print "after map!")
	 (unless (null? saved)
	    (let ((f (car saved)))
	       (set! saved (cdr saved))
	       (f 0)))))
   
   (define (test-map!2)
      (let* ((kk #f)
	     (first #t)
	     (map_ map!)
	     (res (map_ (lambda (n)
			   (when (= n 2)
			      (call/cc
			       (lambda (k)
				  (set! kk k))))
			   n)
			'(1 2 3 4))))
	 (print "after map!2" res)
	 (when first
	    (set! first #f)
	    (kk #t))
	 (print res)))

   (define (test-for-each)
      (let ((done '())
	    (saved '())
	    (for-each_ for-each))
	 (for-each_ (lambda (n)
		       (unless (member n done)
			  (set! done (cons n done))
			  (call/cc
			   (lambda (k)
			      (set! saved (cons k saved)))))
		       (print n))
	       '(1 2 3 4))
	 (print "after for-each" )
	 (unless (null? saved)
	    (let ((f (car saved)))
	       (set! saved (cdr saved))
	       (f 0)))))
   
   (define (test-filter)
      (let* ((kk #f)
	     (first #t)
	     (remove-count 3)
	     (filter_ filter)
	     (done
	      (filter_ (lambda (e)
			 (cond
			    ((> e 0) #t)
			    ((> remove-count 0)
			     (set! remove-count (- remove-count 1))
			     (if (= remove-count 2)
				 (call/cc
				  (lambda (k)
				     (set! kk k))))
			     #f)
			    (else #t)))
		      '(1 -1 2 -2 3 -3 4 -4 5 -5))))
	 (print "after filter")
	 (when first
	    (set! first #f)
	    (kk #f))
	 (print done)))
   
   (define (test-filter2)
      (let* ((kk #f)
	     (first #t)
	     (remove-count 3)
	     (done
	      (filter (lambda (e)
			 (cond
			    ((> e 0) #t)
			    ((> remove-count 0)
			     (set! remove-count (- remove-count 1))
			     (if (= remove-count 2)
				 (call/cc
				  (lambda (k)
				     (set! kk k))))
			     #f)
			    (else #t)))
		      '(1 -1 2 -2 3 -3 4 -4 5 -5))))
	 (print "after filter2")
	 (when first
	    (set! first #f)
	    (kk #f))
	 (print done)))

   (define (test-filter-map)
      (print "test-filter-map")
      (let* ((kk #f)
	     (first #t)
	     (remove-count 3)
	     (done
	      (filter-map (lambda (e)
			     (print e)
			     (cond
				((> e 0) e)
				((> remove-count 0)
				 (set! remove-count (- remove-count 1))
				 (if (= remove-count 2)
				     (call/cc
				      (lambda (k)
					 (set! kk k))))
				 #f)
				(else (* e e))))
			  '(1 -1 2 -2 3 -3 4 -4 5 -5))))
	 (print "after filter-map")
	 (when first
	    (set! first #f)
	    (kk #f))
	 (print done)))
   
   (define (test-filter!)
      (print "test-filter!")
      (let* ((kk #f)
	     (first #t)
	     (remove-count 3)
	     (filter!_ filter!)
	     (done
	      (filter!_ (lambda (e)
			  (print e)
			  (cond
			     ((and first (> e 0)) #t)
			     ((and (not first) (< e 0)) #t)
			     ((> remove-count 0)
			      (set! remove-count (- remove-count 1))
			      (if (= remove-count 2)
				  (call/cc
				   (lambda (k)
				      (set! kk k))))
			      #f)
			     (else #f)))
		       '(1 -1 2 -2 3 -3 4 -4 5 -5))))
	 (print "after filter!" done)
	 (when first
	    (set! first #f)
	    (kk #f))
	 (print done)))

   (define (test-filter2!)
      (print)
      (print "test-filter2!")
      (let* ((kk #f)
	     (first #t)
	     (remove-count 3)
	     (done
	      (filter! (lambda (e)
			  (print e)
			  (cond
			     ((and first (> e 0)) #t)
			     ((and (not first) (< e 0)) #t)
			     ((> remove-count 0)
			      (set! remove-count (- remove-count 1))
			      (if (= remove-count 2)
				  (call/cc
				   (lambda (k)
				      (set! kk k))))
			      #f)
			     (else #f)))
		       (list 1 -1 2 -2 3 -3 4 -4 5 -5))))
	 (print "after filter2!" done)
	 (when first
	    (set! first #f)
	    (kk #f))
	 (print done)))

   (define (test-call-with-values)
      (define (f1)
	 (define kk #unspecified)
	 (define first #t)
	 (call-with-values (lambda ()
			      (if (call/cc
				   (lambda (k)
				      (set! kk k)
				      #t))
				  (print "1")
				  (print "2"))
			      3)
			   (lambda L (print L)))
	 (when first
	    (set! first #f)
	    (kk #f)))

      (define (f2)
	 (define kk #unspecified)
	 (define first #t)
	 (call-with-values (lambda () (values 1 2 3))
			   (lambda L
			      (call/cc
			       (lambda (k)
				  (set! kk k)
				  #t))
			      (print L)
			      (set! L (cdr L))))
	 (when first
	    (set! first #f)
	    (kk #f)))

      (define (f3)
	 (define kk #unspecified)
	 (define kkk #unspecified)
	 (define first #t)
	 (define second #f)
	 (call-with-values (lambda ()
			      (if (call/cc
				   (lambda (k)
				      (set! kk k)
				      #t))
				  (print "1")
				  (print "2"))
			      (values 1 2 3))
			   (lambda L
			      (call/cc
			       (lambda (k)
				  (set! kkk k)
				  #t))
			      (print L)
			      (set! L (cdr L))))
	 (when first
	    (set! first #f)
	    (set! second #t)
	    (kk #f))
	 (when second
	    (set! second #f)
	    (kkk #f)))

      (print " f1") (f1)
      (print " f2") (f2)
      (print " f3") (f3))

   (define (test-dynamic-winds)
      (define (f1)
	 (define first #t)
	 (define kk #unspecified)
	 (dynamic-wind (lambda ()
			  (print "before1")
			  (call/cc
			   (lambda (k) (set! kk k)))
			  (print "before2"))
		       (lambda () (print "during"))
		       (lambda () (print "after")))
	 (when first
	    (set! first #f)
	    (kk #t)))
      
      (define (f2)
	 (define first #t)
	 (define kk #unspecified)
	 (dynamic-wind (lambda ()
			  (print "before"))
		       (lambda ()
			  (print "during1")
			  (call/cc
			   (lambda (k) (set! kk k)))
			  (print "during2"))
		       (lambda () (print "after")))
	 (when first
	    (set! first #f)
	    (kk #t)))

      (define (f3)
	 (define first #t)
	 (define second #t)
	 (define kk #unspecified)
	 (dynamic-wind (lambda ()
			  (print "before"))
		       (lambda () (print "during"))
		       (lambda ()
			  (print "after1")
			  (call/cc
			   (lambda (k) (set! kk k)))
			  (print "after2")))
	 (when first
	    (set! first #f)
	    (kk #t))
	 (when second
	    (set! second #f)
	    (kk #t)))

      (define (f4)
	 (define first #t)
	 (define kk #unspecified)
	 (with-handler
	  (lambda (err)
	     (print "caught error"))
	  (dynamic-wind (lambda ()
			   (print "before"))
			(lambda ()
			   (print "during")
			   (car (car first)))
			(lambda ()
			   (print "after1")
			   (call/cc
			    (lambda (k) (set! kk k)))
			   (print "after2"))))
	 (when first
	    (set! first #f)
	    (kk #t))
	 (print "finally"))
      
      (print " f1") (f1)
      (print " f2") (f2)
      (print " f3") (f3)
      (print " f4") (f4))
   
   (define (test-with-input-from-string)
      (define first #t)
      (define counter 0)
      (define kk #unspecified)
      (with-input-from-string
	    "1 2 3 4 5 6"
	 (lambda ()
	    (read)
	    (call/cc (lambda (k) (set! kk k)))
	    (print (current-input-port))
	    (let ((r (read)))
	       (if (eof-object? r)
		   (begin
		      (set! kk #f)
		      (print "eof"))
		   (print r)))))
      (if (< counter 5)
	  (begin
	     (set! counter (+ counter 1))
	     (if kk (kk #t)))))

   (define (test-with-output-to-string)
      (define first #t)
      (define counter 0)
      (define kk #unspecified)
      (let ((t (with-output-to-string
		  (lambda ()
		     (call/cc (lambda (k) (set! kk k)))
		     (print counter)))))
	 (print "t: " t))
      (if (< counter 5)
	  (begin
	     (set! counter (+ counter 1))
	     (kk #t))))

   (define (test-hashtable-for-each)
      (let ((ht (make-hashtable))
	    (kk #f))
	 (hashtable-put! ht 1 #t)
	 (hashtable-put! ht 2 #t)
	 (hashtable-put! ht 3 #t)
	 (hashtable-put! ht 4 #t)
	 (hashtable-for-each ht
			     (lambda (key val)
				(print key " " val)
				(let ((t #unspecified))
				   (if (call/cc (lambda (k) (set! t k) #t))
				       (if (not kk)
					   (set! kk t))
				       (set! kk #f)))))
					   
			      
	 (if kk (kk #f))))
   
   (define (test-nested-call/ccs)
      (define count 0)
      (define kk #f)
      (define kkk #f)
      
      (call/cc
       (lambda (k)
	  (set! kk k)
	  (print 1 " " count)
	  (call/cc
	   (lambda (k2)
	      (set! kkk k2)
	      (print 2 " " count)))
	  (print 3 " " count)))
      (print 4 " " count)
      (set! count (+ count 1))
      (if (= count 1)
	  (kk #t))
      (if (= count 2)
	  (kkk #t))
      (if (= count 3)
	  (kk #t))
      (if (= count 4)
	  (kkk #t)))

   (define (test-call/cc-bind-exit)
      (define count 0)
      (define kk #f)

      (print (bind-exit (escape)
		(print 0 " " count)
		(call/cc
		 (lambda (k)
		    (set! kk k)
		    (print 1 " " count)))
		(print 2 " " count)
		(if (= count 2)
		    (escape (+ 100 count)))
		(print 3 " " count)
		4))

      (set! count (+ count 1))
      (if (< count 5)
	  (kk #t)))

   (define (test-any)
      (print)
      (print "test-any")
      (let* ((kk #f)
	     (first #t)
	     (count 3)
	     (done
	      (any (lambda (e)
		      (print e)
		      (cond
			 ((> count 0)
			  (set! count (- count 1))
			  (if (= count 2)
			      (call/cc
			       (lambda (k)
				  (set! kk k))))
			  #f)
			 (else e)))
		   (list #f #f #f #f 1 2 3 4))))
	 (print "after any" done)
	 (when first
	    (set! first #f)
	    (kk #f))
	 (print done)))

   (define (test-every)
      (print)
      (print "test-every")
      (let* ((kk #f)
	     (first #t)
	     (count 3)
	     (done
	      (any (lambda (e)
		      (print e)
		      (cond
			 ((> count 0)
			  (set! count (- count 1))
			  (if (= count 2)
			      (call/cc
			       (lambda (k)
				  (set! kk k))))
			  #t)
			 (else e)))
		   (list 1 2 3 4 5 6))))
	 (print "after every" done)
	 (when first
	    (set! first #f)
	    (kk #f))
	 (print done)))

   (print "map") (test-map)
   (print "map2") (test-map2)
   (print "map!") (test-map!)
   (print "map!2") (test-map!2)
   (print "for-each") (test-for-each)
   (print "filter") (test-filter)
   (print "filter2") (test-filter2)
   (print "filter-map") (test-filter-map)
   (print "test-filter!") (test-filter!)
   (print "test-filter2!") (test-filter2!)
   (print "call-with-values") (test-call-with-values)
   (print "dynamic-winds") (test-dynamic-winds)
   ;; TODO: Bigloo does not work the same way...
   ;(print "with-input-from-string") (test-with-input-from-string)
   ;(print "with-output-to-string") (test-with-output-to-string)
   
   (print "hashtable-for-each") (test-hashtable-for-each)
   (print "nested call/ccs") (test-nested-call/ccs)
   (print "call/cc within bind-exit") (test-call/cc-bind-exit)
   (print "test-any") (test-any)
   (print "test-every") (test-every)
   (set! some-fun #f))
(print "t10")
(t10)

(define (t11)
   (define (f)
      (let ((path '())
	    (c #f))
	 (let ((add (lambda (s)
		       (set! path (cons s path)))))
	    (dynamic-wind
	       (lambda () (add 'connect))
	       (lambda ()
		  (add (call/cc
			(lambda (c0)
			   (set! c c0)
			   'talk1))))
	       (lambda () (add 'disconnect)))
	    (if (< (length path) 4)
		(c 'talk2)
		(reverse path)))))
   (print (f)))
(print "t11")
(t11)

(define (t12)
   (define (f1)
      (define kk #f)
      (define first #t)
      (print (call/cc (lambda (k) (set! kk k) "first-round")))
      (when first
	 (set! first #f)
	 (kk "second round")))
   (define (f2)
      (define kk #f)
      (define first #t)
      (print (dynamic-wind
		(lambda () 'do-nothing)
		(lambda () (call/cc (lambda (k) (set! kk k) "first-round")))
		(lambda () 'do-nothing)))
      (when first
	 (set! first #f)
	 (kk "second round")))
   (define (f3)
      (define kk #f)
      (define first #t)
      (dynamic-wind
	 (lambda () 'do-nothing)
	 (lambda () (print (call/cc (lambda (k) (set! kk k) "first-round"))))
	 (lambda () 'do-nothing))
      (when first
	 (set! first #f)
	 (kk "second round")))
   (f1)
   (f2)
   (f3))
(print "t12")
(t12)

(define (t13)
   (define (f1)
      (define kk #f)
      (define first #t)
      
      (print (if #t
		 (call/cc (lambda (k) (set! kk k) 1))
		 (call/cc (lambda (k) (set! kk k) 1))))
      (print "2")
      (when first
	 (print 3)
	 (set! first #f)
	 (kk 4)))

   (define (f2)
      (define kk #f)
      (define first #t)
      
      (print (let loop ()
		(if #f
		    (loop)
		    (call/cc (lambda (k) (set! kk k) 1)))))

      (print "2")
      (when first
	 (print 3)
	 (set! first #f)
	 (kk 4)))

   (print "f1") (f1)
   (print "f2") (f2)
   )
(print "t13")
(t13)

(define (t14)
   (define kk #f)
   (define first #t)
   
   (let loop ((i 0))
      (when (< i 3)
	 (call/cc (lambda (k)
		     (if (not kk)
			 (set! kk k))
		     #f))
	 (print i)
	 (loop (+ i 1))))

   (when first
      (set! first #f)
      (kk #t)))
(print "t14")
(t14)

(define (t15)
   (define kk #f)
   (define first #t)
   
   (let loop ((j 0)
	      (i 0)
	      (k 0))
      (when (< i 3)
	 (print i " " j " " k)
	 (loop (+ j 1)
	       (+ (call/cc (lambda (k)
			      (if (not kk)
				  (set! kk k))
			      1))
		  i)
	       (+ k 1))))

   (when first
      (set! first #f)
      (kk 1)))
(print "t15")
(t15)
