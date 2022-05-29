;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-2009 Florian Loitsch, see LICENSE file       */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2Js.                                  */
;*                                                                     */
;*   Scheme2Js is distributed in the hope that it will be useful,      */
;*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
;*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
;*   LICENSE file for more details.                                    */
;*=====================================================================*/

(module template-display
   (import tools)
   (export (macro template-display)
	   (macro p-display)))

(define-macro (p-display p . Largs)
   `(begin
       ,@(map (lambda (arg)
		 `(display ,arg ,p))
	      Largs)))

(define-macro (template-display p . Largs)
   (define (my-error msg val)
      (let ((loc (if (epair? Largs) (cer Largs) #f)))
	 (my-error2 msg val loc)))
   (define (my-error2 msg val loc)
      (match-case loc
	 ((at ?fname ?loc)
	  (error/location "template-display" msg val fname loc))
	 (else
	  (error "template-display" msg val))))

   (define (ltrim str)
      (let loop ((i 0))
	 (cond
	    ((>= i (string-length str))
	     "")
	    ((char=? (string-ref str i) #\space)
	     (loop (+ i 1)))
	    (else
	     (substring str i (string-length str))))))

   (define (split-args patterns/args)
      (let loop ((patterns/args patterns/args)
		 (rev-patterns '())
		 (rev-args '()))
	 (cond
	    ((null? patterns/args)
	     (values (reverse! rev-patterns)
		     (reverse! rev-args)))
	    ((string? (car patterns/args))
	     (loop (cdr patterns/args)
		   (cons `(pattern ,(car patterns/args)) rev-patterns)
		   rev-args))
	    ((and (pair? (car patterns/args))
		  (memq (caar patterns/args) '(?@ ??)))
	     (loop (cdr patterns/args)
		   (cons (car patterns/args) rev-patterns)
		   rev-args))
	    (else
	     (loop (cdr patterns/args)
		   rev-patterns
		   (cons (car patterns/args) rev-args))))))

   (define (aux patterns args hole)
      (cond
	 ((and (null? patterns)
	       (null? args)
	       (not hole))
	  '())
	 ((and hole
	       (null? patterns))
	  (my-error "?@ without obligatory hole" Largs))
	 ((null? patterns)
	  (my-error "too many arguments" Largs))
	 (else
	  (match-case (car patterns)
	     (((kwote ?@) ?test . ?cond-args)
	      ;; conditional with obligatory hole
	      (let ((body-sym (gensym 'hole))
		    (body (aux (cdr patterns) args hole)))
		 (receive (sub-patterns sub-args)
		    (split-args cond-args)
		    `((let ((,body-sym (lambda () ,@body)))
			 (if ,test
			     (begin ,@(aux sub-patterns sub-args body-sym))
			     (,body-sym)))))))
	     (((kwote ??) ?test . ?cond-args)
	      ;; conditional output.
	      (receive (sub-patterns sub-args)
		 (split-args cond-args)
		 `((when ,test ,@(aux sub-patterns sub-args #f))
		   ,@(aux (cdr patterns) args hole))))
	     (((and (or pattern rem-pattern) ?pattern-type)
	       ?pattern)
	      (cond
		 ((string-contains pattern "~@")
		  =>
		  (lambda (hole-pos)
		     (aux (cons* `(,pattern-type ,(substring pattern 0 hole-pos))
				 '(__HOLE_P_DISPLAY__)
				 `(rem-pattern
				   ,(substring pattern (+fx hole-pos 2)
					       (string-length pattern)))
				 (cdr patterns))
			  args
			  hole)))
		 ((string-contains pattern "~e")
		  =>
		  (lambda (exec-pos)
		     (aux (cons* `(,pattern-type ,(substring pattern 0 exec-pos))
				 '(__EXEC_P_DISPLAY__)
				 `(rem-pattern
				   ,(substring pattern (+fx exec-pos 2)
					       (string-length pattern)))
				 (cdr patterns))
			  args
			  hole)))
		 ((string-contains pattern "~a")
		  =>
		  (lambda (value-pos)
		     (aux (cons* `(,pattern-type ,(substring pattern 0 value-pos))
				 '(__VALUE_P_DISPLAY__)
				 `(rem-pattern
				   ,(substring pattern (+fx value-pos 2)
					       (string-length pattern)))
				 (cdr patterns))
			  args
			  hole)))
		 ((string-contains pattern "~x")
		  =>
		  (lambda (hex-pos)
		     (aux (cons* `(,pattern-type ,(substring pattern 0 hex-pos))
				 '(__HEX_P_DISPLAY__)
				 `(rem-pattern
				   ,(substring pattern (+fx hex-pos 2)
					       (string-length pattern)))
				 (cdr patterns))
			  args
			  hole)))
		 ((string-contains pattern "~?")
		  =>
		  (lambda (maybe-pos)
		     (aux (cons* `(,pattern-type ,(substring pattern 0 maybe-pos))
				 '(__MAYBE_P_DISPLAY__)
				 `(rem-pattern
				   ,(substring pattern (+fx maybe-pos 2)
					       (string-length pattern)))
				 (cdr patterns))
			  args
			  hole)))
		 ((string-prefix? "$" pattern)
		  (let* ((p (open-input-string
			     (substring pattern 1 (string-length pattern))))
			 (value (read p))
			 (end-pos (+fx (input-port-position p) 1)))
		     (close-input-port p)
		     (aux (cons* '(__VALUE_P_DISPLAY__)
				 `(rem-pattern
				   ,(substring pattern end-pos
					       (string-length pattern)))
				 (cdr patterns))
			  (cons value args)
			  hole)))
		 ((string-contains pattern "$")
		  =>
		  (lambda (dollar-pos)
		     (aux (cons* `(,pattern-type ,(substring pattern 0 dollar-pos))
				 `(rem-pattern
				   ,(substring pattern dollar-pos
					       (string-length pattern)))
				 (cdr patterns))
			  args
			  hole)))
		 ((eq? pattern-type 'pattern)
		  (aux (cons `(rem-pattern ,(ltrim pattern))
			     (cdr patterns))
		       args
		       hole))
		 ((string-null? pattern)
		  (aux (cdr patterns) args hole))
		 (else ;; pattern-type == 'rem-pattern
		  `((display ,pattern ,p)
		    ,@(aux (cdr patterns) args hole)))))
	     ((__HOLE_P_DISPLAY__)
	      (when (not hole)
		 (my-error "~@ without surrounding ?@ (or more than one ~@)" Largs))
	      (if (and (not (null? args))
		       (pair? (car args)) ;; a symbol
		       (eq? (caar args) 'quote))
		  `((,hole)
		    ,@(aux (cdr patterns) (cdr args) #f))
		  `((,hole)
		    ,@(aux (cdr patterns) args #f))))
	     ((__EXEC_P_DISPLAY__)
	      (when (null? args)
		 (my-error "not enough arguments (~e)" Largs))
	      (match-case (car args)
		 ((separated ?separator
			     (lambda ?formals
				. ?disp-body)
			     . ?lists)
		  (let ((tmp (gensym 'tmp)))
		     `((let ((,tmp #t))
			  (for-each (lambda ,formals
				       (if ,tmp
					   (set! ,tmp #f)
					   (display ,separator ,p))
				       (template-display ,p ,@disp-body))
				    ,@lists))
		       ,@(aux (cdr patterns) (cdr args) hole))))
		 ((separated-e ?separator ?fun . ?lists)
		  (let ((tmp (gensym 'tmp))
			(formals (map (lambda (i)
					 (string->symbol (format "arg~a" i))
					 (iota (length lists))))))
		     `((let ((,tmp #t))
			  (for-each (lambda ,formals
				       (if ,tmp
					   (set! ,tmp #f)
					   (display ,separator ,p))
				       (template-display ,p "~e" (,fun ,@formals)))
				    ,@lists))
		       ,@(aux (cdr patterns) (cdr args) hole))))
		 ((each (lambda ?formals
			   . ?disp-body)
			. ?lists)
		  `((for-each (lambda ,formals
				 (template-display ,p ,@disp-body))
			      ,@lists)
		    ,@(aux (cdr patterns) (cdr args) hole)))
		 ((each-a ?fun . ?lists)
		  (let ((formals (map (lambda (i)
					 (string->symbol (format "arg~a" i))
					 (iota (length lists))))))
		     `((for-each (lambda ,formals
				    (template-display ,p
				       "~a" (,fun ,@formals)))
				 ,@lists)
		       ,@(aux (cdr patterns) (cdr args) hole))))
		 (else
		  `(,(car args)
		    ,@(aux (cdr patterns) (cdr args) hole)))))
	     ((__VALUE_P_DISPLAY__)
	      (when (null? args)
		 (my-error "not enough arguments (~a or $)" Largs))
	      `((display ,(car args) ,p)
		,@(aux (cdr patterns) (cdr args) hole)))
	     ((__HEX_P_DISPLAY__)
	      (when (null? args)
		 (my-error "not enough arguments (~x)" Largs))
	      `((fprintf ,p "~x" ,(car args))
		,@(aux (cdr patterns) (cdr args) hole)))
	     ((__MAYBE_P_DISPLAY__)
	      (when (null? args)
		 (my-error "not enough arguments (~?)" Largs))
	      (let ((tmp (gensym 'tmp)))
		 `((let ((,tmp ,(car args)))
		      (when ,tmp
			 (display ,tmp ,p))
		      ,@(aux (cdr patterns) (cdr args) hole)))))
	     (else
	      (my-error2 "bad pattern" (car patterns)
			 (if (epair? (car patterns))
			     (car patterns)
			     patterns)))))))
   (receive (patterns args)
      (split-args Largs)
      `(begin ,@(aux patterns args #f))))
