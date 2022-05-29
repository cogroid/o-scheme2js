;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-09 Florian Loitsch, see LICENSE file         */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2Js.                                  */
;*                                                                     */
;*   Scheme2Js is distributed in the hope that it will be useful,      */
;*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
;*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
;*   LICENSE file for more details.                                    */
;*=====================================================================*/

(module pobject-conv
   (import nodes
	   error
	   export-desc
	   config
	   verbose)
   (export
    ;; going to be used in symbol-pass.
    (wide-class Runtime-Ref::Ref)
    (wide-class Define::Set!))
   (export (pobject-conv::Node prog)
	   (runtime-ref id::symbol)))

;; recognized as "artificial" runtime-reference.
(define (runtime-ref id)
   ;; we use a function to differenciate this construct
   ;; from similar user-constructs.
   (list 'runtime-ref id (lambda () 'runtime-ref)))

(define (location s-expr)
   (or (and (epair? s-expr)
	    (cer s-expr))
       ;; not "really" correct, but an approximate location is better than
       ;; nothing.
       (and (pair? s-expr)
	    (location (car s-expr)))))

(define (scheme->pobject-map l)
   (let loop ((l l)
	      (rev-res '()))
      (cond
	 ((null? l)
	  (reverse! rev-res))
	 ((not (pair? l))
	  (scheme2js-error "Object-conv"
			   "invalid expression-list"
			   l
			   l))
	 (else
	  (let ((loc (location l)))
	     (loop (cdr l)
		   (cons (scheme->pobject (car l) loc)
			 rev-res)))))))

(define (location-map f l)
   (let loop ((l l)
	      (rev-res '()))
      (if (null? l)
	  (reverse! rev-res)
	  (let ((loc (location l)))
	     (loop (cdr l)
		   (cons (f (car l) loc)
			 rev-res))))))

(define (attach-location o loc)
   (when loc (Node-location-set! o loc))
   o)
   
(define (pobject-conv prog)
   (verbose "list->pobject")
   (instantiate::Module
      (body (scheme->pobject prog (location prog)))))

(define (expr-list->Begin expr-list)
   (instantiate::Begin
      (exprs (scheme->pobject-map expr-list))))

(define (lambda->pobject arguments body)
   ;; if there's a vaarg, make it a the last element of the list and return
   ;; (the list . #t)
   (define (vaarg-list! arguments)
      (cond
	 ((null? arguments)
	  (values arguments #f))
	 ((not (pair? arguments))
	  (values (list arguments) #t))
	 (else
	  (let* ((p (last-pair arguments))
		 (vaarg (cdr p)))
	     (cond
		((null? vaarg)
		 (values arguments #f))
		(else
		 (set-cdr! p (list vaarg)) ;; physically attach the vaarg
		 (values arguments #t)))))))
	 
      
   (receive (formals vaarg?)
      (vaarg-list! arguments)

      (unless (and (list? formals)
		   (every? symbol? formals))
	 (scheme2js-error "Object-conv"
			  "Invalid arguments-clause"
			  arguments
			  arguments))

      (let ((formal-decls
	     (location-map (lambda (formal loc)
			      (attach-location (instantiate::Ref
						  (id formal))
					       loc))
			   formals)))
	 (instantiate::Lambda
	    (formals formal-decls)
	    (vaarg? vaarg?)
	    (body (instantiate::Return
		     (val (expr-list->Begin body))))))))

(define (let-form->pobject bindings body kind)
   (define (binding->pobject binding)
      (when (or (null? binding)
		(null? (cdr binding))
		(not (symbol? (car binding)))
		(not (null? (cddr binding))))
	 (scheme2js-error "pobject-conversion"
			  "Bad Let-form binding"
			  binding
			  binding))
      (let ((var (car binding))
	    (val (cadr binding)))
	 (instantiate::Set!
	    (lvalue (attach-location (instantiate::Ref (id var))
				     (location binding)))
	    (val (scheme->pobject val (location (cdr binding)))))))
   
   (let ((pobject-bindings (map! binding->pobject bindings)))
      (instantiate::Let
	 (bindings pobject-bindings)
	 (body (expr-list->Begin body))
	 (kind kind))))

(define (case->pobject key clauses)
   (define (clause->pobject clause last?)
      (match-case clause
	 ((?consts . ?raw-exprs)
	  (let* ((exprs (scheme->pobject-map raw-exprs))
		 (begin-expr (instantiate::Begin (exprs exprs))))
	     (if (and last?
		      (eq? consts 'else))
		 (instantiate::Clause
		    (consts '())
		    (expr begin-expr)
		    (default-clause? #t))
		 (begin
		    (unless (list? consts)
		       (scheme2js-error "Object-conv"
					"bad constants in case-clause"
					consts
					consts))
		    (instantiate::Clause
		       (consts (map (lambda (const)
				       (instantiate::Const (value const)))
				    consts))
		       (expr begin-expr)
		       (default-clause? #f))))))
	 (else
	  (scheme2js-error "Object-conv"
			   "bad Case-clause"
			   clause
			   clause))))
   
   (define (clauses->pobjects clauses rev-result)
      (cond
	 ((null? clauses) ;; should never happen
	  (reverse! rev-result))
	 ((not (pair? clauses)) ;; dotted form (x . y)
	  (scheme2js-error "Object-conv"
			   "bad case-form"
			   clauses
			   clauses))
	 ((null? (cdr clauses))
	  (let ((rev-all-clauses (cons (clause->pobject (car clauses) #t)
				       rev-result)))
	     ;; if there was no default clause, we add one.
	     (if (Clause-default-clause? (car rev-all-clauses))
		 (reverse! rev-all-clauses)
		 (reverse! (cons (clause->pobject '(else #unspecified) #t)
				 rev-all-clauses)))))
	 (else
	  (clauses->pobjects (cdr clauses)
			     (cons (clause->pobject (car clauses) #f)
				   rev-result)))))

   (instantiate::Case
      (key (scheme->pobject-no-loc key))
      (clauses (clauses->pobjects clauses '()))))

(define (scheme->pobject-no-loc exp)
   (cond
      ((pair? exp)
       (match-case exp
	  ((quote ?datum) (instantiate::Const (value datum)))
	  ((lambda ?formals . ?body) (lambda->pobject formals body))
	  ((if ?test ?then)
	   ;(scheme->pobject `(if ,test ,then #f))
	   (set-cdr! (cddr exp) '(#f))
	   (scheme->pobject-no-loc exp))
	  ((if ?test ?then ?else)
	   (instantiate::If
	      (test (scheme->pobject test (location (cdr exp))))
	      (then (scheme->pobject then (location (cddr exp))))
	      (else (scheme->pobject else (location (cdddr exp))))))
	  ((if . ?L) (scheme2js-error #f "bad if-form" exp exp))
	  ((case ?key . ?clauses)
	   (case->pobject key clauses))
	  ((set! (and ?var (? symbol?)) ?expr)
	   (instantiate::Set!
	      (lvalue (attach-location (instantiate::Ref
					  (id var))
				       (location (cdr exp))))
	      (val (scheme->pobject expr (location (cddr exp))))))
	  ((set! (@ ?sym ?qualifier) ?expr)
	   (let ((id (cadr exp)))
	      (instantiate::Set!
		 (lvalue (attach-location (instantiate::Ref
					     (id id))
					  (location (cdr exp))))
		 (val (scheme->pobject expr (location (cddr exp)))))))
	  ((set! . ?L) (scheme2js-error #f "bad set!-form" exp exp))
	  ((let ?bindings . ?body) (let-form->pobject bindings body 'let))
	  ((letrec ?bindings . ?body) (let-form->pobject bindings body 'letrec))
	  ((begin . ?body) (instantiate::Begin (exprs (scheme->pobject-map body))))
	  ((define ?var ?expr)
	   (instantiate::Define
	      (lvalue (attach-location (instantiate::Ref
					(id var))
				     (location (cdr exp))))
	      (val (scheme->pobject expr (location (cddr exp))))))
	  ((pragma ?str)
	   (if (string? str)
	       (instantiate::Pragma (str str))
	       (scheme2js-error #f "bad pragma-form" exp exp)))
	  ((runtime-ref ?id (? procedure?))
	   (instantiate::Runtime-Ref
	      (id id)))
	  ((@ ?sym ?qualifier)
	   (instantiate::Ref (id (cdr exp))))
	  ((?operator . ?operands)
	   (if (and (config 'return)
		    (eq? operator 'return!))
	       (if (or (null? operands)
		       (not (null? (cdr operands))))
		   (scheme2js-error #f "bad return! form: " exp exp)
		   (instantiate::Return
		      (val (scheme->pobject (car operands)
					    (location operands)))))
	       (instantiate::Call
		  (operator (scheme->pobject operator (location exp)))
		  (operands (scheme->pobject-map operands)))))))
      ((eq? exp #unspecified)
	(instantiate::Const (value #unspecified)))
       ;; unquoted symbols must be var-refs
      ((symbol? exp)
       (instantiate::Ref
	  (id exp)))
      ((vector? exp)
       (scheme2js-error #f "vectors must be quoted" exp exp))
      (else
       (instantiate::Const (value exp)))))

(define (scheme->pobject exp loc)
   (attach-location (scheme->pobject-no-loc exp) loc))
