;*=====================================================================*/
;*    Author      :  Florian Loitsch                                   */
;*    Copyright   :  2007-10 Florian Loitsch, see LICENSE file         */
;*    -------------------------------------------------------------    */
;*    This file is part of Scheme2Js.                                  */
;*                                                                     */
;*   Scheme2Js is distributed in the hope that it will be useful,      */
;*   but WITHOUT ANY WARRANTY; without even the implied warranty of    */
;*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     */
;*   LICENSE file for more details.                                    */
;*=====================================================================*/

(module out
   (cond-expand
      (enable-threads (library pthread)))
   (import config
	   tools
	   nodes
	   export-desc
	   walk
	   gen-js
	   mutable-strings
	   verbose
	   allocate-names
	   callcc
	   compile-optimized-call
	   compile-optimized-boolify
	   compile-optimized-set
	   template-display
	   pipe-port
	   js-pp
	   push-declarations
	   error)
   (static
    (class Pragmas
       lock
       pragmas::pair ;; will start with "dummy"-pair
       last-pragma::pair) ;; last in list
    (class Out-Env
       trampoline?::bool
       max-tail-depth::bint
       suspend-resume?::bool
       call/cc?::bool
       optimize-calls?::bool
       debug?::bool

       foreign-out

       pp?::bool
       pragmas       ;; ::Pragmas or #f

       last-line
       last-file)
    (wide-class Out-Lambda::Lambda
       lvalue))
   (export (compile-value const p::output-port foreign-out loc)
	   (out tree::Module p)))

(define (out tree p)
   (verbose "Compiling")
   (gen-var-names tree)
   (push-var-declarations tree)
   (cond-expand
      ((not enable-threads)
       (config-set! 'pp #f)
       (when (>=fx (bigloo-debug) 1)
	  (warning "pretty-printing/compression only works when pthreads are enabled"))))
   (if (config 'pp)
       (pp-gen-code tree p)
       (gen-code tree p #f)))

(define (pp-gen-code tree p)
   (let* ((dummy-pair (list 'dummy))
	  (pragmas (instantiate::Pragmas
		     (lock (make-mutex))
		     (pragmas dummy-pair)
		     (last-pragma dummy-pair))))
      (receive (out-p in-p out-p-closer)
	 (open-pipe-port)
	 (let* ((compress? (config 'compress))
		(indent-width (let ((t (config 'indent)))
				 (if (and (fixnum? t)
					  (positive? t))
				     t
				     0)))
		(t (make-thread (lambda ()
				   (js-pp in-p
					  p
					  (lambda ()
					     (consume-next-pragma! pragmas))
					  compress?
					  indent-width)))))
	    (thread-start-joinable! t)
	    (gen-code tree out-p pragmas)
	    (out-p-closer)
	    (thread-join! t)))))
   
(define (gen-code tree p pragmas)
   (verbose "  generating code")
   (let ((env (instantiate::Out-Env
		 (trampoline? (and (config 'trampoline) #t))
		 (max-tail-depth (config 'max-tail-depth))
		 (suspend-resume? (and (config 'suspend-resume) #t))
		 (call/cc? (and (config 'call/cc) #t))
		 (optimize-calls? (and (config 'optimize-calls) #t))
		 (debug? (and (config 'debug) #t))

		 (foreign-out (config 'foreign-out))

		 (pp? (and (config 'pp) #t))
		 (pragmas pragmas)

		 (last-line 0)
		 (last-file ""))))
      (compile tree env p #t)))

(define *tmp-var* "sc_tmp") ;; can't conflict with generated names.

;; when using immutable values.
(define *symbol-prefix* "\\uEBAC")
(define *keyword-prefix* "\\uEBAD")

;; TODO propose other global access as option.
; (define (scm2js-globals-init p)
;    'do-nothing)
; (define (scm2js-global global)
;    (string-append "SC_" global))

(define (scm2js-globals-init p)
   (template-display p
      "var sc_globals = SC_SCM2JS_GLOBALS;\n"))
(define (scm2js-global global)
   (string-append "sc_globals." global))

(define (call/cc-frame-name scope)
   (with-access::Call/cc-Scope-Info scope (id)
      (string-append "'frame-" (symbol->string id) "'")))

(define (call/cc-counter-name nb)
   (string-append "sc_counter_" (number->string nb)))

(define-nmethod (Node.compile ignored ignored2)
   (error #f "Internal Error: forgot node-type" this))

;; return true, if we should use nested new sc_Pairs instead of sc_list(...).
(define (small-list/pair? l)
   (define (smaller? l n)
      (cond
	 ((< n 0) #f)
	 ((null? l) #t)
	 ((not (pair? l)) #t)
	 (else
	  (smaller? (cdr l) (- n 1)))))

   (smaller? l 5))

;*---------------------------------------------------------------------*/
;*    my-string-for-read ...                                           */
;*    -------------------------------------------------------------    */
;*    MS: new implementation 13apr2010.                                */
;*---------------------------------------------------------------------*/
(define (my-string-for-read str)
   
   (define (count str ol)
      (let loop ((i 0)
		 (n 0))
	 (if (=fx i ol)
	     n
	     (let ((c (string-ref str i)))
		(case c
		   ((#\" #\\ #\Newline #\Return)
		    (loop (+fx i 1) (+fx n 2)))
		   ((#\/ #\null)
		    (loop (+fx i 1) (+fx n 6)))
		   (else
		    (loop (+fx i 1) (+fx n 1))))))))
   
   (define (encode str ol nl)
      (if (=fx nl ol)
	  str
	  (let ((res (make-string nl)))
	     (let loop ((i 0)
			(j 0))
		(if (=fx j nl)
		    res
		    (let ((c (string-ref str i)))
		       (case c
			  ((#\" #\\)
			   (string-set! res j #\\)
			   (string-set! res (+fx j 1) c)
			   (loop (+fx i 1) (+fx j 2)))
			  ((#\Newline)
			   (string-set! res j #\\)
			   (string-set! res (+fx j 1) #\n)
			   (loop (+fx i 1) (+fx j 2)))
			  ((#\Return)
			   (string-set! res j #\\)
			   (string-set! res (+fx j 1) #\r)
			   (loop (+fx i 1) (+fx j 2)))
			  ((#\/)
			   (string-set! res j #\\)
			   (string-set! res (+fx j 1) #\u)
			   (string-set! res (+fx j 2) #\0)
			   (string-set! res (+fx j 3) #\0)
			   (string-set! res (+fx j 4) #\2)
			   (string-set! res (+fx j 5) #\f)
			   (loop (+fx i 1) (+fx j 6)))
			  ((#\null)
			   (string-set! res j #\\)
			   (string-set! res (+fx j 1) #\u)
			   (string-set! res (+fx j 2) #\0)
			   (string-set! res (+fx j 3) #\0)
			   (string-set! res (+fx j 4) #\0)
			   (string-set! res (+fx j 5) #\0)
			   (loop (+fx i 1) (+fx j 6)))
			  (else
			   (string-set! res j c)
			   (loop (+fx i 1) (+fx j 1))))))))))
   
   (let ((ol (string-length str)))
      (encode str ol (count str ol))))

;*---------------------------------------------------------------------*/
;*    display-string-for-read ...                                      */
;*---------------------------------------------------------------------*/
(define (display-string-for-read str op)
   (let ((ol (string-length str)))
      (let loop ((i 0))
	 (when (<fx i ol)
	    (let ((c (string-ref str i)))
	       (case c
		  ((#\" #\\)
		   (write-char #\\ op)
		   (write-char c op)
		   (loop (+fx i 1)))
		  ((#\Newline)
		   (write-char #\\ op)
		   (write-char #\n op)
		   (loop (+fx i 1)))
		  ((#\Return)
		   (write-char #\\ op)
		   (write-char #\r op)
		   (loop (+fx i 1)))
		  ((#\/)
		   (write-char #\\ op)
		   (write-char #\u op)
		   (write-char #\0 op)
		   (write-char #\0 op)
		   (write-char #\2 op)
		   (write-char #\f op)
		   (loop (+fx i 1)))
		  ((#\null)
		   (write-char #\\ op)
		   (write-char #\u op)
		   (write-char #\0 op)
		   (write-char #\0 op)
		   (write-char #\0 op)
		   (write-char #\0 op)
		   (loop (+fx i 1)))
		  (else
		   (write-char c op)
		   (loop (+fx i 1)))))))))


(define (compile-value val p foreign-out loc)
   (define (display-ucs2-char p c) ;; without the quotes
      (let ((i (ucs2->integer c)))
	 (cond
	    ((and (<=fx i #x7F)
		  (or (ucs2-alphabetic? c)
		      (ucs2-numeric? c)))
	     (template-display p "~a" (ucs2->char c)))
	    ((<=fx i #xF)
	     (template-display p "\\u000~x" i))
	    ((<=fx i #xFF)
	     (template-display p "\\u00~x" i))
	    ((<=fx i #xFFF)
	     (template-display p "\\u0~x" i))
	    (else
	     (template-display p "\\u~x" i)))))
	     
   (cond
      ((null? val)
       (template-display p "null"))
      ((boolean? val)
       (template-display p "~a" (if val "true" "false")))
      ((symbol? val)
       (template-display p
	  "\"~?~a\""
	  (and (not (use-mutable-strings?)) *symbol-prefix*)
	  (my-string-for-read (symbol->string! val))))
      ((char? val)
       (template-display p
	  "(new sc_Char(\"~a\"))" (my-string-for-read (string val))))
      ((ucs2? val)
       (template-display p
	  "(new sc_Char(\"~e\"))" (display-ucs2-char p val)))
      ((number? val)
       ;; CARE: initially I had "($val)" here. and I suppose there was a
       ;; reason I put the parenthesis around. this will probably come back and
       ;; bite me...
       (template-display p
	  (?@ (< val 0) "(~@)")
	  "$val"))
      ((string? val)
       (template-display p
	  (?@ (use-mutable-strings?) "(new sc_String(~@))")
	  "\"~a\"" (my-string-for-read val)))
      ((ucs2-string? val)
       (template-display p
	  (?@ (use-mutable-strings?) "(new sc_String(~@))")
	  "\"~e\""
	  (let loop ((i 0))
	     (unless (>= i (ucs2-string-length val))
		(display-ucs2-char p (ucs2-string-ref val i))
		(loop (+fx i 1))))))
      ((vector? val)
       (template-display p
	  "[~e]"
	  (let loop ((i 0))
	     (unless (>= i (vector-length val))
		(if (not (= i 0))
		    (template-display p ", "))
		(compile-value (vector-ref val i) p foreign-out loc)
		(loop (+ i 1))))))
      ((pair? val)
       (if (small-list/pair? val)
	   (template-display p
	      "(new sc_Pair(~e,~e))"
	      (compile-value (car val) p foreign-out loc)
	      (compile-value (cdr val) p foreign-out loc))
	   (template-display p
	      "sc_list(~e)"
	      (separated ", " 
			 (lambda (e) "~e" (compile-value e p foreign-out loc))
			 val))))
      ((eq? val #unspecified)
       (template-display p "undefined"))
      ((keyword? val)
       (if (use-mutable-strings?)
	   (template-display p
	      "(new sc_Keyword(\"~a\"))"
	      (my-string-for-read (keyword->string! val)))
	   (template-display p
	      "\"~a~a\"" *keyword-prefix*
	      (my-string-for-read (keyword->string! val)))))
      (foreign-out
       (foreign-out val p))
      (else
       (scheme2js-error "val-out"
			"Internal Error: forgot Val-type"
			val
			loc))))
   
(define-nmethod (Const.compile p stmt?)
   (with-access::Const this (value)
      (template-display p
	 (?@ stmt? "~@;\n")
	 "~e" (compile-value value p (Out-Env-foreign-out env) this))))

(define-nmethod (Ref.compile p stmt?)
   (with-access::Ref this (var)
      (with-access::Named-Var var (js-id)
	 (template-display p
	    (?@ stmt? "~@;\n")
	    "$js-id"))))

(define-nmethod (Module.compile p stmt?)
   ;; trampoline-code
   (when (Out-Env-trampoline? env)
      (scm2js-globals-init p)
      (let ((tail-obj (scm2js-global "TAIL_OBJECT"))
	    (max-tail-depth (Out-Env-max-tail-depth env)))
	 [assert (max-tail-depth) (fixnum? max-tail-depth)]
	 (template-display p
	    "var sc_tailTmp;\n"
	    "var sc_tailObj = ~a;\n" tail-obj
	    "~a.calls = ~a;\n" tail-obj max-tail-depth
	    "~a.MAX_TAIL_CALLs = ~a;\n" tail-obj max-tail-depth
	    "var sc_funTailCalls = ~a;\n"max-tail-depth)))

   ;; note: due to "push-declarations" these are not all declared variables.
   ;;   Just the ones that need to be declared by us.
   (with-access::Module this (declared-vars body)
      (for-each (lambda (var)
		   (with-access::Named-Var var (js-id)
		      (template-display p "var $js-id;\n")))
		declared-vars)

      ;; finally the body.
      (walk body p #t)))

(define-nmethod (Lambda.compile p stmt?)
   (widen!::Out-Lambda this
      (lvalue #f))
   (walk this p stmt?))

;; Llvalue/stmt?, if given, indicate, that this lambda should be assigned to
;; the given lvalue. stmt? is true, if we should treat this node, as if it was
;; a statement-node.
;; If there's a lvalue and it's not a stmt?, the return value of the compiled
;; code is unspecified. (ie, it doesn't anymore necessarily return the
;; lambda).
(define-nmethod (Out-Lambda.compile p stmt?)
   (define (vaarg-code vaarg nb-args)
      (with-access::Named-Var vaarg (js-id)
	 (template-display p
	    "var $js-id = null;\n"
	    "for (var $*tmp-var* = arguments.length - 1;"
	    "     $*tmp-var* >= $(number->string nb-args);"
	    "     --$*tmp-var*) {\n"
	    "  $js-id = sc_cons(arguments[$*tmp-var*], $js-id);\n"
	    "}\n")))

   (define (trampoline-start-code)
      (template-display p
	 "var sc_tailTmp;\n"
	 "var sc_tailObj;\n"
	 "var sc_funTailCalls = (sc_tailObj = ~a).calls;\n"
	 (scm2js-global "TAIL_OBJECT")))

   (define (declare/reset-counter-variables)
      (with-access::Lambda this (call/cc-nb-while-counters)
	 (if (zerofx? call/cc-nb-while-counters)
	     (template-display p "/*do nothing*/\n")
	     (let loop ((i 0))
		(unless (>=fx i call/cc-nb-while-counters)
		   (template-display p
		      "var ~a = 0;\n" (call/cc-counter-name i))
		   (loop (+fx i 1)))))))

   (define (restore-counter-variables)
      (with-access::Lambda this (call/cc-nb-while-counters)
	 (let loop ((i 0))
	    (unless (>=fx i call/cc-nb-while-counters)
	       (let ((name (call/cc-counter-name i)))
		  (template-display p
		     ;; if a counter-var is in the state, then we will enter
		     ;; the while during restoration and thus increment it at
		     ;; that time. For simplicity we will decrement _all_
		     ;; loop-variables before "jumping" to the target. Therefore
		     ;; we  assign "1" to those counters that are not yet
		     ;; active. They will then be decrement as well (but not
		     ;; incremented again during the jump).
		     "~a = sc_callCcState.~a || 1;\n" name name))
	       (loop (+fx i 1))))))

   (define (restore-frames)
      (with-access::Lambda this (call/cc-contained-scopes)
	 (for-each
	  (lambda (scope)
	     (with-access::Call/cc-Scope-Info scope (vars)
		(unless (null? vars) ;; should only happen for lambda-frame
		   (let ((frame-name (call/cc-frame-name scope)))
		      (template-display p
			 "if (sc_callCcFrame = sc_callCcState[~a]) {\n"
			 "  ~e"
			 "}\n"
			 frame-name
			 (each (lambda (var)
				  "~a = sc_callCcFrame.~a;\n"
				  (Named-Var-js-id var) (Named-Var-js-id var))
			       vars))))))
	  call/cc-contained-scopes)))

   (define (do-hoisted)
      (define (emit-commands rev-hoisted)
	 (if (null? rev-hoisted)
	     (template-display p "sc_callCcTmp")
	     (match-case (car rev-hoisted)
		((set! ?v)
		 (template-display p
		    "~a = ~e"
		    (Named-Var-js-id v)
		    (emit-commands (cdr rev-hoisted))))
		((return)
		 (template-display p
		    "return ~e" (emit-commands (cdr rev-hoisted))))
		(else
		 (error "out"
			"Internal Error: forgot to implement hoisted command"
			(car rev-hoisted))))))

      (with-access::Lambda this (call/cc-hoisted)
	 (unless (null? call/cc-hoisted)
	    (template-display p
	       "switch (sc_callCcIndex) {\n"
	       "  ~e"
	       "}\n"
	       (each (lambda (index/rev-hoisted)
			"case ~a: ~e; break;\n"
			(car index/rev-hoisted)
			(emit-commands (cdr index/rev-hoisted)))
		     call/cc-hoisted)))))

   (define (decrement-counter-variables)
      ;; by decrementing all counter-variables, we can increment the variable
      ;; unconditionally at the beginning of whiles, thus removing an 'if'.
      (with-access::Lambda this (call/cc-nb-while-counters)
	 (let loop ((i 0))
	    (unless (>=fx i call/cc-nb-while-counters)
	       (template-display p
		  "~a--;\n" (call/cc-counter-name i))
	       (loop (+fx i 1))))))
   
   (define (call/cc-start-code)
      (let ((storage (scm2js-global "CALLCC_STORAGE")))
	 (template-display p
	    "var sc_callCcTmp;\n"
	    "var sc_callCcIndex = 0;\n"
	    "var sc_callCcState = false;\n"
	    "if (~a" storage "['doCall/CcDefault?']) {\n"
	    "  ~e" (declare/reset-counter-variables)
	    "} else if (~a" storage "['doCall/CcRestore?']) {\n"
	    "  var sc_callCcFrame;\n"
	    "  sc_callCcState = ~a" storage ".pop();\n"
	    "  sc_callCcIndex = sc_callCcState.sc_callCcIndex;\n"
	    "  ~e" (restore-counter-variables)
	    "  ~e" (restore-frames)
	    "  sc_callCcTmp = ~a" storage ".callNext();\n"
	    "  ~e" (decrement-counter-variables)
	    "  ~e" (do-hoisted)
	    "} else { // root-fun\n"
	    "  return sc_callCcRoot(this, arguments);\n"
	    "}\n")))

   (define (finally-updates)
      (define (vars-update scope)
	 (with-access::Call/cc-Scope-Info scope (finally-vars)
	    (let ((frame-name (call/cc-frame-name scope)))
	       (template-display p
		  ;; note: the '=' is intentional (and should not be a '===')
		  "if (sc_callCcFrame = sc_callCcState[~a" frame-name "]) {\n"
		  "  ~e"
		  "}\n"
		  (each (lambda (var)
			   "sc_callCcFrame.~a = ~a;\n"
			   (Named-Var-js-id var) (Named-Var-js-id var))
			finally-vars)))))

      (with-access::Lambda this (call/cc-finally-scopes)
	 (unless (null? call/cc-finally-scopes)
	    (template-display p
	       "finally {\n"
	       "  if (sc_callCcState) {\n"
	       "    ~e" (for-each vars-update call/cc-finally-scopes)
	       "  }\n"
	       "}"))))

   ;; if the was already an old state, copy the relevant frames from the old
   ;; one into the new one.
   (define (save-frames)
      (define (save-frame scope)
	 ;; if this is a while-frame always save the counter-id
	 (let* ((frame-counter-id (Call/cc-Scope-Info-counter-id-nb scope))
		(frame-counter (and frame-counter-id
				    (call/cc-counter-name frame-counter-id))))
	    (when frame-counter
	       (template-display p
		  "sc_callCcState[~a] = ~a;\n"
		  frame-counter frame-counter)))

	 (let* ((frame-name (call/cc-frame-name scope))
		(call/cc? (Out-Env-call/cc? env)) ;; and not just suspend
		(while (Call/cc-Scope-Info-surrounding-while scope))
		(while-nb (and while (While-call/cc-counter-nb while)))
		(counter (and while-nb (call/cc-counter-name while-nb)))
		(vars (Call/cc-Scope-Info-vars scope)))
	    (template-display p
	       "if (sc_old_callCcState && "
	       (?? (and call/cc? counter)
		   "sc_old_callCcState.~a " counter " === ~a" counter " && ")
	       "    sc_old_callCcState[~a" frame-name"]) {\n"
	       "  sc_callCcState[~a" frame-name "] = "
	       "                sc_old_callCcState[~a" frame-name"];\n"
	       ;; no need to update the variables. If this was necessary it
	       ;; will happen in the finally clause.
	       "} else {\n"
	       "  sc_callCcState[~a" frame-name "] = { ~e };\n"
	       "}\n"
	       (separated ", "
			  (lambda (var)
			     "~a: ~a"
			     (Named-Var-js-id var) (Named-Var-js-id var))
			  vars))))

      (define (conditional-save-frame scope)
	 (with-access::Lambda this (call/cc-nb-indices)
	    (let* ((indices (Call/cc-Scope-Info-indices scope))
		   (sorted (sort <fx indices))
		   (continuous? (let loop ((i (car sorted))
					   (l (cdr sorted)))
				   (cond
				      ((null? l) #t)
				      ((=fx (+fx i 1) (car l))
				       (loop (car l) (cdr l)))
				      (else #f))))
		   (len (length sorted))
		   (single? (=fx len 1))
		   (always? (and continuous? (=fx len call/cc-nb-indices))))
	       (cond
		  (always? (save-frame scope))
		  (single?
		   (template-display p
		      "if (sc_callCcIndex === ~a) {\n" (car sorted)
		      "  ~e" (save-frame scope)
		      "}\n"))
		  ((and continuous? (=fx (car sorted) 1))
		   (template-display p
		      "if (sc_callCcIndex <= ~a) {\n"
		      (car (last-pair sorted))
		      "  ~e" (save-frame scope)
		      "}\n"))
		  ((and continuous?
			(=fx (car (last-pair sorted)) call/cc-nb-indices))
		   (template-display p
		      "if (sc_callCcIndex >= ~a) {\n"
		      (car sorted)
		      "  ~e" (save-frame scope)
		      "}\n"))
		  (continuous?
		   (template-display p
		      "if (sc_callCcIndex >= ~a && sc_callCcIndex <= ~a) {\n"
		      (car sorted) (car (last-pair sorted))
		      "  ~e" (save-frame scope)
		      "}\n"))
		  (else
		   (template-display p
		      "switch (sc_callCcIndex) {\n"
		      "  ~e\n" (each (lambda (i) "case ~a: " i) indices)
		      "  ~e"   (save-frame scope)
		      "}\n"))))))

      (with-access::Lambda this (call/cc-contained-scopes)
	 (for-each conditional-save-frame call/cc-contained-scopes)))

   (define (call/cc-end-code)
      (template-display p
	 "catch (sc_e) {\n"
	 "  if (sc_e instanceof sc_CallCcException && sc_e.backup "
	 "      && sc_callCcIndex !== 0) {\n" ;; if 0 then it was tail-call
	 "    var sc_old_callCcState = sc_callCcState;\n"
	 "    sc_callCcState = {};\n"
	 "    ~e" (save-frames)
	 "    sc_callCcState.sc_callCcIndex = sc_callCcIndex;\n"
	 "    sc_callCcState.this_ = this;\n"
	 "    sc_callCcState.callee = arguments.callee;\n"
	 "    sc_e.push(sc_callCcState);\n"
	 "  }\n"
	 "  throw sc_e;\n"
	 "} ~e" (finally-updates)))

   (define (split-formals/vaarg)
      (with-access::Lambda this (vaarg? formals)
	 (if (not vaarg?)
	     (values formals #f)
	     (let ((rev (reverse formals)))
		(values (reverse! (cdr rev))
			(car rev))))))
   
   (with-access::Out-Lambda this (lvalue declared-vars body vaarg? formals
					 call/cc? contains-trampoline-call?
					 location)
      (receive (formals-w/o-vaarg vaarg)
	 (split-formals/vaarg)

	 (let* ((declaration? (and #f ;; TODO: currently disabled as function
				   ;; declarations are only allowed at
				   ;; source-element positions. Not inside
				   ;; try/catch (for instance).
				   
				   ;source-element?
				   lvalue
				   stmt?
			     
				   (with-access::Ref lvalue (var)
				      (with-access::Var var (constant?)
					 constant?)) ;; not muted or anything
			     
				   ;; a declaration puts the assignment at the
				   ;; beginning of the scope. This is not good,
				   ;; if we want to access 'with' variables,
				   ;; that represent closures...
				   #f ;; TODO: correct this
			     ))
		;; a function can't be alone as statement. ie.
		;; function () {}; is not a valid statement.
		;; but
		;; (function () {}); is. So we wrap the function expression if
		;; we are in a statement. This only happens, if we don't assign
		;; to some lvalue.
		;;
		;; Note, that this only happens at top-level, where the last
		;; statement-value is returned.
		;;
		;; if we are an expression, and we need to do some assignments,
		;; we prefer parenthesis too. This happens, when we have a
		;; lvalue, or we are needing trampolines.
		(needs-parenthesis? (or (and stmt?
					     (not lvalue))
					(and (not stmt?)
					     lvalue)
					(Out-Env-debug? env))))
	    (template-display p
	       (?@ stmt? "~@;\n")
	       (?@ needs-parenthesis? "(~@)")
	       (?@ (Out-Env-debug? env)
		   "~a=~@,~a.sc_name=\"~a\",~a.sc_location=\"~a\",~a.sc_arity=~a,~a"
		   *tmp-var*
		   *tmp-var* (cond
				((Ref? lvalue) (Var-id (Ref-var lvalue)))
				(lvalue        (let ((op (open-output-string)))
						  (walk lvalue op #f)
						  (close-output-port op)))
				(else          ""))
		   *tmp-var* location
		   *tmp-var* (if vaarg?
				 (negfx (length formals))
				 (length formals))
		   *tmp-var*)
	       (?@ lvalue "~e = ~@" (walk lvalue p #f))
	       (?@ #t "function(~e) {\n ~e ~@ }" ;; always.
		   (separated ","
			      (lambda (e) "~e" (walk e p #f))
			      formals-w/o-vaarg)
		   (when vaarg?
		      (with-access::Ref vaarg (var)
			 (vaarg-code var (- (length formals) 1)))))
	       (?? (or call/cc? contains-trampoline-call?)
		   "~e" (scm2js-globals-init p))
	       (?? contains-trampoline-call? "~e" (trampoline-start-code))
	       (?@ call/cc?
		   "try {\n"
		   "  ~e"   (call/cc-start-code)
		   "  ~@"   ;; body
		   "} ~e\n" (call/cc-end-code))
	       "  ~e" ;; declared-vars
	       "  ~e" ;; body
	       (each (lambda (var)
			"var ~a;\n" (Named-Var-js-id var))
		     declared-vars)
	       (walk body p #t))))))

(define-nmethod (Frame-alloc.compile p stmt?)
   (template-display p
      (?@ stmt? "~@;\n") ;; should never happen.
      "{~e}" ;; literal-object creation
      (with-access::Frame-alloc this (vars)
	 (unless (null? vars)
	    (let loop ((vars vars))
	       (with-access::Named-Var (car vars) (js-id)
		  (if (null? (cdr vars))
		      (template-display p "$js-id : undefined")
		      (begin
			 (template-display p "$js-id : undefined, ")
			 (loop (cdr vars))))))))))

(define-nmethod (Frame-push.compile p stmt?)
   (with-access::Frame-push this (body frame-allocs)
      (with-access::Frame-alloc (car frame-allocs) (storage-var)
	 (with-access::Named-Var storage-var (js-id)
	    (template-display p
	       "with ($js-id) {\n"
	       "  ~e" (walk body p #t)
	       "}\n")))))

(define-nmethod (If.compile p stmt?)
   (with-access::If this (test then else)
      (cond
	 (stmt?
	  (template-display p
	     "if (~e) {\n" (compile-boolified p walk test)
	     "  ~e"        (walk then p #t)
	     "}\n"
	     (?? (not (Const? else)) ;; avoid common case 'undefined
		 "else {\n"
		 "  ~e"    (walk else p #t)
		 "}\n")))
	 ((and (Const? then)
	       (eq? (Const-value then) #t))
	  ;; should nearly never happen...
	  ;; usually (or X Y) is converted into:
	  ;;     (tmp = X, X!==false? tmp: Y)
	  (template-display p
	     "(~e || ~e)"
	     (compile-boolified p walk test)
	     (walk else p #f)))
	 ((and (Const? else)
	       (eq? (Const-value else) #f))
	  ;; happens more often.
	  (template-display p
	     "(~e && ~e)"
	     (compile-boolified p walk test)
	     (walk then p #f)))
	 (else
	  (template-display p
	     "(~e? ~e: ~e)"
	     (compile-boolified p walk test)
	     (walk then p #f)
	     (walk else p #f))))))

(define-nmethod (Case.compile p stmt?)
   (with-access::Case this (key clauses)
      (template-display p
	 "switch (~e) {\n" (walk key p #f)
	 "  ~e"    (for-each (lambda (clause) (walk clause p #t)) clauses)
	 "}\n")))

(define-nmethod (Clause.compile p stmt?)
   (with-access::Clause this (default-clause? consts expr)
      (if default-clause?
	  (template-display p
	     "default:\n"
	     "~e" (walk expr p #t)
	     "break;\n")
	  (template-display p
	     ;; consts
	     "~e" (for-each (lambda (const)
			       (template-display p
				  "case ~e:\n" (walk const p #f)))
			    consts)
	     ;; body
	     "~e" (walk expr p #t)
	     "break;\n"))))

(define-nmethod (Decl-Set!.compile p stmt?)
   [assert (stmt?) stmt?]
   (with-access::Set! this (lvalue val)
      (template-display p
	 "var ~e;\n" (compile-unoptimized-set! p walk this))))

(define-nmethod (Set!.compile p stmt?)
   (with-access::Set! this (lvalue val)
      (if (Lambda? val)
	  (begin
	     (widen!::Out-Lambda val
		(lvalue lvalue))
	     (walk val p stmt?))
	  (template-display p
	     (?@ stmt? "~@;\n")
	     (?@ (not stmt?) "(~@)") ;; for now...
	     "~e" (compile-set! p walk this)))))

(define-nmethod (Begin.compile p stmt?)
   (define (call/cc-begin-out)
      (with-access::Begin this (exprs call/cc-ranges)
	 (template-display p
	    "switch (sc_callCcIndex) {\n"
	    " case 0:\n"
	    "  ~e"
	    "}\n"
	    (for-each (lambda (expr range)
			 (template-display p
			    (?? (and range (not (null? range)))
				"~e\n" ;; the cases
				(each (lambda (index)
					 "case ~a: " index)
				      range))
			    "~e" (walk expr p #t)))
		      exprs
		      call/cc-ranges))))
   
   (with-access::Begin this (exprs call/cc? call/cc-ranges)
      (cond
	 ((and call/cc? (not (null? call/cc-ranges)))
	  (call/cc-begin-out))
	 (stmt?
	  ;; we do not need { }, as all statements that could contain blocks
	  ;; already have them (or do not need them).
	  (for-each (lambda (n) (walk n p #t))
		    exprs))
	 (else
	  (template-display p
	     "(~e)"
	     (separated ", "
			(lambda (e) "~e" (walk e p #f))
			exprs))))))

(define-nmethod (Call/cc-Resume.compile p stmt?)
   (template-display p
      (?@ stmt? "~@;\n")
      "sc_callCcIndex = 0"))

(define-nmethod (Call.compile p stmt?)
   (define (compile-operator)
      ;; if the operator is a var-ref and contains a "." we have to do
      ;; some tricks: frame.f() would give 'frame' to f as this-object.
      ;; by using the sequence-construct we can avoid that:
      ;; (0,frame.f)() sends the global-object as 'this'-object.
      (with-access::Call this (operator)
	 (if (and (Ref? operator)
		  (with-access::Ref operator (var)
		     (with-access::Named-Var var (js-id)
			(string-index js-id #\.))))
	     (with-access::Ref operator (var)
		(with-access::Named-Var var (js-id)
		   (template-display p
		      "(0, $js-id)")))
	     (walk operator p #f))))

   (define (compile-operands)
      (with-access::Call this (operands)
	 (template-display p
	    "~e"
	    (separated ", "
		       (lambda (operand) "~e" (walk operand p #f))
		       operands))))
      
   (define (compile-trampoline-call)
      (define (do-trampoline-call tail-obj)
	 (with-access::Call this (operator)
	    (template-display p
	       "(~a.f = ~e," tail-obj (walk operator p #f)
	       " ~a.f(~e))"  tail-obj (compile-operands))))

      (let ((operator (Call-operator this))
	    (max-tail-depth (Out-Env-max-tail-depth env)))
	 (with-access::Call this (operator)
	    (template-display p
	       "(this === sc_tailObj?\n"
	       "     (!sc_funTailCalls?\n"  ;; == 0
	       "          (this.args = [~e],"   (compile-operands)
	       "           this.f = ~e,"        (walk operator p #f)
	       "           this)\n"
	       "       :\n"
	       "          (this.calls = sc_funTailCalls - 1,\n"
	       "          ~e))\n"         (do-trampoline-call "this")
	       "  :\n"
	       "     (sc_tailObj.calls = ~a,\n" max-tail-depth
	       "      sc_tailTmp = ~e,\n" (do-trampoline-call "sc_tailObj")
	       "      (sc_tailObj === sc_tailTmp?"
	       "                 sc_tailObj.restart()"
	       "               : sc_tailTmp)))"))))

   (with-access::Call this (operator operands call/cc? call/cc-index
				     trampoline? location)
      (template-display p
	 (?@ stmt? "~@;\n")
	 (?@ #t "(~@)") ;;  was (not stmt?). now always. even for stmt.
	 (?@ (and call/cc? call/cc-index)
	     "sc_callCcIndex = ~a, ~@" call/cc-index)
	 "~e"
	 (cond
	    ((and (Out-Env-optimize-calls? env)
		  (compile-optimized-call p walk operator operands))
	     'do-nothing) ;; already printed
	    (trampoline?
	     (compile-trampoline-call))
	    ((and (Out-Env-debug? env)
		  (not (Out-Env-call/cc? env)))
	     (let ((len (length operands)))
		(template-display p
		   "sc_arity_check(~e, ~a)(~e)"
		   (compile-operator) len (compile-operands))))
	    (else
	     (template-display p
		"~e(~e)" (compile-operator) (compile-operands)))))))

(define-nmethod (While.compile p stmt?)
   (define (finally-outs scope)
      (with-access::Call/cc-Scope-Info scope (finally-vars)
	 (let ((frame-name (call/cc-frame-name scope))
	       (counter (call/cc-counter-name
			 (While-call/cc-counter-nb this))))
	    (template-display p
	       "if (sc_callCcState.~a === ~a ? sc_callCcState[~a] : false) {\n"
	       ;; state.counter === counter && state.frame
	       counter counter frame-name
	       "  ~e"
	       "}\n"
	       (each (lambda (var)
			"sc_callCcState[~a].~a = ~a;\n"
			;; state.frame.var-id = var-id;
			frame-name (Named-Var-js-id var) (Named-Var-js-id var))
		     finally-vars)))))

   (define (body-out)
      (with-access::While this (body call/cc? call/cc-finally-scopes
				     call/cc-counter-nb)
	 (template-display p
	    (?? call/cc? "~a++;\n" (call/cc-counter-name call/cc-counter-nb))
	    (?@ (not (null? call/cc-finally-scopes))
		"try {"
		"  ~@"
		"} finally {"
		"  if (sc_callCcState) {"
		"    ~e"
		"  }"
		"}\n"
		(for-each finally-outs call/cc-finally-scopes))
	    "~e" (walk body p #t))))

   (with-access::While this (init test body label call/cc?)
      (template-display p
	 ;; init
	 (?? (not (Const? init)) "~e" (walk init p #t))
	 ;; label
	 (?? (not (eq? label (default-label)))
	     "~a:\n" (mangle-JS-sym (Label-id label))))

      (cond
	 ((and (Const? test)
	       (eq? (Const-value test) #t))
	  (template-display p
	     "do {\n"
	     "  ~e" (body-out)
	     "} while (true);\n"))
	 ((not call/cc?)
	  (template-display p
	     "while (~e) {\n" (compile-boolified p walk test)
	     "  ~e"           (walk body p #t)
	     "}\n"))
	 (else
	  (template-display p
	     "while (sc_callCcIndex || (~e)) {\n" (walk test p #f)
	     "  ~e"                               (body-out)
	     "}\n")))))
			  
(define-nmethod (Continue.compile p stmt?)
   (with-access::Continue this (label)
      (if (eq? label (default-label))
	  (template-display p
	     "continue;\n")
	  (template-display p
	     "continue ~a;\n" (mangle-JS-sym (Label-id label))))))

(define-nmethod (Return.compile p stmt?)
   (with-access::Return this (val)
      (template-display p
	 "return ~e;\n" (walk val p #f))))

(define-nmethod (Labeled.compile p stmt?)
   (with-access::Labeled this (label body)
      (with-access::Label label (id)
	 (template-display p
	    "~a: {\n" (mangle-JS-sym id)
	    "  ~e"    (walk body p #t)
	    "}\n"))))

(define-nmethod (Break.compile p stmt?)
   (with-access::Break this (label val)
      (with-access::Label label (id)
	 (template-display p
	    "{\n"
	    "  ~e" (walk val p #t)
	    "  break ~a;\n" (if (eq? label (default-label))
				""
				(mangle-JS-sym id))
	    "}\n"))))

; (define-nmethod (Pragma.compile p stmt?)
;    (with-access::Pragma this (str)
;       (template-display p
; 	 (?@ stmt? "~@;\n")
; 	 "(~a)" str)))


(define-nmethod (Pragma.compile p stmt?)
   (with-access::Pragma this (str)
      (with-access::Out-Env env (pp? pragmas)
	 (when pp?
	    (add-pragma! pragmas str))
	 (let ((to-be-displayed (if pp?
				    ;; placeholder
				    #a000
				    str)))
	    (template-display p
	       (?@ stmt? "~@;\n")
	       "(~a)" to-be-displayed)))))

(define (add-pragma! pragmas::Pragmas p)
   (with-access::Pragmas pragmas (lock last-pragma)
      (with-lock lock
	 (lambda ()
	    (let ((tmp (list p)))
	       (set-cdr! last-pragma tmp)
	       (set! last-pragma tmp))))))

(define (consume-next-pragma! pragmas::Pragmas)
   (with-access::Pragmas pragmas (lock pragmas)
      (with-lock lock
	 (lambda ()
	    (when (null? (cdr pragmas))
	       ;; should never happen
	       (error "out"
		      "Internal Error: consume-pragma call without pragma"
		      #f))
	    (let ((res (cadr pragmas)))
	       (set! pragmas (cdr pragmas))
	       res)))))


