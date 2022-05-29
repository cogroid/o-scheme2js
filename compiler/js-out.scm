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

(module js-out
   (import js-nodes
	   js-lexer)
   (export (js-out tree::Node port::output-port
		   #!key (compress?::bool #f) (indent-width::bint 3))))

(define (valid-js-id? str #!key (strip-delimiters? #f))
   ;; skip first and last '"'
   (let ((start (if strip-delimiters? 1 0))
	 (stop (if strip-delimiters?
		   (-fx (string-length str) 1)
		   (string-length str))))
      (and (>fx stop start)
	   (let loop ((i 1))
	      (cond
		 ((>=fx i stop)
		  ;; avoid the case where the str is for instance
		  ;; "null" or "if", etc.
		  (not (JS-reserved-id? (substring str start stop))))
		 ((or (char-alphabetic? (string-ref str i))
		      (char=? #\_ (string-ref str i)))
		  (loop (+fx i 1)))
		 ((and (>fx i start)
		       (char-numeric? (string-ref str i)))
		  (loop (+fx i 1)))
		 (else #f))))))

(define (js-out tree port #!key (compress?::bool #f) (indent-width::bint 3))
   ;; TODO: indentation-width is global variable and hence not thread-safe.
   ;;       should not matter in the case of Hop.
   (set! *indentation-width* indent-width)
   (stmt-out tree 0 port compress?))

(define *indentation-width* 3)

(define (indent++ indent)
   (+ indent *indentation-width*))

(define (indent! indent p compress?)
   (unless compress?
      (let loop ((i 0))
	 (when (<fx i indent)
	    (display #\space p)
	    (loop (+fx i 1))))))

(define (newline-out p compress?)
   (unless compress?
      (display #\newline p)))

(define (space-out p compress?)
   (unless compress?
      (display #\space p)))

(define (op-out op p compress?)
   (case op
      ((OR) (display "||" p))
      ((BIT_OR) (display "|" p))
      (else (display op p))))

(define (block-body body indent p compress?
		    needs-separation? newline-after?) 
   (let ((body-block? (Block? body)))
      (if body-block?
	  (begin
	     (space-out p compress?)
	     (block-out body indent p compress?
			#f newline-after?)
	     #t)
	  (begin
	     (if (and compress?
		      needs-separation?)
		 (display #\space p)
		 ;; when compress? is set newline-out does nothing
		 (newline-out p compress?))
	     (stmt-out body (indent++ indent) p compress?)
	     #f))))

(define (Block-out-without-braces stmt indent p compress?)
   (if (Block? stmt)
       (with-access::Block stmt (stmts)
	  (for-each (lambda (n)
		       (stmt-out n indent p compress?))
		    stmts))
       (stmt-out stmt indent p compress?)))

(define (stmt->block stmt)
   (cond
      ((Block? stmt)
       stmt)
      ((NOP? stmt)
       (instantiate::Block (stmts '())))
      (else
       (instantiate::Block (stmts (list stmt))))))


(define (primary-expr? n)
   (or (This? n)
       (Ref? n)
       (Literal? n)
       (RegExp? n) ;; strictly speaking a RegExp is a literal, too.
       (Array? n)
       (Obj-Init? n)))

;; we merge NewExpressions and MemberExpressions
;; in other words we will not print 'new's without parenthesis.
;; we could readd them by adding an additional flag to the expr-outs

(define (call-expr? n)
   (or (primary-expr? n)
       (Fun? n)
       (Named-Fun? n)
       (Access? n)
       (New? n)
       (Call? n)))

(define (lhs-expr? n) (call-expr? n))

(define (unary-expr? n)
   (or (lhs-expr? n)
       (Postfix? n)
       (Unary? n)))

(define (*-or-unary-expr? n)
   (or (unary-expr? n)
       (and (Binary? n)
	    (eq? (Binary-op n) '*))))

(define (mult-expr? n)
   (or (unary-expr? n)
       (and (Binary? n)
	    (let ((op (Binary-op n)))
	       (case op
		  ((* / %) #t)
		  (else #f))))))

(define (add-expr? n)
   (or (mult-expr? n)
       (and (Binary? n)
	    (let ((op (Binary-op n)))
	       (case op
		  ((+ -) #t)
		  (else #f))))))

(define (shift-expr? n)
   (or (add-expr? n)
       (and (Binary? n)
	    (let ((op (Binary-op n)))
	       (case op
		  ((<< >> >>>) #t)
		  (else #f))))))

(define (rel-expr? n)
   (or (shift-expr? n)
       (and (Binary? n)
	    (let ((op (Binary-op n)))
	       (case op
		  ((< > <= >= instanceof in) #t)
		  (else #f))))))

(define (eq-expr? n)
   (or (rel-expr? n)
       (and (Binary? n)
	    (let ((op (Binary-op n)))
	       (case op
		  ((== != === !==) #t)
		  (else #f))))))

(define (bit-and-expr? n)
   (or (eq-expr? n)
       (and (Binary? n)
	    (eq? (Binary-op n) '&))))

(define (bit-xor-expr? n)
   (or (bit-and-expr? n)
       (and (Binary? n)
	    (eq? (Binary-op n) '^))))

(define (bit-or-expr? n)
   (or (bit-xor-expr? n)
       (and (Binary? n)
	    (eq? (Binary-op n) 'BIT_OR))))

(define (and-expr? n)
   (or (bit-or-expr? n)
       (and (Binary? n)
	    (eq? (Binary-op n) '&&))))
   
(define (or-expr? n)
   (or (and-expr? n)
       (and (Binary? n)
	    (eq? (Binary-op n) 'OR))))

(define (assig-expr? n)
   (or (or-expr? n)
       (Cond? n)
       (Assig? n)))

(define (expr? n)
   (cond-expand
      (bigloo-debug (unless (or (assig-expr? n)
				(Sequence? n)
				(Pragma? n))
		       (error "expr?"
			      "Internal error: forgot an expression"
			      (class-name (object-class n))))
		    #t)
      (else #t)))

(define (no-requirement n) #t)

(define-generic (stmt-out this::Node indent p compress?)
   ;; if we are here the node is probably just an expression.
   (indent! indent p compress?)
   (nested-expr-out this expr? #f #t indent p compress?)
   (display #\; p)
   (newline-out p compress?))

(define-generic (expr-out this::Node in-for-init? stmt-begin?
				indent port compress?)
   (error "expr-out"
	  "Internal error: forgot node-type"
	  (class-name (object-class this))))

(define (nested-expr-out this::Node of-required-type?::procedure
			 in-for-init? stmt-begin?
			 indent p compress?)
   (let* ((needs-parentheses? (or (not (of-required-type? this))
				  (and in-for-init?
				       (Binary? this)
				       (eq? (Binary-op this) 'in))
				  (and stmt-begin?
				       (or (Obj-Init? this)
					   (Named-Fun? this)
					   (Fun? this)
					   (Pragma? this)))))
	  ;; if we are inside parenthesis we don't care for the 'for' anymore.
	  (new-in-for-init? (and in-for-init? (not needs-parentheses?)))
	  ;; nor are we at the beginning of a stmt anymore.
	  (new-stmt-begin? (and stmt-begin? (not needs-parentheses?))))
      (if needs-parentheses?
	  (begin
	     (display #\( p)
	     (expr-out this new-in-for-init? new-stmt-begin?
		       indent p compress?)
	     (display #\) p))
	  ;; at tail position.
	  (expr-out this new-in-for-init? new-stmt-begin?
		    indent p compress?))))

(define-method (stmt-out this::Program indent p compress?)
   (with-access::Program this (body)
      (Block-out-without-braces body 0 p compress?)))

(define (block-out this::Block indent p compress?
		   indent? new-line-after?)
   (with-access::Block this (stmts)
      (when indent? (indent! indent p compress?))
      (display #\{ p)
      (newline-out p compress?)
      (for-each (lambda (n)
		   (stmt-out n (indent++ indent) p compress?))
		stmts)
      (indent! indent p compress?)
      (display #\} p)
      (when new-line-after? (newline-out p compress?))))
   
(define-method (stmt-out this::Block indent p compress?)
   (block-out this indent p compress?
	      #t #t))

(define-method (expr-out this::Sequence in-for-init? stmt-begin?
			 indent p compress?)
   ;; we require expr?s as elements.
   ;; -> a,(b,c) and (a,b),c will become a,b,c
   (with-access::Sequence this (exprs)
      (nested-expr-out (car exprs) expr? in-for-init? stmt-begin?
		       indent p compress?)
      (for-each (lambda (n)
		   (display #\, p)
		   (space-out p compress?)
		   (nested-expr-out n expr? in-for-init? #f
				    (indent++ indent) p compress?))
		(cdr exprs))))

(define (var-decl-list-out this::Var-Decl-List indent p compress?)
   (display "var " p)
   (with-access::Var-Decl-List this (vars)
      ;; we don't require any kind of expression type.
      ;; we know the expression is either a ref (simple) or an assignment.
      ;; and assignments won't allow ',' anyways.
      (nested-expr-out (car vars) no-requirement #f #f
		       (+fx indent 4) p compress?)
      (for-each (lambda (n)
		   (display #\, p)
		   (space-out p compress?)
		   (nested-expr-out n no-requirement #f #f
				    (indent++ (+fx indent 4)) p compress?))
		(cdr vars))))
		       
(define-method (expr-out this::Var-Decl-List in-for-init? stmt-begin?
			 indent p compress?)
   (var-decl-list-out this indent p compress?))

(define-method (stmt-out this::Var-Decl-List indent p compress?)
   (indent! indent p compress?)
   (var-decl-list-out this indent p compress?)
   (display #\; p)
   (newline-out p compress?))

(define-method (expr-out this::Ref in-for-init? stmt-begin?
			 indent p compress?)
   (with-access::Ref this (id)
      (display id p)))

(define-method (stmt-out this::NOP indent p compress?)
   (indent! indent p compress?)
   (display #\; p)
   (newline-out p compress?))

(define (if-out this::If indent p compress? indent?)
   (with-access::If this (test then else)
      (when (and (If? then)            ;; nested if
		 (NOP? (If-else then)) ;; has no else-branch
		 (not (NOP? else)))    ;; but we have one
	 (set! then (instantiate::Block (stmts (list then))))) ;; protect our else
      (when indent? (indent! indent p compress?))
      (display "if" p)
      (space-out p compress?)
      (display #\( p)
      (nested-expr-out test expr? #f #f (+fx indent 4) p compress?)
      (display #\) p)
      (let* ((else-branch? (not (NOP? else)))
	     (then-block? (block-body then indent p compress?
				      #f (not else-branch?))))
	 (when else-branch?
	    (if then-block?
		(space-out p compress?)
		(indent! indent p compress?))
	    (display "else" p)
	    (if (If? else)
		(begin
		   (display #\space p)
		   (if-out else indent p compress? #f))
		(block-body else indent p compress? #t #t))))))

(define-method (stmt-out this::If indent p compress?)
   (if-out this indent p compress? #t))

(define-method (stmt-out this::For indent p compress?)
   (with-access::For this (init test incr body)
      (indent! indent p compress?)
      (display "for" p)
      (space-out p compress?)
      (display #\( p)
      (when init
	 ;; here we pass in-for-init? as #t
	 ;; no-requirement as this could be a var-decl-list too.
	 (nested-expr-out init no-requirement #t #f
			  (+fx indent 5) p compress?))
      (display #\; p)
      (when test
	 (space-out p compress?)
	 (nested-expr-out test expr? #f #f
			  (indent++ (+fx indent 5)) p compress?))
      (display #\; p)
      (when incr
	 (space-out p compress?)
	 (nested-expr-out incr expr? #f #f
			  (indent++ (+fx indent 7)) p compress?))
      (display #\) p)
      (block-body body indent p compress? #f #t)))

(define-method (stmt-out this::While indent p compress?)
   (with-access::While this (test body)
      (indent! indent p compress?)
      (display "while" p)
      (space-out p compress?)
      (display #\( p)
      (nested-expr-out test expr? #f #f (+fx indent 7) p compress?)
      (display #\) p)
      (block-body body indent p compress? #f #t)))

(define-method (stmt-out this::Do indent p compress?)
   (with-access::Do this (body test)
      (indent! indent p compress?)
      (display "do" p)
      (if (block-body body indent p compress? #t #f)
	  (space-out p compress?)
	  (indent! indent p compress?))
      (display "while" p)
      (space-out p compress?)
      (display #\( p)
      (nested-expr-out test expr? #f #f (+fx indent 7) p compress?)
      (display ");" p)
      (newline-out p compress?)))

(define-method (stmt-out this::For-In indent p compress?)
   (with-access::For-In this (lhs obj body)
      (indent! indent p compress?)
      (display "for" p)
      (space-out p compress?)
      (display #\( p)
      ;; note that we set in-for-init? to true.
      ;; no-requirement as this could be a var-decl-list too.
      (nested-expr-out lhs no-requirement #t #f
		       (+fx indent 5) p compress?)
      (display " in " p)
      (nested-expr-out obj expr? #f #f
		       (indent++ (+fx indent 5)) p compress?)
      (display #\) p)
      (block-body body indent p compress? #f #t)))

(define-method (stmt-out this::Continue indent p compress?)
   (with-access::Continue this (id)
      (indent! indent p compress?)
      (display "continue" p)
      (when id
	 (display #\space p)
	 (display id p))
      (display #\; p)
      (newline-out p compress?)))

(define-method (stmt-out this::Break indent p compress?)
   (with-access::Break this (id)
      (indent! indent p compress?)
      (display "break" p)
      (when id
	 (display #\space p)
	 (display id p))
      (display #\; p)
      (newline-out p compress?)))

(define-method (stmt-out this::Return indent p compress?)
   (with-access::Return this (val)
      (indent! indent p compress?)
      (display "return" p)
      (when val
	 (display #\space p)
	 (nested-expr-out val expr? #f #f (+fx indent 6) p compress?))
      (display #\; p)
      (newline-out p compress?)))

(define-method (stmt-out this::With indent p compress?)
   (with-access::With this (obj body)
      (indent! indent p compress?)
      (display "with" p)
      (space-out p compress?)
      (display #\( p)
      (nested-expr-out obj expr? #f #f (+fx indent 6) p compress?)
      (display #\) p)
      (block-body body indent p compress? #f #t)))

(define-method (stmt-out this::Switch indent p compress?)
   (with-access::Switch this (key cases)
      (indent! indent p compress?)
      (display "switch" p)
      (space-out p compress?)
      (display #\( p)
      (nested-expr-out key expr? #f #f (+fx indent 8) p compress?)
      (display #\) p)
      (space-out p compress?)
      (display #\{ p)
      (newline-out p compress?)
      (for-each (lambda (n)
		   (stmt-out n indent p compress?))
		cases)
      (indent! indent p compress?)
      (display #\} p)
      (newline-out p compress?)))

(define-method (stmt-out this::Case indent p compress?)
   (with-access::Case this (expr body)
      (indent! indent p compress?)
      (display "case " p)
      (nested-expr-out expr expr? #f #f (+fx indent 6) p compress?)
      (display #\: p)
      (newline-out p compress?)
      (Block-out-without-braces body (indent++ indent) p compress?)))

(define-method (stmt-out this::Default indent p compress?)
   (with-access::Default this (body)
      (indent! indent p compress?)
      (display "default:" p)
      (newline-out p compress?)
      (Block-out-without-braces body (indent++ indent) p compress?)))

(define-method (stmt-out this::Throw indent p compress?)
   (with-access::Throw this (expr)
      (indent! indent p compress?)
      (display "throw " p)
      (nested-expr-out expr expr? #f #f (+fx indent 6) p compress?)
      (display #\; p)
      (newline-out p compress?)))

(define-method (stmt-out this::Try indent p compress?)
   (with-access::Try this (body catch finally)
      (indent! indent p compress?)
      (display "try" p)
      ;; body must be a block. Just in case someone removed the block...
      (let ((needs-separation #t) ;; don't care as it won't be used
	    (new-line-after? #f))
      (block-body (stmt->block body) indent p compress?
		  needs-separation new-line-after?)
      (when catch
	 (space-out p compress?)
	 (stmt-out catch indent p compress?))
      (when finally
	 (space-out p compress?)
	 (display "finally" p)
	 (block-body (stmt->block finally) indent p compress? #t #f))
      (newline-out p compress?))))

(define-method (stmt-out this::Catch indent p compress?)
   (with-access::Catch this (exception body)
      (display "catch(" p)
      ;; can only be a Decl, so we test for 'primary-expr?'
      (nested-expr-out exception primary-expr? #f #f indent p compress?)
      (display #\) p)
      ;; catch must be block.
      (block-body (stmt->block body) indent p compress? #f #f)))

(define-method (stmt-out this::Labeled indent p compress?)
   (with-access::Labeled this (id body)
      (indent! indent p compress?)
      (display id p)
      (display #\: p)
      (block-body body indent p compress? #f #t)))

(define (function-out fun name indent p compress?)
   (with-access::Fun fun (params body)
      (display "function" p)
      (when name
	 ;; name must be a Decl. -> test for primary-expr?
	 (display #\space p)
	 (nested-expr-out name primary-expr? #f #f indent p compress?))
      (display #\( p)
      (unless (null? params)
	 ;; params can only be 'Param's. we test for primary-expr?
	 (nested-expr-out (car params) primary-expr? #f #f
			  (+fx indent 10) p compress?)
	 (for-each (lambda (param)
		      (display #\, p)
		      (space-out p compress?)
		      (nested-expr-out param primary-expr? #f #f
				       (indent++ (+fx indent 10)) p compress?))
		   (cdr params)))
      (display #\) p)
      (block-body (stmt->block body) indent p compress? #f #f)))

(define-method (stmt-out this::Fun-Binding indent p compress?)
   (with-access::Fun-Binding this (lhs rhs)
      (indent! indent p compress?)
      (function-out rhs lhs indent p compress?)
      (newline-out p compress?)))

(define-method (expr-out this::Named-Fun in-for-init? stmt-begin?
			 indent p compress?)
   (with-access::Named-Fun this (name fun)
      (function-out fun name indent p compress?)))

(define-method (expr-out this::Fun in-for-init? stmt-begin?
			 indent p compress?)
   (function-out this #f indent p compress?))

(define-method (expr-out this::Assig in-for-init? stmt-begin?
			 indent p compress?)
   (with-access::Assig this (lhs rhs)
      (nested-expr-out lhs lhs-expr? in-for-init? stmt-begin?
		       indent p compress?)
      (space-out p compress?)
      (display #\= p)
      (space-out p compress?)
      (nested-expr-out rhs assig-expr? in-for-init? #f
		       (indent++ indent) p compress?)))

(define-method (expr-out this::Assig-op in-for-init? stmt-begin?
			 indent p compress?)
   (with-access::Assig-op this (lhs op rhs)
      (nested-expr-out lhs lhs-expr? in-for-init? stmt-begin?
		       indent p compress?)
      (space-out p compress?)
      (op-out op p compress?)
      (display #\= p)
      (space-out p compress?)
      (nested-expr-out rhs assig-expr? in-for-init? #f
		       (indent++ indent) p compress?)))

(define-method (expr-out this::Cond in-for-init? stmt-begin?
			 indent p compress?)
   (with-access::Cond this (test then else)
      (nested-expr-out test or-expr? in-for-init? stmt-begin?
		       indent p compress?)
      (display #\? p)
      (space-out p compress?)
      ;; even if we are in a for-init, the middle part is not affected.
      (nested-expr-out then assig-expr? #f #f
		       (indent++ indent) p compress?)
      (display #\: p)
      (space-out p compress?)
      (nested-expr-out else assig-expr? in-for-init? #f
		       (indent++ (indent++ indent)) p compress?)))

(define-method (expr-out this::Call in-for-init? stmt-begin?
			 indent p compress?)
   (with-access::Call this (fun args)
      ;; our requirement is much simpler as the spec lets expect.
      ;; this is due to the fact that we always print parenthesis after 'new'
      ;; expressions.
      (nested-expr-out fun lhs-expr? in-for-init? stmt-begin?
		       indent p compress?)
      (display #\( p)
      (unless (null? args)
	 (nested-expr-out (car args) assig-expr? #f #f
			  (indent++ indent) p compress?)
	 (for-each (lambda (arg)
		      (display #\, p)
		      (space-out p compress?)
		      (nested-expr-out arg assig-expr? #f #f
				       (indent++ indent) p compress?))
		   (cdr args)))
      (display #\) p)))

(define-method (expr-out this::Binary in-for-init? stmt-begin?
			 indent p compress?)
   (with-access::Binary this (lhs op rhs)
      (let ((lhs-req (case op
			((* / %) mult-expr?)
			((+ -) add-expr?)
			((<< >> >>>) shift-expr?)
			((< > <= >= instanceof in) rel-expr?)
			((== != === !==) eq-expr?)
			((&) bit-and-expr?)
			((^) bit-xor-expr?)
			((BIT_OR) bit-or-expr?)
			((&&) and-expr?)
			((OR) or-expr?)
			(else (error "lhs-req"
				     "Internal Error: missed an op"
				     op))))
	    ;; in most cases where there is a comment we deviate from the spec.
	    ;; this should yield less parenthesis.
	    (rhs-req (case op
			;; we could theoretically optimize x*(y*z) to
			;; (x*y)*z (which would be written as x*y*z), but
			;; the precision is not necessarily the same.
			;((*) *-or-unary-expr?) ;; x*(y*z) <~=> (x*y)*z
			((/ % *) unary-expr?)
			;; we can't optimize '+' as x+(y+z) <!=> (x+y)+z
			;; ex:  "t"+(1+3) -> "t4"
			;;  but ("t"+1)+3 -> "t13"
			((+ -) mult-expr?)
			((<< >> >>>) add-expr?)
			((< > <= >= instanceof in) shift-expr?)
			((== != === !==) rel-expr?)
			((&) bit-and-expr?) ;; x&(y&z) <=> (x&y)&z
			((^) bit-xor-expr?) ;; x^(y^z) <=> (x^y)^z
			((BIT_OR) bit-or-expr?) ;; x|(y|z) <=> (x|y)|z
			((&&) and-expr?) ;; x&&(y&&z) <=> (x&&y)&&z
			((OR) or-expr?) ;; x||(y||z) <=> (x||y)||z
			(else (error "rhs-req"
				     "Internal Error: missed an op"
				     op)))))
	 (nested-expr-out lhs lhs-req in-for-init? stmt-begin?
			  indent p compress?)
	 (case op
	    ((in instanceof)
	     ;; spaces are necessary.
	     (display #\space p) (display op p) (display #\space p))
	    (else
	     (space-out p compress?)
	     (op-out op p compress?)
	     (space-out p compress?)))
	 (nested-expr-out rhs rhs-req in-for-init? stmt-begin?
			  (indent++ indent) p compress?))))
   
(define-method (expr-out this::Unary in-for-init? stmt-begin?
			 indent p compress?)
   (with-access::Unary this (op expr)
      (display op p)
      (case op
	 ((delete void typeof) (display #\space p))
	 (else 'do-nothing))
      (nested-expr-out expr unary-expr? in-for-init? #f
		       (indent++ indent) p compress?)))

(define-method (expr-out this::Postfix in-for-init? stmt-begin?
			 indent p compress?)
   (with-access::Postfix this (expr op)
      (nested-expr-out expr lhs-expr? in-for-init? stmt-begin?
		       indent p compress?)
      (display op p)))

(define-method (expr-out this::New in-for-init? stmt-begin? indent p compress?)
   (with-access::New this (class args)
      (display "new " p)
      ;; the requirement is much simpler as one could expect from the spec as
      ;; we are always printing the parenthesis of 'new's.
      (nested-expr-out class call-expr? #f #f (+fx indent 4) p compress?)
      (display #\( p)
      (unless (null? args)
	 (nested-expr-out (car args) assig-expr? #f #f
			  (indent++ (+fx indent 4)) p compress?)
	 (for-each (lambda (arg)
		      (display #\, p)
		      (space-out p compress?)
		      (nested-expr-out arg assig-expr? #f #f
				       (indent++ (+fx indent 4)) p compress?))
		   (cdr args)))
      (display #\) p)))

(define-method (expr-out this::Access in-for-init? stmt-begin?
			 indent p compress?)
   (with-access::Access this (obj field)
      (if (and (String? field)
	       (valid-js-id? (String-val field) :strip-delimiters? #t)
	       (not (Number? obj))
	       (not (Pragma? obj)))
	  (let ((str (String-val field)))
	     (nested-expr-out obj call-expr? in-for-init? stmt-begin?
			      indent p compress?)
	     (display #\. p)
	     ;; strip delimiters
	     (display (substring str 1 (-fx (string-length str) 1)) p))
	  (begin
	     (nested-expr-out obj call-expr? in-for-init? stmt-begin?
			      indent p compress?)
	     (display #\[ p)
	     (nested-expr-out field expr? #f #f (indent++ indent) p compress?)
	     (display #\] p)))))

(define-method (expr-out this::This in-for-init? stmt-begin?
			 indent p compress?)
   (display "this" p))

(define-method (expr-out this::Literal in-for-init? stmt-begin?
			 indent p compress?)
   (with-access::Literal this (val)
      (error "Literal-out" "Internal Error: forgot literal type" val)))

(define-method (expr-out this::Undefined in-for-init? stmt-begin?
			 indent p compress?)
   ;; This is actually not really correct.
   ;; By writing 'undefined' we reference the 'undefined' variable which could
   ;; be changed. However there is no fast way to get a correct undefined.
   (display "undefined" p))

(define-method (expr-out this::Null in-for-init? stmt-begin?
			 indent p compress?)
   (display "null" p))

(define-method (expr-out this::Bool in-for-init? stmt-begin?
			 indent p compress?)
   (with-access::Bool this (val)
      (if val
	  (display "true" p)
	  (display "false" p))))

(define-method (expr-out this::Number in-for-init? stmt-begin?
			 indent p compress?)
   (with-access::Number this (val)
      (display val p)))

(define-method (expr-out this::String in-for-init? stmt-begin?
			 indent p compress?)
   ;; the string must be a valid string (including the delimiters).
   (with-access::String this (val)
      (display val p)))

(define-method (expr-out this::Array in-for-init? stmt-begin?
			 indent p compress?)
   (with-access::Array this (els len)
      (display #\[ p)
      (let loop ((i 0)
		 (els els))
	 (unless (>= i len)
	    (unless (=fx i 0)
	       (display #\, p))
	    (if (or (null? els)
		    (not (=fx (Array-Element-index (car els)) i)))
		(loop (+ i 1) els)
		(begin
		   (unless (=fx i 0) (space-out p compress?))
		   (nested-expr-out (Array-Element-expr (car els))
				    assig-expr? #f #f
				    (indent++ indent) p compress?)
		   (loop (+ i 1) (cdr els))))))
      (display #\] p)))

(define-method (expr-out this::Obj-Init in-for-init? stmt-begin?
			 indent p compress?)
   (define (init-display init)
      (with-access::Property-Init init (name val)
	 (cond
	    ((and (String? name)
		  (valid-js-id? (String-val name) :strip-delimiters? #t))
	     (let ((str (String-val name)))
		(display (substring str 1 (-fx (string-length str) 1)) p)))
	    (else
	     (display (Literal-val name) p)))
	 (display ":" p)
	 (space-out p compress?)
	 (nested-expr-out val assig-expr? #f #f (indent++ indent) p compress?)))
      
   (with-access::Obj-Init this (inits)
      (display #\{ p)
      (unless (null? inits)
	 (init-display (car inits))
	 (for-each (lambda (init)
		      (display "," p)
		      (space-out p compress?)
		      (init-display init))
		   (cdr inits)))
      (display #\} p)))

(define-method (expr-out this::RegExp in-for-init? stmt-begin?
			 indent p compress?)
   (with-access::RegExp this (pattern)
      (display pattern p)))

(define-method (expr-out this::Pragma in-for-init? stmt-begin?
			 indent p compress?)
   (with-access::Pragma this (str)
      (display str p)))
