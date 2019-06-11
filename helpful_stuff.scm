;;; <---- START of helful_stuff.scm ---->

;;;+--------------------------------------------------------------------------------------------+
;;;| IMPEMENTATION:										|
;;;| 	+ Created several top-level checking functions to check a given expression 		|
;;;|	+ Called them recursively in other functions.						|
;;;|												|
;;;| NOTE:											|
;;;| 	- This file will be used as a module in (Q2) "myeval.scm" and (Q3) "simplify.scm"	| 
;;;+--------------------------------------------------------------------------------------------+

;;; <--- Top-level format checking functions for expressions --->

;;; Boolean for e being a number
(define is-number?
	(lambda (e)
		(number? e)
	)
)

;;; Boolean for e being a var
(define is-symbol?
	(lambda (e)
		(symbol? e)
	)
)

;;; Boolean for e being a varible
(define is-var?
	(lambda (e)
		(and (is-symbol? e) (not (is-operator? e)))
	)
)

;;; Boolean for e being an operator
(define is-operator?
	(lambda (e)
		(or 
			(equal? '+ e)
			(equal? '- e)
			(equal? '* e)
			(equal? '/ e)
			(equal? '** e)
			(equal? 'inc e)
			(equal? 'dec e)
		)
	)
)

;;; Boolean to check if an expresion is a literal value
(define is-literal?
	(lambda (expr)
		(or (is-number? expr) (is-symbol? expr))
	)
)

;;; check if the expression is a list of n length
(define list-of-n?
	(lambda (n expr)
		(and 
			(list? expr)
			(= n (length expr))
		)
	)
)

;;; boolean for expression being a plus operation
(define is-+-op?
	(lambda (expr)
		(and 
			(list-of-n? 3 expr)
			(and
				(not (is-operator? (car expr)))
				(equal? '+ (car (cdr expr)))
				(not (is-operator? (car (cdr (cdr expr)))))
			)
		)
	)
)

;;; boolean for expression being a minus operation
(define is-minus-op?
	(lambda (expr)
		(and 
			(list-of-n? 3 expr)
			(and
				(not (is-operator? (car expr)))
				(equal? '- (car (cdr expr)))
				(not (is-operator? (car (cdr (cdr expr)))))
			)
		)
	)
)

;;; boolean for expression being a multiplication operation
(define is-*-op?
	(lambda (expr)
		(and 
			(list-of-n? 3 expr)
			(and
				(not (is-operator? (car expr)))
				(equal? '* (car (cdr expr)))
				(not (is-operator? (car (cdr (cdr expr)))))
			)
		)
	)
)

;;; boolean for expression being a divide operation
(define is-/-op?
	(lambda (expr)
		(and 
			(list-of-n? 3 expr)
			(and
				(not (is-operator? (car expr)))
				(equal? '/ (car (cdr expr)))
				(not (is-operator? (car (cdr (cdr expr)))))
			)
		)
	)
)

;;; boolean for expression being an exponent operation
(define is-**-op?
	(lambda (expr)
		(and 
			(list-of-n? 3 expr)
			(and
				(not (is-operator? (car expr)))
				(equal? '** (car (cdr expr)))
				(not (is-operator? (car (cdr (cdr expr)))))
			)
		)
	)
)

;;; boolean for expression being a increment operation
(define is-inc-op?
	(lambda (expr)
		(and 
			(list-of-n? 2 expr)
			(and
				(equal? 'inc (car expr))
				(not (is-operator? (car (cdr expr))))
			)
		)
	)
)

;;; boolean for expression being a decrement operation
(define is-dec-op?
	(lambda (expr)
		(and 
			(list-of-n? 2 expr)
			(and
				(equal? 'dec (car expr))
				(not (is-operator? (car (cdr expr))))
			)
		)
	)
)

;;; boolean for same variables/literals used in the expr: Eg. (a + a) or ( 2 - 2) etc...
(define equal-vars?
	(lambda (expr)
		(equal? (car expr) (car (cdr (cdr expr))))
	)
)

;;; boolean for expression according to the ENBF syntax in assignment description
(define is-expr? 
	(lambda (expr)
		(cond 
			((is-literal? expr)
				(cond 
					((is-operator? expr)
						#f
					)
					(else 
						#t
					)
				)
			)
			((is-+-op? expr)
				(and 
					(is-expr? (car expr))
					(is-expr? (car (cdr (cdr expr))))
				)
			)
			((is-minus-op? expr)
				(and 
					(is-expr? (car expr))
					(is-expr? (car (cdr (cdr expr))))
				)
			)
			((is-*-op? expr)
				(and 
					(is-expr? (car expr))
					(is-expr? (car (cdr (cdr expr))))
				)
			)
			((is-/-op? expr)
				(and 
					(is-expr? (car expr))
					(is-expr? (car (cdr (cdr expr))))
				)
			)
			((is-**-op? expr)
				(and 
					(is-expr? (car expr))
					(is-expr? (car (cdr (cdr expr))))
				)
			)
			((is-inc-op? expr)
				(is-expr? (car (cdr expr)))
			)
			((is-dec-op? expr)
				(is-expr? (car (cdr expr)))
			)
			(else
				#f
			)
		)
	)
)

;;; returns a list of variables in the expr
(define get-vars
	(lambda (expr)
		(cond 
			((and (is-symbol? expr) (not (is-operator? expr)))
				(list expr)
			)
			((list-of-n? 2 expr)
				(get-vars (car (cdr expr)))
			)
			((list-of-n? 3 expr)
				(append
					(get-vars (car expr))
					(get-vars (car (cdr (cdr expr))))
				)
			)
			(else
				'()
			)
		)
	)
)


;;; boolean if the the variables in the given list of variables are in the given enviornment.
(define vars-in-env?
	(lambda (lst_of_vars env)
		(cond 
			((null? lst_of_vars)
				#t
			)
			(else
				(and 
					(in-env? (car lst_of_vars) env) 
					(vars-in-env? (cdr lst_of_vars) env)
				)
			)
		)
	)
)

;;; boolean for the given expression is fit for evaluation w.r.t. env
(define eval-ok?
	(lambda (expr env)
		(cond 
			((is-expr? expr)
				(cond 
					((vars-in-env? (get-vars expr) env)
						#t
					)
					(else
						(error "Unknown variable found in expression!")
					)
				)
			)
			(else
				#f
			)
		)
	)
)

;;; boolean for unary operaton
(define unary?
	(lambda (expr)
		(list-of-n? 2 expr)
	)
)

;;; boolen for binary operation
(define binary?
	(lambda (expr)
		(list-of-n? 3 expr)
	)
)

;;; evaluate expression recursively to return the result
(define eval-expr
	(lambda (expr env)
		(cond
			((is-literal? expr)
				(cond 
					((is-var? expr)
						(apply-env env expr)
					)
					(else 
						expr
					)
				)
			)
			((unary? expr)
				(cond
					((is-inc-op? expr)
						(+ (eval-expr (car (cdr expr)) env) 1)
					)
					(else
						(- (eval-expr (car (cdr expr)) env) 1)
					)
				)
			)
			(else
				(cond
					((is-+-op? expr)
						(+ (eval-expr (car expr) env) (eval-expr (car (cdr (cdr expr))) env))
					)
					((is-minus-op? expr)
						(- (eval-expr (car expr) env) (eval-expr (car (cdr (cdr expr))) env))
					)
					((is-*-op? expr)
						(* (eval-expr (car expr) env) (eval-expr (car (cdr (cdr expr))) env))
					)
					((is-/-op? expr)
						(cond
							((zero? (eval-expr (car (cdr (cdr expr))) env))
								(error "divison by 0 not allowed!!!")
							)
							(else 
								(/ (eval-expr (car expr) env) (eval-expr (car (cdr (cdr expr))) env))
							)
						)
					)
					((is-**-op? expr)
						(expt (eval-expr (car expr) env) (eval-expr (car (cdr (cdr expr))) env))
					)
				)
			)
		)
	)
)

;;; simplify a + operation
(define simplify-+
	(lambda (lexpr rexpr)
		(cond 
            ((equal? 0 lexpr)                                  
                rexpr)
            ((equal? 0 rexpr)                          
                lexpr)
            (else
                (list lexpr '+ rexpr))
        )
	)
)

;;; simplify a * operation
(define simplify-*
    (lambda (lexpr rexpr)
        (cond 
            ((equal? 1 lexpr)                               
                rexpr)
            ((equal? 1 rexpr)        
                lexpr)
            ((or (equal? lexpr 0) (equal? rexpr 0))
                0)
            (else
                (list lexpr '* rexpr))
        )
    )
)

;;; simplify a / operation
(define simplify-/
    (lambda (lexpr rexpr)
        (cond 
            ((equal? 1 rexpr)
                lexpr)
            (else
                (list lexpr '/ rexpr))
        )
    )
)

;;; simplify a - operation
(define simplify-minus
    (lambda (lexpr rexpr)
        (cond
            ((equal? lexpr rexpr)
                0)
            ((equal? 0 rexpr)
                lexpr)
            (else
                (list lexpr '- rexpr))
        )
    )
)

;;; simplify a ** operation
(define simplify-**
    (lambda (lexpr rexpr)
        (cond
            ((equal? 0 rexpr)
                1)
            ((equal? 1 rexpr)
                lexpr)
            ((equal? 1 lexpr)
                1)
            (else
                (list lexpr '** rexpr))
        )
    )
)

;;; simplify an inc operation
(define simplify-inc
    (lambda (rexpr)
        (cond
            ((number? rexpr)
                (+ rexpr 1))
            (else
                (list 'inc rexpr))
        )
    )
)

;;; simplify a dec operation
(define simplify-dec
    (lambda (rexpr)
        (cond
            ((number? rexpr)
                (- rexpr 1))
            (else
                (list 'dec rexpr))
        )
    )
)
