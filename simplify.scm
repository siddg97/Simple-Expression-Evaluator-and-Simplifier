;;; <---- START of simplify.scm ---->

;;;+------------------------------------------------------------------------------------------------------------------------------------+
;;;| IMPEMENTATION:															|
;;;| 	+ Created several top-level checking functions to check the given simplyfying cases						|
;;;|	+ Called them recursively in other functions.											|
;;;| 	+ (simplify expr) returns a simplified expression for expr 									|
;;;|																	|
;;;| NOTE:																|
;;;| 	I am loading the file "helpful_stuff.scm" to get helpful functions for checking top-level expressions and simplifying them	|
;;;+------------------------------------------------------------------------------------------------------------------------------------+

(load "helpful_stuff.scm")

;;; Returns a simplified expression - if possible else return expression as it is
(define simplify 
	(lambda (expr)
		(cond 
			((is-literal? expr)
				expr
			)
			((is-inc-op? expr) 
				(simplify-inc (simplify (car (cdr expr))))
			)
			((is-dec-op? expr) 
				(simplify-dec (simplify (car (cdr expr))))
			)
			((is-+-op? expr)
				(simplify-+ (simplify (car expr)) (simplify(car (cdr (cdr expr)))))
			)
			((is-*-op? expr)
				(simplify-* (simplify (car expr)) (simplify(car (cdr (cdr expr)))))
			)
			((is-minus-op? expr)
				(simplify-minus (simplify (car expr)) (simplify(car (cdr (cdr expr)))))
			)
			((is-/-op? expr)
				(simplify-/ (simplify (car expr)) (simplify(car (cdr (cdr expr)))))
			)
			((is-**-op? expr)
				(simplify-** (simplify (car expr)) (simplify(car (cdr (cdr expr)))))
			)
			(else
				(error "Invalid expression!!")
			)
		)	
	)
)
