;;; <---- START of simplify.scm ---->

;;;+------------------------------------------------------------------------------------------------------------------------------------+
;;;| ASSIGNMENT 2 - CMPT 383 - SUMMER 2019 																								|
;;;| Instructor: Toby Donaldson																											|
;;;| Author: Siddharth Gupta | SFU ID: 301327469																						|
;;;| Due-Date: June 5 2019																												|
;;;|																																	|
;;;| IMPEMENTATION:																														|
;;;| 	+ Created several top-level checking functions to check the given simplyfying cases												|
;;;|	+ Called them recursively in other functions.																					|
;;;| 	+ (simplify expr) returns a simplified expression for expr 																		|
;;;|																																	| 
;;;| CITATIONS:																															|
;;;|	- Assignment 2 description 	[ http://www.sfu.ca/~tjd/383summer2019/a2.html ]													|
;;;|	- Course notes for Scheme on course home page [ http://www.sfu.ca/~tjd/383summer2019/index.html ]								|
;;;|																																	|
;;;| NOTE:																																|
;;;| 	I am loading the file "helpful_stuff.scm" to get helpful functions for checking top-level expressions and simplifying them		|
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