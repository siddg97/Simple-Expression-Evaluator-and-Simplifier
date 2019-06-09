;;; <---- START of helfulADT2.scm ---->

;;;+------------------------------------------------------------------------------------------------------------+
;;;| ASSIGNMENT 2 - CMPT 383 - SUMMER 2019 																		|
;;;| Instructor: Toby Donaldson																					|
;;;| Author: Siddharth Gupta | SFU ID: 301327469																|
;;;| Due-Date: June 5 2019																						|
;;;|																											|
;;;| IMPEMENTATION:																								|
;;;| 	+ Created several helpful functions for the ADT implementation in "env2.scm"							|
;;;|	+ Called them recursively in other functions.															|
;;;|																											| 
;;;| CITATIONS:																									|
;;;|	- Assignment 2 description 	[ http://www.sfu.ca/~tjd/383summer2019/a2.html ]							|
;;;|	- Course notes for Scheme on course home page [ http://www.sfu.ca/~tjd/383summer2019/index.html ]		|
;;;|																											|
;;;| NOTE:																										|
;;;| 	- This file will be used as a module in (Q1) "env2.scm"													| 
;;;+------------------------------------------------------------------------------------------------------------+

;;; <---- Helpful functions for ADT2 ---->

;;; combine 2 enviornments
(define combine
	(lambda (e1 e2)
		(append (list (append (car e1) (car e2))) (list (append (car (cdr e1)) (car (cdr e2)))))		
	)
)

;;; return boolen for an empty enviornment
(define empty-env?
	(lambda (env)
		(cond 
			((and (null? (car env)) (null? (car (cdr env))))
				#t
			)
			(else 
				#f
			)
		)
	)
)

;;; car like function for env
(define car-env
	(lambda (env)
		(cond 
			((empty-env? env)
				'(() ())
			)
			(else
				(append
					(list (list (car (car env))))
					(list (list (car (car (cdr env)))))
				)
			)
		)
	)
)

;;;  cdr like function for env
(define cdr-env
	(lambda (env)
		(cond 
			((empty-env? env)
				'(() ())
			)
			(else
				(append
					(list (cdr (car env)))
					(list (cdr (car (cdr env))))
				)
			)
		)
	)
)


;;; boolean for given variable in an anviornment
(define in-env?
	(lambda (v env)
		(cond
			((and
				(null? (car env))
				(null? (car (cdr env))))
				#f
			)
			((equal? v (car (car env)))
				#t
			)
			(else
				(in-env? v (list (cdr (car env)) (cdr (car (cdr env)))))
			)
		)
	)
)

;;; update the given enviornent with the current value for the given varibale. if v is not found in the env then does nothing
(define update-env 
	(lambda (v val env)
		(cond
			((empty-env? env)
				'(() ())
			)
			((equal? v (car (car env)))
				(combine
					(append
						(list (list v))
						(list (list val))
					)
					(update-env v val (cdr-env env))
				)
			)
			(else
				(combine 
					(car-env env)
					(update-env v val (cdr-env env))
				)
			)
		)
	)
)

;;; <---- End of Helpful ADT2 functions ---->
