;;; <---- START of helfulADT1.scm ---->

;;;+------------------------------------------------------------------------------------+
;;;| IMPEMENTATION:									|
;;;| 	+ Created several helpful functions for the ADT implementation in "env1.scm"	|
;;;|	+ Called them recursively in other functions.					|
;;;|											| 
;;;| NOTE:										|
;;;| 	- This file will be used as a module in "env1.scm"				| 
;;;+------------------------------------------------------------------------------------+

;;; <---- Helpful ADT1 functions ---->

;;; Returns boolean for v in env
(define in-env?
	(lambda (v env)
		(cond
			((null? env)	; If env is empty then v doesnt exist in env
				#f
			)
			((equal? v (car (car env))) ; If first element of the first list in env matches with v then v is found
				#t
			)
			(else 		; Recursive step to find if v is in enviornment env
				(in-env? v (cdr env))
			)
		)
	)
)

;;; Update the variable v with value val in env
(define update-env
	(lambda (v val env)
		(cond 
			((null? env)
				'()
			)
			((equal? v (car (car env)))
				(append (cdr env) (list (list v val)))
			)
			(else
				(append 
					(update-env v val (cdr env)) 
					(list (list (car (car env)) (car (cdr (car env)))))
				)
			)
		)
	)
)

;;; cdr like function for env
(define cdr-env
	(lambda (env)
		(cdr env)
	)
)

;;; car like function for env
(define car-env
	(lambda (env)
		(list (car env))
	)
)
;;; <---- End of Helpful ADT1 functions ---->
