;;; <---- START : env1.scm ---->


;;;+--------------------------------------------------------------------------------------------------------+
;;;| ASSIGNMENT 2 - CMPT 383 - SUMMER 2019 																	|
;;;| Instructor: Toby Donaldson																				|
;;;| Author: Siddharth Gupta | SFU ID: 301327469															|
;;;| Due-Date: June 5 2019																					|
;;;|																										|
;;;| IMPEMENTATION:																							|
;;;| 	- Simple enviorment using list of 2-lists ADT														|
;;;|	- Each enviornment variable is a 2 element list 													|
;;;| 	- First elemnent of the list is name of var 														|
;;;|	- Second element of the list is value of var 														|
;;;|																										|
;;;| CITATIONS:																								|
;;;|	- Assignment 2 description [ http://www.sfu.ca/~tjd/383summer2019/a2.html ]							|
;;;|	- Course notes for Scheme on course home page [ http://www.sfu.ca/~tjd/383summer2019/index.html ]	|
;;;|																										|
;;;| NOTES:																									|
;;;| 	+ loading the file "helpfulADT1.scm" to acces helpful functions for ADT described above.			|
;;;+--------------------------------------------------------------------------------------------------------+


(load "helpfulADT1.scm")


;;; <---- Start of (make-empty-env): Returns an empty list since the enviorment is empty
(define make-empty-env
	(lambda ()
		'()
	)
)
;;; End of (make-empty env) ---->

;;; <---- Start of (apply-env env v): Returns the value of v if in the given enviorment [env] else returns an error message
(define apply-env 
	(lambda (env v)
		(cond 
			((null? env)	; If env is empty then no variablles exist for the name v
				(error "apply-env: empty enviorment")
			)
			((equal? (car (car env)) v)	; First element of the first list is v [matching var found in env]
				(car (cdr (car env))) ; Second element of that list is value of v
			)
			(else		; Else look for v in the rest of the env list
				(apply-env (cdr env) v) 
			)
		)
	)
)
;;; End of (apply-env env v) ---->

;;; <---- Start of (extend-env v val env): Returns a new enviornment that has env (list of lists) appended with the list (v val). If v already has a value then replace it with val
(define extend-env
	(lambda (v val env)
		(cond 
			((in-env? v env)
				(update-env v val env)
			)
			(else  ; Add to env the 2 element list (v val)
				(append env (list (list v val)))
			)
		)
	)
)
;;; End of (extend-env v val env)
