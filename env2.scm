;;; <---- START : env2.scm  ---->
		|
;;;+------------------------------------------------------------------------------------------------------------+
;;;| IMPEMENTATION:												|
;;;| 	- Simple enviorment using dictionaries ADT using 2 concurrent lists					|
;;;|	- Each enviornment variable is a an element in the first list at index i				|
;;;| 	- The corresponding element at index i in the second list is the values of the enviornment variable 	|
;;;|														|
;;;| NOTES:													|
;;;| 	+ loading the file "helpfulADT2.scm" to acces helpful functions for ADT described above.		|
;;;+------------------------------------------------------------------------------------------------------------+

(load "helpfulADT2.scm")

;;; <---- Start of (make-empty-env): Returns a list with 2 empty lists
(define make-empty-env
	(lambda ()
		(list '() '())
	)
)
;;; End of (make-empty env) ---->

;;; <---- Start of (apply-env env v): Returns the value of v if in the given enviorment [env] else returns an error message
(define apply-env 
	(lambda (env v)
		(cond 
			((empty-env? env)	; If env is empty then no variablles exist for the name v
				(error "apply-env: empty enviorment")
			)
			((equal? (car (car env)) v)	; First element of the first list is v [matching var found in env]
				(car (car (cdr env))) ; First element of the second list in env
			)
			(else		; Else look for v in the rest of the env list
				(apply-env (list (cdr (car env)) (cdr (car (cdr env)))) v) 
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
			(else 
				(append 
					(list (append (car env) (list v))) 
					(list (append (car (cdr env)) (list val)))
				)
			)
		)
	)
)
