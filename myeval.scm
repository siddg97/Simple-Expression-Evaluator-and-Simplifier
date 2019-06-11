;;; <---- START : myeval.scm  ----->

;;;+------------------------------------------------------------------------------------+
;;;| IMPEMENTATION:									|
;;;| 	+ Created several top-level checking functions to check a given expression 	|
;;;|	+ Called them recursively in other functions.					|
;;;| 	+ (myeval expr env) provides certain error messages for certain cases		|
;;;|											|
;;;| NOTE:										|
;;;| 	- I am loading my "env1.scm" file for using in (myeval expr env)		|
;;;|	- I am loading the module "helpful_stuff.scm" which has helpful functions 	|
;;;+------------------------------------------------------------------------------------+

(load "env1.scm")
(load "helpful_stuff.scm")


;;; myeval function: evaluates given expression for given enviornment
(define myeval
	(lambda (expr env)
		(cond 
			((eval-ok? expr env)
				(eval-expr expr env)
			)
			(else
				(error "[ERROR] myeval expression has wrong syntax")
			)
		)
	)
)
