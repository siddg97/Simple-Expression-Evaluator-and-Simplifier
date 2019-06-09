;;; <---- START : myeval.scm  ----->

;;;+------------------------------------------------------------------------------------------------------------+
;;;| ASSIGNMENT 2 - CMPT 383 - SUMMER 2019 																		|
;;;| Instructor: Toby Donaldson																					|
;;;| Author: Siddharth Gupta | SFU ID: 301327469																|
;;;| Due-Date: June 5 2019																						|
;;;|																											|
;;;| IMPEMENTATION:																								|
;;;| 	+ Created several top-level checking functions to check a given expression 								|
;;;|	+ Called them recursively in other functions.															|
;;;| 	+ (myeval expr env) provides certain error messages for certain cases									|
;;;|																											| 
;;;| CITATIONS:																									|
;;;|	- Assignment 2 description 	[ http://www.sfu.ca/~tjd/383summer2019/a2.html ]							|
;;;|	- Course notes for Scheme on course home page [ http://www.sfu.ca/~tjd/383summer2019/index.html ]		|
;;;|																											|
;;;| NOTE:																										|
;;;| 	- I am loading my "env1.scm" file for using in (myeval expr env)										|
;;;|	- I am loading the module "helpful_stuff.scm" which has helpful functions for all assignment questions	|
;;;+------------------------------------------------------------------------------------------------------------+

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