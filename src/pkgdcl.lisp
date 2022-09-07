;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2020-2022 by Symbolics Pte. Ltd. All rights reserved.

(uiop:define-package #:special-functions
    (:use #:cl
	  #:alexandria+
	  #:let-plus
	  #:num-utils.polynomial	; polynomial evaluation
	  #:num-utils.arithmetic)
  (:nicknames #:spfn #:specfun)

  (:import-from #:float-features
		#:double-float-positive-infinity
		#:double-float-negative-infinity
		#:double-float-nan
		#:bits-double-float
		#:float-nan-p)

  (:export #:erf			; libm
	   #:erfc			;Return (- 1 (erf x))
	   #:inverse-erf		; Boost
	   #:inverse-erfc		; Boost

	   #:gamma			;Return true Gamma
	   #:incomplete-gamma		;Return values (p q) for normalised incomplete gamma
	   #:lower-incomplete-gamma	;Return the normalised lower incomplete gamma function P(a,x), a>=0, x>=0
	   #:upper-incomplete-gamma	;Return the normalised upper incomplete gamma function Q(a,x), a>=0, x>=0
	   #:regularised-gamma-prefix	;used in PDF

	   #:factorial			; Ramanujan
	   #:log-gamma))		; libm




