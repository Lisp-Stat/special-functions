;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2020,2021 by Symbolics Pte. Ltd. All rights reserved.

(uiop:define-package #:special-functions
    (:use #:cl
	  #:num-utils.polynomial	; polynomial evaluation
	  #:num-utils.arithmetic)
  (:nicknames #:spfn #:specfun)

  (:import-from #:float-features
		#:double-float-positive-infinity
		#:double-float-negative-infinity
		#:double-float-nan
		#:bits-double-float	;for SBCL workaround of double-float-nan
		#:float-nan-p)

  (:export #:erf			; libm
	   #:erfc			; libm
	   #:inverse-erf		; Boost
	   #:inverse-erfc		; Boost
	   #:gamma			; Cephes
	   #:factorial			; Ramanujan
	   #:log-gamma))		; libm




