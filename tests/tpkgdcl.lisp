;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2020 by Symbolics Pte. Ltd. All rights reserved.

(uiop:define-package #:special-functions-tests
  (:use #:cl
	#:fiveam
	#:special-functions
	#:num-utils.num=
	#:num-utils.test-utilities
	#:select
	#:cl-variates)

  (:export #:run!
	   #:all-tests))
