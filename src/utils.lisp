;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: SPECFUN -*-
;;; Copyright (c) 2019-2021 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:special-functions)

;;; Some utilities/wrappers specific to special-functions

;; Wrap FLOAT-FEATURES functions with more descriptive names
(defun decode-float64 (x)
  "Convert the (unsigned-byte 64) bit representation into a native double-float"
  (float-features:bits-double-float x))

(defun encode-float64 (x)
  "Returns the bit representation of the double-float X as an (unsigned-byte 64)"
  (float-features:double-float-bits x))


;; Mathmatical constructs
;; More accurate than calculating and useful for long-double
(defconstant +square-root-2-pi+ 2.5066282746310005024d0)

(defun sin-pi (x)
  "Returns (sin (* pi x))"
  (declare (double-float x))
  (sin (* cl:pi x)))
