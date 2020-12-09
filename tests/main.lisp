;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: SPECIAL-FUNCTIONS-TESTS -*-
;;; Copyright (c) 2020 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:special-functions-tests)

(def-suite all-tests
    :description "The master suite of all special functions tests.")
(in-suite all-tests)

;;; Fiveam get-float does not allow for ranges.
(defun gen-double (generator min max)
  "Return a generator producing doubles between min and max, inclusive"
  (lambda ()
    (random-range-inclusive generator min max)))

(defun gen-positive-double (generator min max)
  "Return a generator producing doubles between min and max, inclusive. If min or max is negative, return small value"
  (lambda ()
    (let ((min (if (plusp min) min 2.848094538889218D-306)))
      (random-range-inclusive generator min max))))

(defparameter *report-epsilon* t "Print key statistics in terms of machine epsilon")

(defun print-test-summary (result &key (report-epsilon *report-epsilon*))
  "Print summary of results.
Include some values in epsilon if report-epsilon is true. This is useful when comparing to other implementations"
  (write result)
  (when report-epsilon
    (format t "~%   Key stats in terms of epsilon:~%     Max error = ~,2Eε, mean error = ~,2Eε~%"
	    (/ (max-error  result) double-float-epsilon)
	    (/ (mean-error result) double-float-epsilon))))

(defun eps (x)
  "Return a multiple of epsilon"
  (* x double-float-epsilon))

(defun test-specfun ()
  (run! 'all-tests))
