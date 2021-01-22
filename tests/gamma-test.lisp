;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: SPECIAL-FUNCTIONS-TESTS -*-
;;; Copyright (c) 2020 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:special-functions-tests)

(def-suite gamma
  :description "Tests for the gamma and related functions"
  :in all-tests)
(in-suite gamma)
#|
(test gamma-factorial
  :description "Test complete gamma at integer and half integer values"
  (let ((result (test-fn (select factorials t 1)
			 #'(lambda (i params)
			     (specfun:gamma (aref (car params) i)))
			 (select factorials t 0))))
    (is (> 1D-7 (max-error  result)) "Maximum error exceeds 1.0D-7 threshold")
    (is (> 1D-8 (mean-error result)) "Mean error exceeds 1.0D-8 threshold")
    (is (> 1D-7 (rms result))        "RMS error exceeds 1.0D-7 threshold")
    (print-test-summary result :report-epsilon t)))

(test gamma-near-0
  :description "Test complete gamma near 0"
  (let ((result (test-fn (select near-0 t 1)
			 #'(lambda (i params)
			     (specfun:gamma (aref (car params) i)))
			 (select near-0 t 0))))
    (is (> 1D-7 (max-error  result)) "Maximum error exceeds 1.0D-7 threshold")
    (is (> 1D-8 (mean-error result)) "Mean error exceeds 1.0D-8 threshold")
    (is (> 1D-7 (rms result))        "RMS error exceeds 1.0D-7 threshold")
    (print-test-summary result :report-epsilon t)))

(test gamma-near-1
  :description "Test complete gamma near 1"
  (let ((result (test-fn (select near-1 t 1)
			 #'(lambda (i params)
			     (specfun:gamma (aref (car params) i)))
			 (select near-1 t 0))))
    (is (> 1D-7 (max-error  result)) "Maximum error exceeds 1.0D-7 threshold")
    (is (> 1D-8 (mean-error result)) "Mean error exceeds 1.0D-8 threshold")
    (is (> 1D-7 (rms result))        "RMS error exceeds 1.0D-7 threshold")
    (print-test-summary result :report-epsilon t)))

(test gamma-near-2
  :description "Test complete gamma near 2"
  (let ((result (test-fn (select near-2 t 1)
			 #'(lambda (i params)
			     (specfun:gamma (aref (car params) i)))
			 (select near-2 t 0))))
    (is (> 1D-7 (max-error  result)) "Maximum error exceeds 1.0D-7 threshold")
    (is (> 1D-8 (mean-error result)) "Mean error exceeds 1.0D-8 threshold")
    (is (> 1D-7 (rms result))        "RMS error exceeds 1.0D-7 threshold")
    (print-test-summary result :report-epsilon t)))

(test gamma-near-minus-10
  :description "Test complete gamma near -10"
  (let ((result (test-fn (select near-minus-10 t 1)
			 #'(lambda (i params)
			     (specfun:gamma (aref (car params) i)))
			 (select near-minus-10 t 0))))
    (is (> 1D-7 (max-error  result)) "Maximum error exceeds 1.0D-7 threshold")
    (is (> 1D-8 (mean-error result)) "Mean error exceeds 1.0D-8 threshold")
    (is (> 1D-7 (rms result))        "RMS error exceeds 1.0D-7 threshold")
    (print-test-summary result :report-epsilon t)))

(test gamma-near-minus-55
  :description "Test complete gamma near -55"
  #+nil
  (signals floating-point-overflow (test-fn (select near-minus-55 t 1)
			 #'(lambda (i params)
			     (specfun:gamma (aref (car params) i)))
			 (select near-minus-55 t 0)))
  ;; (write "  Should overflow near 170")
  (let ((result (test-fn (select near-minus-55 t 1)
			 #'(lambda (i params)
			     (specfun:gamma (aref (car params) i)))
			 (select near-minus-55 t 0))))
    (is (> 1D-7 (max-error  result)) "Maximum error exceeds 1.0D-7 threshold")
    (is (> 1D-8 (mean-error result)) "Mean error exceeds 1.0D-8 threshold")
    (is (> 1D-7 (rms result))        "RMS error exceeds 1.0D-7 threshold")
    (print-test-summary result :report-epsilon t)))
|#

(def-suite log-gamma
  :description "Tests for log-gamma"
  :in gamma)
(in-suite log-gamma)

(test log-gamma-factorial
  :description "Test log-gamma at integer and half integer values"
  (let ((result (test-fn (select factorials t 2) ; expected log-gamma value
			 #'(lambda (i params)
			     (specfun:log-gamma (aref (car params) i)))
			 (select factorials t 0)))) ; input value column
    (is (> (eps 2.11d0)  (max-error  result)) "Maximum error exceeds 2.1ε threshold")
    (is (> (eps 0.569) (mean-error result)) "Mean error exceeds 0.569ε threshold")
    (print-test-summary result :report-epsilon *report-epsilon*)))

(test log-gamma-near-0
  :description "Test log-gamma near 0"
  (let ((result (test-fn (select near-0 t 2)
			 #'(lambda (i params)
			     (specfun:log-gamma (aref (car params) i)))
			 (select near-0 t 0))))
    (is (> (eps 1.95)  (max-error  result)) "Maximum error exceeds 1.95ε threshold")
    (is (> (eps 0.665) (mean-error result)) "Mean error exceeds 0.665ε threshold")
    (print-test-summary result :report-epsilon *report-epsilon*)))

(test log-gamma-near-1
  :description "Test log-gamma near 1"
  (let ((result (test-fn (select near-1 t 2)
			 #'(lambda (i params)
			     (specfun:log-gamma (aref (car params) i)))
			 (select near-1 t 0))))
    (is (> (eps 0.5)  (max-error  result)) "Maximum error exceeds 0.5ε threshold")
    (is (> (eps 0.0185) (mean-error result)) "Mean error exceeds 0.0185ε threshold")
    (print-test-summary result :report-epsilon *report-epsilon*)))

(test log-gamma-near-2
  :description "Test log-gamma near 2"
  (let ((result (test-fn (select near-2 t 2)
			 #'(lambda (i params)
			     (specfun:log-gamma (aref (car params) i)))
			 (select near-2 t 0))))
    (is (> (eps 0.0157)  (max-error  result)) "Maximum error exceeds 0.0157ε threshold")
    (is (> (eps 3.84d-4) (mean-error result)) "Mean error exceeds 3.84d-4ε threshold")
    (print-test-summary result :report-epsilon *report-epsilon*)))

(test log-gamma-near-minus-10
  :description "Test log-gamma near -10"
  (let ((result (test-fn (select near-minus-10 t 2)
			 #'(lambda (i params)
			     (specfun:log-gamma (aref (car params) i)))
			 (select near-minus-10 t 0))))
    (is (> (eps 4.83d+5)  (max-error  result)) "Maximum error exceeds 4.83d+5ε threshold")
    (is (> (eps 3.07d+4) (mean-error result)) "Mean error exceeds 3.06d+4ε threshold")
    (print-test-summary result :report-epsilon *report-epsilon*)))

(test log-gamma-near-55
  :description "Test log-gamma near -55"
  (let ((result (test-fn (select near-minus-55 t 2)
			 #'(lambda (i params)
			     (specfun:log-gamma (aref (car params) i)))
			 (select near-minus-55 t 0))))
    (is (> (eps 8.17d+4)  (max-error  result)) "Maximum error exceeds 8.16d+4ε threshold")
    (is (> (eps 4.54d+3) (mean-error result)) "Mean error exceeds 4.53d+3ε threshold")
    (print-test-summary result :report-epsilon *report-epsilon*)))
