;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: SPECIAL-FUNCTIONS-TESTS -*-
;;; Copyright (c) 2020-2022 by Symbolics Pte. Ltd. All rights reserved.
(in-package :special-functions-tests)

(def-suite incomplete-gamma
  :description "Tests for the incomplete-gamma functions"
  :in all-tests)
(in-suite incomplete-gamma)

#| See:
https://github.com/boostorg/math/blob/c4972b02143b787387560f87238e50f55502834e/test/test_igamma.cpp
https://scm.cs.kuleuven.be/scm/svn/numerics_software/boost_1_59_0/libs/math/performance/test_igamma.cpp

Below is from Boost. Need to modify for CLS tests.

This file tests the incomplete gamma functions tgamma,
tgamma_lower, gamma_p and gamma_q. There are two sets of tests, spot
tests which compare our results with selected values computed
using the online special function calculator at
functions.wolfram.com, while the bulk of the accuracy tests
use values generated with NTL::RR at 1000-bit precision
and our versions of these functions.

Note that when this file is first run on a new platform many of
these tests will fail: the default accuracy is 1 epsilon which
is too tight for most platforms.  In this situation you will
need to cast a human eye over the error rates reported and make
a judgement as to whether they are acceptable.  Either way please
report the results to the CLS mailing list.  Acceptable rates of
error are marked up below as a series of regular expressions that
identify the compiler/stdlib/platform/data-type/test-data/test-function
along with the maximum expected peak and RMS mean errors for that
test. |#

(test incomplete-gamma-small-p
  :description "Test lower incomplete gamma, P, with small data"
  (let ((result (test-fn (select incomplete-gamma-small-data t 5)
			 #'(lambda (i params)
			     (specfun:incomplete-gamma (aref (first params) i) (aref (second params) i)))
			 (select incomplete-gamma-small-data t 0) (select incomplete-gamma-small-data t 1))))

    (is (> 1D-15 (max-error  result)) "Maximum error exceeds 1.0D-15 threshold")
    (is (> 1D-16 (mean-error result)) "Mean error exceeds 1.0D-16 threshold")
    (is (> 1D-16 (rms result))        "RMS error exceeds 1.0D-16 threshold")
    (print-test-summary result :report-epsilon t)))

(test incomplete-gamma-small-q
  :description "Test upper incomplete gamma, Q, with small data"
  (let ((result (test-fn (select incomplete-gamma-small-data t 3)
			 #'(lambda (i params)
			     (specfun:upper-incomplete-gamma (aref (first params) i) (aref (second params) i)))
			 (select incomplete-gamma-small-data t 0) (select incomplete-gamma-small-data t 1))))

    (is (> 1D-15 (max-error  result)) "Maximum error exceeds 1.0D-15 threshold")
    (is (> 1D-16 (mean-error result)) "Mean error exceeds 1.0D-16 threshold")
    (is (> 1D-16 (rms result))        "RMS error exceeds 1.0D-16 threshold")
    (print-test-summary result :report-epsilon t)))

(test incomplete-gamma-medium-p
  :description "Test lower incomplete gamma, P, with medium data"
  (let ((result (test-fn (select incomplete-gamma-medium-data t 5)
			 #'(lambda (i params)
			     (specfun:incomplete-gamma (aref (first params) i) (aref (second params) i)))
			 (select incomplete-gamma-medium-data t 0) (select incomplete-gamma-medium-data t 1))))

    (is (> 1D-14 (max-error  result)) "Maximum error exceeds 1.0D-15 threshold")
    (is (> 1D-16 (mean-error result)) "Mean error exceeds 1.0D-16 threshold")
    (is (> 1D-15 (rms result))        "RMS error exceeds 1.0D-16 threshold")
    (print-test-summary result :report-epsilon t)))

(test incomplete-gamma-medium-q
  :description "Test upper incomplete gamma, Q, with medium data"
  (let ((result (test-fn (select incomplete-gamma-medium-data t 3)
			 #'(lambda (i params)
			     (specfun:upper-incomplete-gamma (aref (first params) i) (aref (second params) i)))
			 (select incomplete-gamma-medium-data t 0) (select incomplete-gamma-medium-data t 1))))

    (is (> 1D-15 (max-error  result)) "Maximum error exceeds 1.0D-15 threshold")
    (is (> 1D-16 (mean-error result)) "Mean error exceeds 1.0D-16 threshold")
    (is (> 1D-16 (rms result))        "RMS error exceeds 1.0D-16 threshold")
    (print-test-summary result :report-epsilon t)))

(test incomplete-gamma-large-p
  :description "Test lower incomplete gamma, P, with large data"
  (let ((result (test-fn (select incomplete-gamma-large-data t 5)
			 #'(lambda (i params)
			     (specfun:incomplete-gamma (aref (first params) i) (aref (second params) i)))
			 (select incomplete-gamma-large-data t 0) (select incomplete-gamma-large-data t 1))))

    (is (> 1D-14 (max-error  result)) "Maximum error exceeds 1.0D-14 threshold")
    (is (> 1D-16 (mean-error result)) "Mean error exceeds 1.0D-16 threshold")
    (is (> 1D-15 (rms result))        "RMS error exceeds 1.0D-15 threshold")
    (print-test-summary result :report-epsilon t)))

(test incomplete-gamma-large-q
  :description "Test upper incomplete gamma, Q, with large data"
  (let ((result (test-fn (select incomplete-gamma-large-data t 3)
			 #'(lambda (i params)
			     (specfun:upper-incomplete-gamma (aref (first params) i) (aref (second params) i)))
			 (select incomplete-gamma-large-data t 0) (select incomplete-gamma-large-data t 1))))

    (is (> 1D-14 (max-error  result)) "Maximum error exceeds 1.0D-14 threshold")
    (is (> 1D-16 (mean-error result)) "Mean error exceeds 1.0D-16 threshold")
    (is (> 1D-15 (rms result))        "RMS error exceeds 1.0D-15 threshold")
    (print-test-summary result :report-epsilon t)))

(test incomplete-gamma-integer-p
  :description "Test lower incomplete gamma, P, with integer data"
  (let ((result (test-fn (select incomplete-gamma-integer-data t 5)
			 #'(lambda (i params)
			     (specfun:incomplete-gamma (aref (first params) i) (aref (second params) i)))
			 (select incomplete-gamma-integer-data t 0) (select incomplete-gamma-integer-data t 1))))

    (is (> 1D-15 (max-error  result)) "Maximum error exceeds 1.0D-16 threshold")
    (is (> 1D-16 (mean-error result)) "Mean error exceeds 1.0D-16 threshold")
    (is (> 1D-16 (rms result))        "RMS error exceeds 1.0D-16 threshold")
    (print-test-summary result :report-epsilon t)))

(test incomplete-gamma-integer-q
  :description "Test upper incomplete gamma, Q, with integer data"
  (let ((result (test-fn (select incomplete-gamma-integer-data t 3)
			 #'(lambda (i params)
			     (specfun:upper-incomplete-gamma (aref (first params) i) (aref (second params) i)))
			 (select incomplete-gamma-integer-data t 0) (select incomplete-gamma-integer-data t 1))))

    (is (> 1D-15 (max-error  result)) "Maximum error exceeds 1.0D-15 threshold")
    (is (> 1D-16 (mean-error result)) "Mean error exceeds 1.0D-16 threshold")
    (is (> 1D-16 (rms result))        "RMS error exceeds 1.0D-16 threshold")
    (print-test-summary result :report-epsilon t)))

