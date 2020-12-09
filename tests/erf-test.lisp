;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: SPECIAL-FUNCTIONS-TESTS -*-
;;; Copyright (c) 2020 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:special-functions-tests)

(def-suite erf
  :description "Tests for the error and related functions"
  :in all-tests)
(in-suite erf)

;;; ERF

(test erf-small
  :description "Test error function on values very near 0"
  (let ((result (test-fn (select erf-small-data t 1)
			 #'(lambda (i params)
			     (specfun:erf (aref (car params) i)))
			 (select erf-small-data t 0))))
    (is (> (eps 1) (max-error  result)) "Maximum error exceeds 1ε threshold")
    (is (> (eps 1) (mean-error result)) "Mean error exceeds 1ε threshold")
    (is (> (eps 1) (rms result))        "RMS error exceeds 1ε threshold")
    (print-test-summary result :report-epsilon t)))

(test erf-medium
  :description "Test error function on interval (-8 8)"
  (let ((result (test-fn (select erf-data t 1)
			 #'(lambda (i params)
			     (specfun:erf (aref (car params) i)))
			 (select erf-data t 0))))
    (is (> (eps 1) (max-error  result)) "Maximum error exceeds 1ε threshold")
    (is (> (eps 1) (mean-error result)) "Mean error exceeds 1ε threshold")
    (is (> (eps 1) (rms result))        "RMS error exceeds 1ε threshold")
    (print-test-summary result :report-epsilon t)))

#+nil
(test erf-large			; current implementation of erf is only valid for x < 6.0
  :description "Test error function on interval [8 90)"
  (let ((result (test-fn (select erf-large-data t 1)
			 #'(lambda (i params)
			     (specfun:erf (aref (car params) i)))
			 (select erf-large-data t 0))))
    (is (> (eps 1) (max-error  result)) "Maximum error exceeds 1ε threshold")
    (is (> (eps 1) (mean-error result)) "Mean error exceeds 1ε threshold")
    (is (> (eps 1) (rms result))        "RMS error exceeds 1ε threshold")
    (print-test-summary result :report-epsilon t)))



;;; ERFC

(test erfc-small
  :description "Test complementary error function on values near 0"
  (let ((result (test-fn (select erf-small-data t 2)
			 #'(lambda (i params)
			     (specfun:erfc (aref (car params) i)))
			 (select erf-small-data t 0))))
    (is (> (eps 1) (max-error  result)) "Maximum error exceeds 1ε threshold")
    (is (> (eps 1) (mean-error result)) "Mean error exceeds 1ε threshold")
    (is (> (eps 1) (rms result))        "RMS error exceeds 1ε threshold")
    (print-test-summary result :report-epsilon t)))

(test erfc-medium
  :description "Test complementary error function on interval (-8 8)"
  (let ((result (test-fn (select erf-data t 2)
			 #'(lambda (i params)
			     (specfun:erfc (aref (car params) i)))
			 (select erf-data t 0))))
    (is (> (eps 1.8) (max-error  result)) "Maximum error exceeds 1.8ε threshold")
    (is (> (eps 1) (mean-error result)) "Mean error exceeds 1ε threshold")
    (is (> (eps 1) (rms result))        "RMS error exceeds 1ε threshold")
    (print-test-summary result :report-epsilon t)))

#-allegro
(test erfc-large
  :description "Test complementary error function on interval [8 90)"
  (let ((result (test-fn (select erf-large-data t 2)
			 #'(lambda (i params)
			     (specfun:erfc (aref (car params) i)))
			 (select erf-large-data t 0))))
    (is (> (eps 1) (max-error  result)) "Maximum error exceeds 1ε threshold")
    (is (> (eps 1) (mean-error result)) "Mean error exceeds 1ε threshold")
    (is (> (eps 1) (rms result))        "RMS error exceeds 1ε threshold")
    (print-test-summary result :report-epsilon t)))



;;; ERF INVERSE

(test erf-inverse
  :description "Test inverse error function"
  (let ((result (test-fn (select erf-inverse-data t 1)
			 #'(lambda (i params)
			     (specfun:inverse-erf (aref (car params) i)))
			 (select erf-inverse-data t 0))))
    (is (> (eps 2) (max-error  result)) "Maximum error exceeds 2ε threshold")
    (is (> (eps 1) (mean-error result)) "Mean error exceeds 1ε threshold")
    (is (> (eps 1) (rms result))        "RMS error exceeds 1ε threshold")
    (print-test-summary result :report-epsilon t)))


;;; ERFC INVERSE

(test erfc-inverse
  :description "Test inverse error function on the interval (0 2)"
  (let ((result (test-fn (select erfc-inverse-data t 1)
			 #'(lambda (i params)
			     (specfun:inverse-erfc (aref (car params) i)))
			 (select erfc-inverse-data t 0))))
    (is (> (eps 2) (max-error  result)) "Maximum error exceeds 2ε threshold")
    (is (> (eps 1) (mean-error result)) "Mean error exceeds 1ε threshold")
    (is (> (eps 1) (rms result))        "RMS error exceeds 1ε threshold")
    (print-test-summary result :report-epsilon t)))


;;; The values in this test are so small that, at double-float
;;; precision, they cause inverse-erf to return ±∞. Boost only runs
;;; this test using C long-doubles and here we just check that an
;;; error is signaled.
#-allegro
(test erfc-inverse-small
  :description "Test inverse error function from near 0 to 1 "
  (signals floating-point-invalid-operation
    (test-fn (select erfc-inverse-small-data t 1)
			 #'(lambda (i params)
			     (specfun:inverse-erfc (aref (car params) i)))
			 (select erfc-inverse-small-data t 0)))
  #+nil
  (let ((result (test-fn (select erfc-inverse-small-data t 1)
			 #'(lambda (i params)
			     (specfun:inverse-erfc (aref (car params) i)))
			 (select erfc-inverse-small-data t 0))))
    (is (> (eps 2) (max-error  result)) "Maximum error exceeds 2ε threshold")
    (is (> (eps 1) (mean-error result)) "Mean error exceeds 1ε threshold")
    (is (> (eps 1) (rms result))        "RMS error exceeds 1ε threshold")
    (print-test-summary result :report-epsilon t)))
