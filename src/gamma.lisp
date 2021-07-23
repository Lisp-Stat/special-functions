;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: SPECFUN -*-
;;; Copyright (c) 2021 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:special-functions)

(defconstant maxgamd 171.62437695630272d0 "Maximum argument for gamma")
(defconstant tiny 1d-300)

(defun stirling (x)
  "Return (gamma x) for x > 13"
  ;; Reference: stirf in Cephes ldouble/gammal.c
  ;; gamma(x) = sqrt(2*Pi)*x^(x-0.5)*exp(-x)*(1 + 1/x * P(1/x))
  ;; 13 <= x <= 1024, relative peak error = 9.44E-21, relative error spread = 8.8E-4
  (declare (type double-float x))
  (let* ((sp (make-array 9
			:initial-contents '(7.1473913781436109426d-4
					    -2.3638488095017589101d-5
					    -5.9502375540563298261d-4
					    6.9893322606231933383d-5
					    7.8403348427447529072d-4
					    -2.2947197478731854413d-4
					    -2.6813271618763043040d-3
					    3.4722222222300751226d-3
					    8.3333333333333314830d-2)
			:element-type 'double-float))
	 (w (/ x))
	 (v) (y))

    (setf w (1+ (* w (evaluate-polynomial sp w))))
    (if (> x 143.016)
	(setf v (expt x (* (/ 2) (- x (/ 4))))
	      y (* (square v) (exp (- x))))
	(setf y (/ (expt x (- x (/ 2))) (exp x))))
    (* +square-root-2-pi+ y w)))

(defun gamma-inverse-small (x)
  "Return 1/gamma(x) for |x| < 0.03125"
  ;; 1/gamma(x) = x*P(x), 0 < x < 0.03125, peak relative error 4.2e-23
  (declare (type double-float x))
  (let ((sp (make-array 9
		      :initial-contents '(-1.193945051381510095614d-3
					  7.220599478036909672331d-3
					  -9.622023360406271645744d-3
					  -4.219773360705915470089d-2
					  1.665386113720805206758d-1
					  -4.200263503403344054473d-2
					  -6.558780715202540684668d-1
					  5.772156649015328608253d-1
					  1d0)
		      :element-type 'double-float))
	(sn (make-array 9
		       :initial-contents '(1.133374167243894382010d-3
					   7.220837261893170325704d-3
					   9.621911155035976733706d-3
					   -4.219773343731191721664d-2
					   -1.665386113944413519335d-1
					   -4.200263503402112910504d-2
					   6.558780715202536547116d-1
					   5.772156649015328608727d-1
					   -1d0)
		       :element-type 'double-float))
	(p 0))
    (cond ((= x 0) 0)
	  ((< x 0) (setf x (- x)
			 p (evaluate-polynomial sn x)))
	  (t       (setf p (evaluate-polynomial sp x))))
    (* x p)))

;;; Reference: Cephes function gammal in file ldouble/gammal.c
(defun gamma-medium (x)
  "Return gamma(x), |x| <= 13, x negative integer produces div by 0"
  ;; gamma(x+2) = P(x)/Q(x), 0 <= x <= 1, peak relative error = 1.83e-20
  (declare (type double-float x))
  (let ((p (make-array 8
		       :initial-contents '(4.212760487471622013093d-5
					   4.542931960608009155600d-4
					   4.092666828394035500949d-3
					   2.385363243461108252554d-2
					   1.113062816019361559013d-1
					   3.629515436640239168939d-1
					   8.378004301573126728826d-1
					   1.0d0)
		       :element-type 'double-float))
	(q (make-array 9
		       :initial-contents '(-1.397148517476170440917d-5
					   2.346584059160635244282d-4
					   -1.237799246653152231188d-3
					   -7.955933682494738320586d-4
					   2.773706565840072979165d-2
					   -4.633887671244534213831d-2
					   -2.243510905670329164562d-1
					   4.150160950588455434583d-1
					   9.999999999999999999908d-1)
		       :element-type 'double-float))
	(z 1d0))

    (loop while (>= x 3d0) ; -13 <= x <= 13. Use recurrence formula to bring argument to [2,3)
	  do (setf x (- x 1d0)
		   z (* z x)))
    (loop while (< x -0.03125d0)
	  do (setf z (/ z x)
		   x (1+ x)))

    ;; At this point -0.03125 <= x < 3

    ;argument near a pole, use approximation of 1/gamma
    (if (<= x 0.03125) (return-from gamma-medium (/ z (gamma-inverse-small x))))

    ;; finish reduction to [2,3)
    (loop while (< x 2.0d0)
	  do (setf z (/ z x)
		   x (1+ x)))

    (if (= x 2.0) (return-from gamma-medium z))

    (setf x (- x 2.0d0))
    (* z
       (/ (evaluate-polynomial p x)
	  (evaluate-polynomial q x)))))

(defun sign-gamma (x)
  "Return sign(gamma(x)), invalid for 0 or negative integer"
  (check-type x double-float)
  (if (or ;;(eq x double-float-nan)	;20210618 SBCL has a bug with double-float-nan
       ;; (= x (bits-double-float #b0111111111111000000000000000000000000000000000000000000000000000))
       (= x double-float-positive-infinity)
       (plusp x))
      1d0
      -1d0))

(defun gamma (x)
  "Return gamma(x), x <= MAXGAMD; NAN/RTE if x is a non-positive integer"
  (declare (double-float x))
  (let (i (q (abs x)) z)
    (cond ((= x double-float-positive-infinity) double-float-positive-infinity)
	  ((> x maxgamd) double-float-positive-infinity)
	  ((and (minusp x) (= 0 (rem x 1)))
	   ;workaround SBCL bug. See mailing list
	   (bits-double-float #b0111111111111000000000000000000000000000000000000000000000000000)) ;double-float-nan
	  ;; double-float-nan) ;x is a negative integer

	  ((<= q 13.0d0) (gamma-medium x))
	  ((and (minusp x) (<= q (1- maxgamd)))
	   (setf i (truncate q)
		 z (- q i))
	   (if (> z (/ 2)) (setf z (- q (1+ i))))
	   (setf z (abs (* q (sin-pi z) (stirling q))))
	   (if (<= z (/ pi most-positive-double-float))
	       (* double-float-positive-infinity (sign-gamma x))
	       (* (sign-gamma x) (/ pi z))))
	  ((minusp x) (* (sign-gamma x) (exp (log-gamma x)))) ;negative & near maxgamd, most values underflow
	  (t (stirling x)))))


