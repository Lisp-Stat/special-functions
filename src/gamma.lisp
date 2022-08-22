;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: SPECFUN -*-
;;; Copyright (c) 2021-2022 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:special-functions)

;;; Resources:
;;; [1] N.M. Temme, A Set of Algorithms for the Incomplete Gamma
;;; Functions, Probability in the Engineering and Informational
;;; Sciences, 8 (1994), pp.291-307, available as
;;; http://oai.cwi.nl/oai/asset/10080/10080A.pdf
;;; [2] Boost C++ Libraries, Release 1.80.0, 2022, http://www.boost.org/
;;; [3] W. Ehrhardt, DAMath, https://github.com/CMCHTPC/ChromaPrint/tree/master/DAMath/damath_2015-07-01

(defconstant maxgamd 171.62437695630272d0 "Maximum argument for gamma")
(defconstant log-minimum-double-value -708.396418532264d0)
(defconstant log-maximum-double-value  709.782712893384d0)


;;;
;;; Gamma
;;;

(defun stirling (x)
  "Return gamma(x) for x > 13"
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
  (if (or (eq x double-float-nan)
	  (= x double-float-positive-infinity)
	  (plusp x))
      1d0
      -1d0))

(defun gamma (x)
  "Return gamma(x), x <= +MAXGAMD+; NAN/RTE if x is a non-positive integer"
  (declare (double-float x))
  (let (i (q (abs x)) z)
    (cond ((= x double-float-positive-infinity) double-float-positive-infinity)
	  ((> x maxgamd) double-float-positive-infinity)
	  ((and (minusp x)
		(= 0 (rem x 1))) ;can't use integerp since we declared x double-float
	   double-float-nan)

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



;;;
;;; Incomplete Gamma
;;;

;; Reference: Boost, gamma.hpp, function finite_gamma_q
(defun q-gamma-integer (a x)
  "Return Q(a,x) when A is an integer, A < min(30,x+1)"
  (declare (double-float a x))
  (let ((sum (exp (- x))))
    (when (not (= sum 0))
      (let ((term sum))
	(loop for n from 1 below a
	      do (setf term (/ term n)
		       term (* term x)
		       sum  (+ term sum)))))
    sum))

;; Reference: Boost finite_half_gamma_q
(defun q-gamma-half (a x)
  "Calculates normalised Q when a is a half-integer for a < min(30, x+1)"
  (declare (double-float a x))
  (let ((e (erfc (sqrt x)))
	(sum 0))
    (when (and (not (= e 0))
	       (> a 1))
      (let ((term (/ (exp (- x))
		     (sqrt (* x pi)))))
	(setf term (* term x)
	      term (/ term 0.5)
	      sum term)
	(loop for n from 2 below a
	      do (setf term (/ term (- n 0.5))
		       term (* term x)
		       sum  (+ term sum)))))
    (+ e sum)))

;; Reference: Boost igamma_large.hpp
(defun pq-asymptotic (a x)
  "Incomplete gamma functions for large A and A near X"
  (declare (double-float a x))
  (let* ((c0 (make-array 19
			 :initial-contents '(-5.0276692801141755d-12
					      2.4361948020667415d-11
					      -5.830772132550426d-11
					      -2.5514193994946248d-11
					      9.14769958223679d-10
					      -4.382036018453353d-9
					      1.0261809784240309d-8
					      6.707853543401498d-9
					      -1.7665952736826078d-7
					      8.296711340953087d-7
					      -1.85406221071516d-6
					      -2.185448510679992d-6
					      3.919263178522438d-5
					      -1.787551440329218d-4
					      3.527336860670194d-4
					      0.0011574074074074073d0
					      -0.014814814814814815d0
					      0.08333333333333333d0
					      -0.3333333333333333d0)
			  :element-type 'double-float))

	 (c1 (make-array 17
			 :initial-contents '(-8.56390702649298d-11
					     4.162792991842583d-10
					     -1.0091543710600413d-9
					     -1.7543241719747647d-11
					     1.1951628599778148d-8
					     -5.752545603517705d-8
					     1.378633446915721d-7
					     4.647127802807434d-9
					     -1.6120900894563446d-6
					     7.64916091608111d-6
					     -1.8098550334489977d-5
					     -4.018775720164609d-7
					     2.0576131687242798d-4
					     -9.902263374485596d-4
					     0.0026455026455026454d0
					     -0.003472222222222222d0
					     -0.001851851851851852d0)
			 :element-type 'double-float))

	 (c2 (make-array 15
			 :initial-contents '(-1.3670488396617114d-9
					     6.228974084922022d-9
					     -1.409252991086752d-8
					     -2.0477098421990866d-10
					     1.4280614206064242d-7
					     -6.298992138380055d-7
					     1.3721957309062934d-6
					     3.423578734096138d-8
					     -1.2760635188618728d-5
					     5.2923448829120125d-5
					     -1.073665322636516d-4
					     2.0093878600823047d-6
					     7.716049382716049d-4
					     -0.0026813271604938273d0
					     0.004133597883597883d0)
			 :element-type 'double-float))

	 (c3 (make-array 13
			 :initial-contents '(-1.9111168485973655d-8
					     8.099464905388083d-8
					     -1.6958404091930278d-7
					     -2.7861080291528143d-11
					     1.4230900732435883d-6
					     -5.6749528269915965d-6
					     1.1082654115347302d-5
					     -2.396505113867297d-7
					     -7.561801671883977d-5
					     2.6772063206283885d-4
					     -4.691894943952557d-4
					     2.2947209362139917d-4
					     6.494341563786008d-4)
			 :element-type 'double-float))

	 (c4 (make-array 11
			 :initial-contents '(-2.292934834000805d-7
					     8.907507532205309d-7
					     -1.6954149536558305d-6
					     2.507497226237533d-10
					     1.1375726970678419d-5
					     -3.968365047179435d-5
					     6.641498215465122d-5
					     -1.4638452578843418d-6
					     -2.990724803031902d-4
					     7.840392217200666d-4
					     -8.618882909167117d-4)
			 :element-type 'double-float))

	 (c5 (make-array 9
			 :initial-contents '(-2.291481176508095d-6
					     8.018470256334202d-6
					     -1.3594048189768693d-5
					     1.419062920643967d-7
					     6.797780477937208d-5
					     -1.9932570516188847d-4
					     2.772753244959392d-4
					     -6.972813758365857d-5
					     -3.3679855336635813d-4)
			 :element-type 'double-float))

	 (c6 (make-array 11
			 :initial-contents '(5.788792863149004d-7
					     -2.0291327396058603d-6
					     3.465155368803609d-6
					     -3.0796134506033047d-9
					     -1.8329116582843375d-5
					     5.61168275310625d-5
					     -8.153969367561969d-5
					     7.902353232660328d-7
					     2.708782096718045d-4
					     -5.921664373536939d-4
					     5.313079364639922d-4)
			 :element-type 'double-float))

	 (c7 (make-array 9
			 :initial-contents '(5.7876949497350525d-6
					     -1.8263488805711332d-5
					     2.7744451511563645d-5
					     -1.2741009095484485d-7
					     -1.0976582244684731d-4
					     2.812695154763237d-4
					     -3.3493161081142234d-4
					     5.171790908260592d-5
					     3.4436760689237765d-4)
			 :element-type 'double-float))

	 (c8 (make-array 7
			 :initial-contents '(4.629953263691304d-5
					     -1.2783517679769218d-4
					     1.6644846642067547d-4
					     -6.969091458420552d-7
					     -4.38297098541721d-4
					     8.394987206720873d-4
					     -6.526239185953094d-4)
			 :element-type 'double-float))

	 (c9 (make-array 5
			 :initial-contents '(2.7750107634328704d-4
					     -6.401475260262758d-4
					     6.782308837667328d-4
					     -7.204895416020011d-5
					     -5.967612901927463d-4)
			 :element-type 'double-float))

	 (c10 (make-array 3
			  :initial-contents '(0.0011089369134596636d0
					      -0.0019144384985654776d0
					      0.0013324454494800656d0)
			 :element-type 'double-float))

	 (c11 (make-array 5
			  :initial-contents '(-0.0010108559391263003d0
					      0.00213896861856891d0
					      -0.0020633421035543276d0
					      1.6251626278391583d-4
					      0.001579727660730835d0)
			 :element-type 'double-float))

	 (c12 (make-array 3
			  :initial-contents '(-0.004041016108167662d0
					      0.00640336283380807d0
					      -0.004072512119514016d0)
			 :element-type 'double-float))

	 (w (make-array 13 :element-type 'double-float))
	 (sigma (/ (- x a) a))
	 (phi (- (nu:log1pmx sigma)))
	 (y (* a phi))
	 (z (sqrt (* 2d0 phi)))
	 p q)

    (when (< x a)
      (setf z (- z)))

    (map-into w
	      #'evaluate-polynomial
	      `(,c0 ,c1 ,c2 ,c3 ,c4 ,c5 ,c6 ,c7 ,c8 ,c9 ,c10 ,c11 ,c12)
	      (alexandria:make-circular-list 13 :initial-element z))

    (setf p (evaluate-polynomial (reverse w) (/ a)) ;our polynomial evaluation reverses the coefficients
	  p (* p
	       (/ (exp (- y))
		  (sqrt (* a 2 pi)))))

    (when (< x a)
      (setf p (- p)))

    (setf p (+ p			;from Boost
	       (/ (erfc (sqrt y))
		  2)))

    (if (>= x a)
	(setf q p
	      p (- 1d0 q))
	(setf q (- 1d0 p)))

    (values p q)))

;;; The difference between these two taylor series is whether they
;;; compute p or q, and then define one in terms of the other in the
;;; return values, e.g. 1 - q to compute p.
;;; Reference: Temme formula (5.5) and function ptaylor/qtaylor
;;; functions

(defun p-taylor (a x dax)
  "Temme/Gautschi code for P(a,x), dax = x^a*exp(-x)/gamma(a+1)
Returns (values p q)"
  (declare (double-float a x dax))
  (when (or (<= a 0)
	    (<= x 0))
    (return-from p-taylor (values 0d0 1d0)))
  (loop for r = a then (1+ r)
	for c = 1d0 then (* c (/ x r))
	for p = 1d0 then (+ p c)
	until (< c (* p double-float-epsilon))
	finally (return (values (* p (/ dax a)) (- 1d0 (* p (/ dax a)))))))

;;; Direct calculation
(defun gamma-aux (x)
  (declare (double-float x))
  (/ (1- (/ (gamma (1+ x))))
     (* x (1- x))))

(defun q-taylor (a x)
  "Temme/Gautschi code for Q(a,x) when x < 1"
  (declare (double-float a x))
  (let* ((q (1- (expt x a)))
	 (s (* (- a) (1- a) (gamma-aux a)))
	 (u (- s (* q (- 1 s))))
	 (p (* a x))
	 (q (1+ a))
	 (r (+ a 3))
	 (term 1d0)
	 (v 1d0))
    (loop
      do (setf p (+ p x)
	       q (+ q r)
	       r (+ r 2)
	       term (* (- p) (/ term q))
	       v (+ v term))
      until (< (abs term) (* (abs v) double-float-epsilon)))
    (setf term (* a (- 1 s) x (exp (* a (log x))))
	  r (* term (/ v (1+ a)))
	  q (+ r u)
	  p (- 1 q))
    (values p q)))

;; Reference: Cephes igam.c, igamc_continued_fraction
(defun q-fraction (a x dax)
  "Continued fraction for Q(a,x)"
  (declare (double-float a x dax))
  (let ((big 4.503599627370496d15)
	(biginv 2.22044604925031308085d-16)
	ans c yc r term y z pk pkm1 pkm2 qk qkm1 qkm2)

    (when (= 0 dax)
      (return-from q-fraction 0d0))

    ;; continued fraction
    (setf  y (- 1 a)
	   z (+ x y 1)
	   c 0
	   pkm2 1
	   qkm2 x
	   pkm1 (1+ x)
	   qkm1 (* z x)
	   ans (/ pkm1 qkm1))
    (loop
      do (progn (setf c (1+ c)
		      y (1+ y)
		      z (+ 2 z)
		      yc (* y c)
		      pk (- (* pkm1 z)
			    (* pkm2 yc))
		      qk (- (* qkm1 z)
			    (* qkm2 yc)))
		(if (not (= qk 0))
		    (setf r (/ pk qk)
			  term (abs (/ (- ans r)
				       r))
			  ans r)
		    (setf term 1d0))
		(setf pkm2 pkm1
		      pkm1 pk
		      qkm2 qkm1
		      qkm1 qk)
		(when (> (abs pk) big)
		    (setf pkm2 (* pkm2 biginv)
			  pkm1 (* pkm1 biginv)
			  qkm2 (* qkm2 biginv)
			  qkm1 (* qkm1 biginv))))
      until (<= term double-float-epsilon))
    (values (- 1 (* ans dax)) (* ans dax))))


;; Convenience functions for p and q (upper & lower) incomplete gamma
(defun lower-incomplete-gamma (x a)
  "Return the normalised lower incomplete gamma function P(a,x), a>=0, x>=0
P(a,x) = integral(exp(-t)*t^(a-1), t=0..x)/gamma(a)"
  (incomplete-gamma x a))		;first value is p

(defun upper-incomplete-gamma (x a)
  "Return the normalised upper incomplete gamma function Q(a,x), a>=0, x>=0
Q(a,x) = integral(exp(-t)*t^(a-1), t=x..Inf)/gamma(a))"
  (nth-value 1 (incomplete-gamma x a)))

(defun incomplete-gamma (a x &key (compute-prefix nil))
  "Return the normalised incomplete gamma functions P and Q, a>=0, x>=0
P(a,x) = integral(exp(-t)*t^(a-1), t=0..x  )/gamma(a)
Q(a,x) = integral(exp(-t)*t^(a-1), t=x..Inf)/gamma(a))
dax    = x^a*exp(-x)/gamma(a) (prefix factor)

Returns three values:
P is the first value, Q the second, DAX the third, e.g. (values p q dax)"
  (declare (double-float a x))
  (let (p q
	(dax 0d0))
    (cond ((or (< x 0)			;TODO check for NaN or +/- infinity too
	       (< a 0))
	   (return-from incomplete-gamma double-float-nan))

	  ;; Handle trivial cases
	  ((= a x 0) (return-from incomplete-gamma (values 0.5d0 0.5d0 nil)))
	  ((= x 0)   (return-from incomplete-gamma (values 0d0 1d0 nil)))
	  ((= a 0)   (return-from incomplete-gamma (values 1d0 0d0 nil))))

    (when compute-prefix
      (setf dax (regularised-gamma-prefix a x)))

    (when (= 0 (nth-value 1 (truncate (* 2 a))))	;a is an integer or half integer
      (cond ((= a 0.5)                  ; p = (erf (sqrt x)); q = (erfc (sqrt x))
	     (let ((x (sqrt x)))
	       (if (< x 1)
		   (setf p (erf x)
			 q (- 1d0 p))
		   (setf q (erfc x)
			 p (- 1d0 q)))))
	     ((= a 1)
	      (setf p (- (nu:exp-1 (- x)))
		    q (exp (- x))))
	     ((and (< a 30)
		   (< a (1+ x)))
	      (cond ((and (= 0 (nth-value 1 (truncate a))) ;a is an integer
			  (> x 0.6))
		     (setf q (q-gamma-integer a x)
			   p (- 1d0 q)))
		    ((> x 0.2d0)
		     (setf q (q-gamma-half a x)
			   p (- 1d0 q)))))))

    ;; Check if asymptotic expansion should be used
    (when (> a 20)
      (let ((mu (/ (- x a)
		   a)))
	(when (or (and (> a 200)
		       (< (abs mu) (sqrt (/ 20 a))))
		  (< (abs mu) 0.4))
	  (multiple-value-setq (p q)
	    (pq-asymptotic a x)))))

    (when (not compute-prefix)
      (setf dax (regularised-gamma-prefix a x)))

    (if (< dax least-positive-double-float)
	(progn
	  (if (> a x)
	      (setf p 0d0)
	      (setf p 1d0))
	  (setf q (- 1d0 p)))
	;; Calculate either Taylor series or continued fraction
	(when (not p)			;or q?
	  (let (alfa)
	    (if (> x 0.25)
		(setf alfa (+ x 0.25d0))
		(setf alfa (- (/ (log 2d0)
				 (log x)))))
	    (cond ((> a alfa)
		   (multiple-value-setq (p q)
		     (p-taylor a x dax)))
		  ((< x 1)
		   (multiple-value-setq (p q)
		     (q-taylor a x)))
		  (t
		     (multiple-value-setq (p q)
		       (q-fraction a x dax)))))))
    (values p q dax)))

;;; Reference: Cephes, igam.c, function igam_fac
(defun regularised-gamma-prefix (a x)
  "Return x^a * exp(-x) / gamma(a)
This function accepts either RATIONAL or FLOAT values."
  (let ((lanczos-g lanczos:g)
	(max-log 7.09782712893383996732d2)
	(e (exp 1d0))
	fac res num)
    (if (> (abs (- a x)) (* 0.4 (abs a)))
	(let ((ax (- (* a (log x))
		     x
		     (log-gamma a))))	;float contagion
	  (if (< ax (- max-log))
	      ;; TODO signal a condition...
	      (return-from regularised-gamma-prefix 0d0)
	      (return-from regularised-gamma-prefix (exp ax)))))
    (setf fac (+ a (- lanczos-g 1/2))
	  res (/ (sqrt (/ fac e))
		 (lanczos:lanczos-sum a)))
    (if (and (< a 200) (< x 200))
	(progn
	  (setf res (* res
		       (exp (- a x))
		       (expt (/ x fac) a))))
	(progn
	  (setf num (- x a (+ lanczos-g 1/2))
		res (* res
		       (exp (+ (* a (nu:log1pmx (/ num fac)))
			       (* x (- 1/2 lanczos-g) (/ fac))))))))
    res))

;;; Reference: Boost gamma.hpp, gamma_p_derivative_imp
;;; Used to compute PDF
(defun gamma-p-derivative (a x)
  "Partial derivative with respect to x of the incomplete gamma function"
  (let ((f1 (regularised-gamma-prefix a x)))
    (cond ((<= a 0) (error "Argument a to the incomplete gamma function must be greater than zero (got a=~S)." a))
	  ((< x 0) (error "Argument x to the incomplete gamma function must be >= 0 (got x=~S)." x))
	  ((= x 0) (cond ((> a 1) 0)
			 ((= a 1) 1)))	;(= a 1) should overflow

	  ;; Normal cases

	  ;; Overflow in double-float domain
	  ((and (floatp x)
		(< x 1)
		(< f1 (* x most-positive-double-float))
		;; Boost signals an overflow error here.
		(warn "Possible overflow in gamma-p-derivative")))

	  ;; Underflow in double-float domain, use logs instead
	  ((zerop f1) (exp (- (* a (log x))
			      x (log-gamma a)
			      (log x))))
	  (t (/ f1 x)))))


#| An alternative implementation

;;; Reference: https://www.boost.org/doc/libs/1_41_0/boost/math/special_functions/gamma.hpp
;;; This passes all Boost tests.
(defun regularised-gamma-prefix* (a z)
  "Return (z^a)(e^-z)/gamma(a), the power term prefix, using Lanczos summation
Most of the error occurs in this function"
  (declare (double-float a z))
  (let* ((agh (+ a lanczos::g-1/2))
	 (d (/ (+ 0.5
		  (- z a lanczos::g))
	       agh))
	 prefix)
    (cond ((< a 1d0) ;In the Boost implementation, a<1 is a special
		     ;case because the Lanczos approximations are
		     ;optimised for a>1. This may not be the case any
					;longer.
	   ;; (format t "a < 1 special case in gamma-prefix~%")
	   (if (< z log-minimum-double-value)
	       (progn			;remove progn when debugging complete
		 (format t "z < log-minimum-double-value")
		 (return-from regularised-gamma-prefix- (exp (- (* a (log z))
							       z
							       (log-gamma a))))
		 )
	       (progn			;remove progn when debugging complete
		 ;; (format t "using direct calculation of gamma-prefix~%")
	       (return-from regularised-gamma-prefix- (/ (* (expt z a)
							   (exp (- z)))
							(gamma a))))) ;no overflow danger, use direct calculation
	   )
	  ((and (<= (abs (* d d a)) 100)	;special case for large z and a ~ z
		(> a 150))
	   (setf prefix (exp (+ (* a (nu:log1pmx d))
				(/ (* z
				      (- 0.5 lanczos::g))
				   agh)))))
	  (t ;general case
	   ;; (format t "general case~%")
	   (let ((alz (* a (log (/ z agh))))
		 (amz (- a z)))
	     (if (or (<= (min alz amz) log-minimum-double-value)
		     (>= (max alz amz) log-maximum-double-value))
		 (let ((amza (/ amz a)))
		   (cond ((and (> (/ (min alz amz) 2) log-minimum-double-value)
			       (< (/ (max alz amz) 2) log-maximum-double-value))
			  ;; Compute square root of result and square it
			  ;; (format t "Compute square root of result and square it")
			  (setf prefix (square (* (expt (/ z agh) (/ a 2)) (exp (/ amz 2))))))
			 ((and (> (/ (min alz amz) 4) log-minimum-double-value)
			       (< (/ (max alz amz) 4) log-maximum-double-value)
			       (> z a))
			  ;; Compute 4th root of result and square it twice
			  (format t "4th root case")
			  (setf prefix (square (square (* (expt (/ z agh) (/ a 4)) (exp (/ amz 4)))))))
			 ((and (> amza log-minimum-double-value)
			       (< amza log-maximum-double-value))
			  ;; (format t "Else case")
			  (setf prefix (expt (/ (* z (exp amza))
						agh)
					     a)))
			 (t ;;(format t "Not a special case of prefix")
			    (setf prefix (exp (+ alz amz))))))
		 (setf prefix (* (expt (/ z agh) a) (exp amz)))))))
    (* prefix
       (/ (sqrt (/ agh (exp 1d0)))
	  (lanczos:lanczos-sum a)))))
|#
