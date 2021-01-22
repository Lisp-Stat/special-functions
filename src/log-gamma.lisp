;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: SPECFUN -*-
;;; Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
;;; Copyright (c) 2020 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:special-functions)

#| Openlibm implementation
https://github.com/JuliaMath/openlibm/blob/master/src/e_lgamma_r.c

 Method:
   1. Argument Reduction for 0 < x <= 8
 	Since gamma(1+s)=s*gamma(s), for x in [0,8], we may
 	reduce x to a number in [1.5,2.5] by
 		lgamma(1+s) = log(s) + lgamma(s)
	for example,
		lgamma(7.3) = log(6.3) + lgamma(6.3)
			    = log(6.3*5.3) + lgamma(5.3)
			    = log(6.3*5.3*4.3*3.3*2.3) + lgamma(2.3)
   2. Polynomial approximation of lgamma around its
	minimun ymin=1.461632144968362245 to maintain monotonicity.
	On [ymin-0.23, ymin+0.27] (i.e., [1.23164,1.73163]), use
		Let z = x-ymin;
		lgamma(x) = -1.214862905358496078218 + z^2*poly(z)
	where
		poly(z) is a 14 degree polynomial.
   2. Rational approximation in the primary interval [2,3]
	We use the following approximation:
		s = x-2.0;
		lgamma(x) = 0.5*s + s*P(s)/Q(s)
	with accuracy
		|P/Q - (lgamma(x)-0.5s)| < 2**-61.71
	Our algorithms are based on the following observation

                             zeta(2)-1    2    zeta(3)-1    3
 lgamma(2+s) = s*(1-Euler) + --------- * s  -  --------- * s  + ...
                                 2                 3

	where Euler = 0.5771... is the Euler constant, which is very
	close to 0.5.

   3. For x>=8, we have
	lgamma(x)~(x-0.5)log(x)-x+0.5*log(2pi)+1/(12x)-1/(360x**3)+....
	(better formula:
	   lgamma(x)~(x-0.5)*(log(x)-1)-.5*(log(2pi)-1) + ...)
	Let z = 1/x, then we approximation
		f(z) = lgamma(x) - (x-0.5)(log(x)-1)
	by
	  			    3       5             11
		w = w0 + w1*z + w2*z  + w3*z  + ... + w6*z
	where
		|w - f(z)| < 2**-58.74

   4. For negative x, since (G is gamma function)
		-x*G(-x)*G(x) = pi/sin(pi*x),
 	we have
 		G(x) = pi/(sin(pi*x)*(-x)*G(-x))
	since G(-x) is positive, sign(G(x)) = sign(sin(pi*x)) for x<0
	Hence, for x<0, signgam = sign(sin(pi*x)) and
		lgamma(x) = log(|Gamma(x)|)
			  = log(pi/(|x*sin(pi*x)|)) - lgamma(-x);
	Note: one should avoid computing pi*(-x) directly in the
	      computation of sin(pi*(-x)).

   5. Special Cases
		lgamma(2+s) ~ s*(1-Euler) for tiny s
		lgamma(1) = lgamma(2) = 0
		lgamma(x) ~ -log(|x|) for tiny x
		lgamma(0) = lgamma(neg.integer) = inf and raise divide-by-zero
		lgamma(inf) = inf
		lgamma(-inf) = inf (bug for bug compatible with C99!?)
|#

#| Note: the libm implementation is some really horrible code; just
short of obfuscated IMO. I've tried to rationalise it the best I can,
and it now only bears a passing resemblance to the original. |#

(defun sin-pi (x)
  "Returns (sin (* pi x))"
  (declare (double-float x))
  (sin (* cl:pi x)))

(let ((two52 4.50359962737049600000d+15) ; 0x43300000, 0x00000000
      (two58 2.8823037615171174d17)
      (half  5.00000000000000000000d-01)
      (one   1.00000000000000000000d+00)

      (a1 (make-array 6
		      :initial-contents '(2.52144565451257326939d-05
					  2.20862790713908385557d-04
					  1.19270763183362067845d-03
					  7.38555086081402883957d-03
					  6.73523010531292681824d-02
					  7.72156649015328655494d-02)
		      :element-type 'double-float))

      (a2 (make-array 6
		      :initial-contents '(4.48640949618915160150d-05
					  1.08011567247583939954d-04
					  5.10069792153511336608d-04
					  2.89051383673415629091d-03
					  2.05808084325167332806d-02
					  3.22467033424113591611d-01)
		      :element-type 'double-float))

      (tc    1.46163214496836224576d+00) ; 0x3FF762D8, 0x6356BE3F
      (tf   -1.21486290535849611461d-01) ; 0xBFBF19B9, 0xBCC38A42
      (tt   -3.63867699703950536541d-18) ;; tt  -(tail of tf)

      (t1 (make-array 5
		      :initial-contents '(3.15632070903625950361d-04
					  -1.40346469989232843813d-03
					  6.10053870246291332635d-03
					  -3.27885410759859649565d-02
					  4.83836122723810047042d-01)
		      :element-type 'double-float))

      (t2 (make-array 5
		      :initial-contents '(-3.12754168375120860518d-04
					  8.81081882437654011382d-04
					  -3.68452016781138256760d-03
					  1.79706750811820387126d-02
					  -1.47587722994593911752d-01)
		      :element-type 'double-float))

      (t3 (make-array 5
		      :initial-contents '(3.35529192635519073543d-04
					  -5.38595305356740546715d-04
					  2.25964780900612472250d-03
					  -1.03142241298341437450d-02
					  6.46249402391333854778d-02)
		      :element-type 'double-float))

      (u (make-array 6
		     :initial-contents '(1.33810918536787660377d-02
					 2.28963728064692451092d-01
					 9.77717527963372745603d-01
					 1.45492250137234768737d0
					 6.32827064025093366517d-01
					 -7.72156649015328655494d-02)
			 :element-type 'double-float))

      (v (make-array 5
		     :initial-contents '(3.21709242282423911810d-03
					 1.04222645593369134254d-01
					 7.69285150456672783825d-01
					 2.12848976379893395361d0
					 2.45597793713041134822d0)
			 :element-type 'double-float))

      (s (make-array 7
		     :initial-contents '(3.19475326584100867617d-05
					 1.84028451407337715652d-03
					 2.66422703033638609560d-02
					 1.46350472652464452805d-01
					 3.25778796408930981787d-01
					 2.14982415960608852501d-01
					 -7.72156649015328655494d-02)
			 :element-type 'double-float))

      (r (make-array 6
		     :initial-contents '(7.32668430744625636189d-06
					 7.77942496381893596434d-04
					 1.86459191715652901344d-02
					 1.71933865632803078993d-01
					 7.21935547567138069525d-01
					 1.39200533467621045958d+00)
			 :element-type 'double-float))

      (wc (make-array 6
		     :initial-contents '(-1.63092934096575273989d-03
					 8.36339918996282139126d-04
					 -5.95187557450339963135d-04
					 7.93650558643019558500d-04
					 -2.77777777728775536470d-03
					 8.33333333333329678849d-02)
		     :element-type 'double-float))

      (w0    4.18938533204672725052d-01)) ; 0x3FDACFE3, 0x90C97D69

  (defun log-gamma (n)
    "Return the logarithm of gamma(x)"
    (declare (double-float n))
    (let* ((x (abs n))
	   (nadj (log (/ pi (abs (* (sin-pi n) n)))))
	   (r (cond ((eql x  double-float-positive-infinity) (square x))
		    ((eql x  double-float-negative-infinity) (square x)) ; Why does SBCL claim this is unreachable?
		    ((= x 0) double-float-positive-infinity)
		    ((or (= x 1) (= x 2)) 0)
		    ((< x (expt 2 -70)) (* (signum x) (- (log (abs x)))))
		    ((and (> x two52) ; 2^52
			  (< n 0))
		     double-float-negative-infinity) ; negative infinity seems correct
		    ((= 0 (sin-pi x)) double-float-positive-infinity)

		    ;; Not using reflection formula in case we want
		    ;; multi-threaded calls. Not that I am certain it
		    ;; makes a difference, but the libm version does
		    ;; not use this either, presumably for this reason.

		    ;; Use reflection formula for negative inputs
		    ;; ((minusp n) (let* ((t-val (* n (sin-pi n))))
		    ;; 		 (- (log (abs (/ pi t-val)))
		    ;; 		    (log-gamma (abs n)))))

		    ((< x 0.23163998126983643d0) (let* ((p1      (* x (evaluate-polynomial u x)))
							(p2  (1+ (* x (evaluate-polynomial v x)))))
						   (+ (- (log x))
						      (* -1/2 x)
						      (/ p1 p2))))
		    ((< x 0.7315998077392578d0) (let* ((y (- x (- tc 1)))
						       (z (square y))
						       (w (* z y))
						       (p1 (evaluate-polynomial t1 w))
						       (p2 (evaluate-polynomial t2 w))
						       (p3 (evaluate-polynomial t3 w))
						       (p  (- (* z p1)
							      (- tt
								 (* w
								    (+ p2
								       (* y p3)))))))
						  (+ (- (log x)) tf p)))
		    ((< x 0.8999996185302734d0) (let* ((y (- 1 x))
						       (z (square y))
						       (p1      (evaluate-polynomial a1 z))
						       (p2 (* z (evaluate-polynomial a2 z)))
						       (p (+ (* y p1) p2)))
						  (+ (- (log x))
						     (- p (* 1/2 y)))))
		    ((< x 1.2316322326660156d0) (let* ((y (1- x))
						       (p1      (* y (evaluate-polynomial u y)))
						       (p2  (1+ (* y (evaluate-polynomial v y)))))
						  (+ (* -1/2 y)
						     (/ p1 p2))))
		    ((< x 1.7316312789916992d0) (let* ((y (- x tc))
						       (z (square y))
						       (w (* z y))
						       (p1 (evaluate-polynomial t1 w))
						       (p2 (evaluate-polynomial t2 w))
						       (p3 (evaluate-polynomial t3 w))
						       (p  (- (* z p1)
							      (- tt
								 (* w
								    (+ p2
								       (* y p3)))))))
						  (+ tf p)))
		    ((< x 2d0) (let* ((y (- 2 x))
				      (z (square y))
				      (p1      (evaluate-polynomial a1 z))
				      (p2 (* z (evaluate-polynomial a2 z)))
				      (p (+ (* y p1) p2)))
				 (- p (* 1/2 y))))
		    ((< x 8d0) (let* ((y (nth-value 1 (truncate x)))
				      (p     (* y (evaluate-polynomial s y)))
				      (q (1+ (* y (evaluate-polynomial r y))))
				      (r (+ (* 1/2 y)
					    (/ p q)))
				      (z (loop
					   with k = 1
					   for i downfrom (truncate x) to 3
					   :do (multf k (+ y (1- i)))
					   :finally (return k))))
				 (+ r (log z))))
		    ((< x two58) (let* ((val (log x))
					(z (/ x))
					(y (square z))
					(w (+ w0 (* z (evaluate-polynomial wc y)))))
				   (+
				    (* (- x half)
				       (- val one))
				    w)))
		    (t (* x (1- (log x)))))))
      (if (minusp n) (- nadj r) r))))

