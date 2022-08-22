;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: SPECFUN -*-
;;; Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
;;; Copyright (c) 2019-2022 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:special-functions)

;;; Double-float error and related functions.

#| References
[1] Boost C++ Libraries, Release 1.69 (2020) http://www.boost.org/
[2] Openlibm mathematical library, Release 0.7.3 (2020) https://openlibm.org/
[3] DAMath, https://github.com/CMCHTPC/ChromaPrint
|#

;;;
;;; inverse-erf/c, reference Boost[1] erv_inv.hpp
;;;
(let ((p-one-half (make-array 8
			      :initial-contents '(-0.00538772965071242932965d0
						  0.00822687874676915743155d0
						  0.0219878681111168899165d0
						  -0.0365637971411762664006d0
						  -0.0126926147662974029034d0
						  0.0334806625409744615033d0
						  -0.00836874819741736770379d0
						  -0.000508781949658280665617d0)
			      :element-type 'double-float))

      (q-one-half (make-array 10
			      :initial-contents '(0.000886216390456424707504d0
						  -0.00233393759374190016776d0
						  0.0795283687341571680018d0
						  -0.0527396382340099713954d0
						  -0.71228902341542847553d0
						  0.662328840472002992063d0
						  1.56221558398423026363d0
						  -1.56574558234175846809d0
						  -0.970005043303290640362d0
						  1.0d0)
			      :element-type 'double-float))

      (p-one-quarter (make-array 9
				 :initial-contents '(-3.67192254707729348546d0
						     21.1294655448340526258d0
						     17.445385985570866523d0
						     -44.6382324441786960818d0
						     -18.8510648058714251895d0
						     17.6447298408374015486d0
						     8.37050328343119927838d0
						     0.105264680699391713268d0
						     -0.202433508355938759655d0)
				 :element-type 'double-float))

      (q-one-quarter (make-array 9
				 :initial-contents '(1.72114765761200282724d0
						     -22.6436933413139721736d0
						     10.8268667355460159008d0
						     48.5609213108739935468d0
						     -20.1432634680485188801d0
						     -28.6608180499800029974d0
						     3.9713437953343869095d0
						     6.24264124854247537712d0
						     1.0d0)
				 :element-type 'double-float))

      (p3 (make-array 11
		      :initial-contents '(-0.681149956853776992068d-9
					  0.285225331782217055858d-7
					  -0.679465575181126350155d-6
					  0.00214558995388805277169d0
					  0.0290157910005329060432d0
					  0.142869534408157156766d0
					  0.337785538912035898924d0
					  0.387079738972604337464d0
					  0.117030156341995252019d0
					  -0.163794047193317060787d0
					  -0.131102781679951906451d0)
		      :element-type 'double-float))

      (q3 (make-array 8
		      :initial-contents '(0.01105924229346489121d0
					  0.152264338295331783612d0
					  0.848854343457902036425d0
					  2.59301921623620271374d0
					  4.77846592945843778382d0
					  5.38168345707006855425d0
					  3.46625407242567245975d0
					  1.0d0)
		      :element-type 'double-float))

      (p6 (make-array 9
		      :initial-contents '(0.266339227425782031962d-11
					  -0.230404776911882601748d-9
					  0.460469890584317994083d-5
					  0.000157544617424960554631d0
					  0.00187123492819559223345d0
					  0.00950804701325919603619d0
					  0.0185573306514231072324d0
					  -0.00222426529213447927281d0
					  -0.0350353787183177984712d0)
		      :element-type 'double-float))

      (q6 (make-array 7
		      :initial-contents '(0.764675292302794483503d-4
					  0.00263861676657015992959d0
					  0.0341589143670947727934d0
					  0.220091105764131249824d0
					  0.762059164553623404043d0
					  1.3653349817554063097d0
					  1.0d0)
		      :element-type 'double-float))

      (p18 (make-array 9
		       :initial-contents '(0.99055709973310326855d-16
					   -0.281128735628831791805d-13
					   0.462596163522878599135d-8
					   0.449696789927706453732d-6
					   0.149624783758342370182d-4
					   0.000209386317487588078668d0
					   0.00105628862152492910091d0
					   -0.00112951438745580278863d0
					   -0.0167431005076633737133d0)
		       :element-type 'double-float))

      (q18 (make-array 7
		       :initial-contents '(0.282243172016108031869d-6
					   0.275335474764726041141d-4
					   0.000964011807005165528527d0
					   0.0160746087093676504695d0
					   0.138151865749083321638d0
					   0.591429344886417493481d0
					   1.0d0)
		       :element-type 'double-float))


      (p44 (make-array 8
		       :initial-contents '(-0.116765012397184275695d-17
					   0.145596286718675035587d-11
					   0.411632831190944208473d-9
					   0.396341011304801168516d-7
					   0.162397777342510920873d-5
					   0.254723037413027451751d-4
					   -0.779190719229053954292d-5
					   -0.0024978212791898131227d0)
		       :element-type 'double-float))

      (q44 (make-array 7
		       :initial-contents '(0.509761276599778486139d-9
					   0.144437756628144157666d-6
					   0.145007359818232637924d-4
					   0.000690538265622684595676d0
					   0.0169410838120975906478d0
					   0.207123112214422517181d0
					   1.0d0)
		       :element-type 'double-float))

      (p44+ (make-array 8
			 :initial-contents '(-0.348890393399948882918d-21
					     0.135880130108924861008d-14
					     0.947846627503022684216d-12
					     0.225561444863500149219d-9
					     0.229345859265920864296d-7
					     0.899465114892291446442d-6
					     -0.28398759004727721098d-6
					     -0.000539042911019078575891d0)
			 :element-type 'double-float))

      (q44+ (make-array 7
			 :initial-contents '(0.231558608310259605225d-11
					     0.161809290887904476097d-8
					     0.399968812193862100054d-6
					     0.468292921940894236786d-4
					     0.00282092984726264681981d0
					     0.0845746234001899436914d0
					     1d0)
			 :element-type 'double-float)))


  (defun inverse-error (p q)
    "Return value of inverse error function: erf_inv(p) if p <= 0.5, erfc_inv(q) otherwise"
    (declare (double-float p q))
    (let ((x (sqrt (- (log q)))))
      (cond ((<= p 0.5) (let ((y 747689/8388608)    ; 0.0891314744949340820313d0
			     (g (* p (+ p 10)))
			     (r (/ (evaluate-polynomial p-one-half p)
				   (evaluate-polynomial q-one-half p))))
			 (+ (* g y)
			    (* g r))))
	    ((>= q 0.25) (let* ((y 73711/32768) ; 2.249481201171875d0
			       (g (sqrt (* -2 (log q))))
			       (xs (- q 0.25))
			       (r (/ (evaluate-polynomial p-one-quarter xs)
				     (evaluate-polynomial q-one-quarter xs))))
			  (/ g (+ y r))))
	    ((< x 3) (let* ((y 26451/32768) ; 0.807220458984375d0
			    (xs (- x 1.125d0))
			    (r (/ (evaluate-polynomial p3 xs)
				  (evaluate-polynomial q3 xs))))
		       (+ (* y x)
			  (* r x))))
	    ((< x 6) (let* ((y 985615/1048576) ; 0.93995571136474609375d0
			    (xs (- x 3))
			    (r (/ (evaluate-polynomial p6 xs)
				  (evaluate-polynomial q6 xs))))
		       (+ (* y x)
			  (* r x))))
	    ((< x 18) (let* ((y 1031409/1048576) ; 0.98362827301025390625d0
			    (xs (- x 6))
			    (r (/ (evaluate-polynomial p18 xs)
				  (evaluate-polynomial q18 xs))))
		       (+ (* y x)
			  (* r x))))
	    ((< x 44) (let* ((y 1045583/1048576) ; 0.99714565277099609375d0
			     (xs (- x 18))
			     (r (/ (evaluate-polynomial p44 xs)
				   (evaluate-polynomial q44 xs))))
			(+ (* y x)
			   (* r x))))
	    (t (let* ((y 1047961/1048576) ; 0.99941349029541015625d0
			     (xs (- x 44))
			     (r (/ (evaluate-polynomial p44+ xs)
				   (evaluate-polynomial q44+ xs))))
			(+ (* y x)
			   (* r x))))))))

(defun inverse-erf (x)
  "Return the inverse function of erf: (erf (inverse-erf x)) = x, -1 < x < 1"
  (declare (double-float x))
  (cond ((float-nan-p x) x)
	((or (< x -1) ; Need to sort out how to handle NaN, instead, throw error
	     (> x 1)) (error "x must be in the range [-1 1]"))
	((= x 1)  double-float-positive-infinity)
	((= x -1) double-float-negative-infinity)
	((= x 0) 0)
	((< x 0) (let* ((p (- x))
			(q (- 1 p)))
		   (- (inverse-error p q))))
	(t (let* ((p x)
		  (q (- 1 x)))
	     (inverse-error p q)))))


(defun inverse-erfc (x)
  "Return the inverse function of erfc: (erfc (inverse-erfc x)) = x, 0 < x < 2"
  (declare (double-float x))
  (cond ((float-nan-p x) (signal 'floating-point-invalid-operation))
	((or (< x 0) ; Need to sort out how to handle NaN.
	     (> x 2)) (error "x must be in the range [0 2])"))
	((= x 0) double-float-positive-infinity)
	((= x 2) double-float-negative-infinity)
	((> x 1) (let* ((q (- 2 x))
			(p (- 1 q))
			(ans (- (inverse-error p q))))
		   (if (float-nan-p ans)
		       (signal 'floating-point-invalid-operation)
		       ans)))
	(t (let* ((p (- 1 x))
		  (q x)
		  (ans (inverse-error p q)))
		   (if (float-nan-p ans)
		       (signal 'floating-point-invalid-operation)
		       ans)))))



#| Port of erf/c from openlibm [2]
See: https://github.com/JuliaMath/openlibm/blob/master/src/s_erf.c
			     x
		      2      |\
     erf(x)  =  ---------  | exp(-t*t)dt
	 	   sqrt(pi) \|
			     0

     erfc(x) =  1-erf(x)
  Note that
		erf(-x) = -erf(x)
		erfc(-x) = 2 - erfc(x)

 Method:
	1. For |x| in [0, 0.84375]
	    erf(x)  = x + x*R(x^2)
          erfc(x) = 1 - erf(x)           if x in [-.84375,0.25]
                  = 0.5 + ((0.5-x)-x*R)  if x in [0.25,0.84375]
	   where R = P/Q where P is an odd poly of degree 8 and
	   Q is an odd poly of degree 10.
						 -57.90
			| R - (erf(x)-x)/x | <= 2


	   Remark. The formula is derived by noting
          erf(x) = (2/sqrt(pi))*(x - x^3/3 + x^5/10 - x^7/42 + ....)
	   and that
          2/sqrt(pi) = 1.128379167095512573896158903121545171688
	   is close to one. The interval is chosen because the fix
	   point of erf(x) is near 0.6174 (i.e., erf(x)=x when x is
	   near 0.6174), and by some experiment, 0.84375 is chosen to
 	   guarantee the error is less than one ulp for erf.

      2. For |x| in [0.84375,1.25], let s = |x| - 1, and
         c = 0.84506291151 rounded to single (24 bits)
         	erf(x)  = sign(x) * (c  + P1(s)/Q1(s))
         	erfc(x) = (1-c)  - P1(s)/Q1(s) if x > 0
			  1+(c+P1(s)/Q1(s))    if x < 0
         	|P1/Q1 - (erf(|x|)-c)| <= 2**-59.06
	   Remark: here we use the taylor series expansion at x=1.
		erf(1+s) = erf(1) + s*Poly(s)
			 = 0.845.. + P1(s)/Q1(s)
	   That is, we use rational approximation to approximate
			erf(1+s) - (c = (single)0.84506291151)
	   Note that |P1/Q1|< 0.078 for x in [0.84375,1.25]
	   where
		P1(s) = degree 6 poly in s
		Q1(s) = degree 6 poly in s

      3. For x in [1.25,1/0.35(~2.857143)],
         	erfc(x) = (1/x)*exp(-x*x-0.5625+R1/S1)
         	erf(x)  = 1 - erfc(x)
	   where
		R1(z) = degree 7 poly in z, (z=1/x^2)
		S1(z) = degree 8 poly in z

      4. For x in [1/0.35,28]
         	erfc(x) = (1/x)*exp(-x*x-0.5625+R2/S2) if x > 0
			= 2.0 - (1/x)*exp(-x*x-0.5625+R2/S2) if -6<x<0
			= 2.0 - tiny		(if x <= -6)
         	erf(x)  = sign(x)*(1.0 - erfc(x)) if x < 6, else
         	erf(x)  = sign(x)*(1.0 - tiny)
	   where
		R2(z) = degree 6 poly in z, (z=1/x^2)
		S2(z) = degree 7 poly in z

      Note1:
	   To compute exp(-x*x-0.5625+R/S), let s be a single
	   precision number and s := x; then
		-x*x = -s*s + (s-x)*(s+x)
	        exp(-x*x-0.5626+R/S) =
			exp(-s*s-0.5625)*exp((s-x)*(s+x)+R/S);
      Note2:
	   Here 4 and 5 make use of the asymptotic series
			  exp(-x*x)
		erfc(x) ~ ---------- ( 1 + Poly(1/x^2) )
			  x*sqrt(pi)
	   We use rational approximation to approximate
      	g(s)=f(1/x^2) = log(erfc(x)*x) - x*x + 0.5625
	   Here is the error bound for R1/S1 and R2/S2
      	|R1/S1 - f(x)|  < 2**(-62.57)
      	|R2/S2 - f(x)|  < 2**(-61.52)

      5. For inf > x >= 28
         	erf(x)  = sign(x)(1 - tiny)  (raise inexact)
         	erfc(x) = tiny*tiny (raise underflow) if x > 0
			= 2 - tiny if x<0

      7. Special case:
         	erf(0)  = 0, erf(inf)  = 1, erf(-inf) = -1,
         	erfc(0) = 1, erfc(inf) = 0, erfc(-inf) = 2,
	   	erfc/erf(NaN) is NaN
|#

(let (;(tiny 1d-300)
      (very-tiny 2.848094538889218D-306) ;; 0x0080000000000000
      (small (expt 2.0d0 -28))

      (erx  8.45062911510467529297d-01) ; 0x3FEB0AC1, 0x60000000
      (efx   1.28379167095512586316d-01) ; 0x3FC06EBA, 0x8214DB69
      (efx8  1.02703333676410069053d+00) ; 0x3FF06EBA, 0x8214DB69


      ;; Coefficients for approximation to erf on [0,0.84375]
      ;; Polynomials are ordered from the highest degree down to the constant term

      (pp (make-array 5
		      :initial-contents '(-2.37630166566501626084d-05
					  -5.77027029648944159157d-03
					  -2.84817495755985104766d-02
					  -3.25042107247001499370d-01
					   1.28379167095512558561d-01)
		      :element-type 'double-float))
      (qq (make-array 6
		      :initial-contents '(-3.96022827877536812320d-06
					  1.32494738004321644526d-04
					  5.08130628187576562776d-03
					  6.50222499887672944485d-02
					  3.97917223959155352819d-01
					  0d0) ; constant term
		      :element-type 'double-float))

      ;; Coefficients for approximation to erf  in [0.84375,1.25]

      (pa (make-array 7
		      :initial-contents '(-2.16637559486879084300d-03
					   3.54783043256182359371d-02
					  -1.10894694282396677476d-01
					   3.18346619901161753674d-01
					  -3.72207876035701323847d-01
					   4.14856118683748331666d-01
					  -2.36211856075265944077d-03)
		      :element-type 'double-float))
      (qa (make-array 7
		      :initial-contents '(1.19844998467991074170d-02
					  1.36370839120290507362d-02
					  1.26171219808761642112d-01
					  7.18286544141962662868d-02
					  5.40397917702171048937d-01
					  1.06420880400844228286d-01
					  0d0) ; constant term
		      :element-type 'double-float))


      ;; Coefficients for approximation to erfc in [1.25,1/0.35]

      (ra (make-array 8
		      :initial-contents '(-9.81432934416914548592d+00
					  -8.12874355063065934246d+01
					  -1.84605092906711035994d+02
					  -1.62396669462573470355d+02
					  -6.23753324503260060396d+01
					  -1.05586262253232909814d+01
					  -6.93858572707181764372d-01
					  -9.86494403484714822705d-03)
		      :element-type 'double-float))
      (sa (make-array 9
		      :initial-contents '(-6.04244152148580987438d-02
					   6.57024977031928170135d+00
					   1.08635005541779435134d+02
					   4.29008140027567833386d+02
					   6.45387271733267880336d+02
					   4.34565877475229228821d+02
					   1.37657754143519042600d+02
					   1.96512716674392571292d+01
					   1d0)
		      :element-type 'double-float))

      ;; Coefficients for approximation to erfc in [1/.35,28]

      (rb (make-array 7
		      :initial-contents '(-4.83519191608651397019d+02
					  -1.02509513161107724954d+03
					  -6.37566443368389627722d+02
					  -1.60636384855821916062d+02
					  -1.77579549177547519889d+01
					  -7.99283237680523006574d-01
					  -9.86494292470009928597d-03)
		      :element-type 'double-float))
      (sb (make-array 8
		      :initial-contents '(-2.24409524465858183362d+01
					  4.74528541206955367215d+02
					  2.55305040643316442583d+03
					  3.19985821950859553908d+03
					  1.53672958608443695994d+03
					  3.25792512996573918826d+02
					  3.03380607434824582924d+01
					  1d0)
		      :element-type 'double-float)))

  (defun erf (n) ; Should this be a generic function to handle the case of complex arguments?
    "Returns the error function of n"
    (declare (double-float n))
;;    (if (float-nan-p n) (return-from erf :not-a-number)) ; Waiting for float-features issue #10 to be resolved.
    (let ((x (abs n)))
      (* (float-sign n)
	 (cond ((eql n double-float-positive-infinity)  1)
	       ((eql n double-float-negative-infinity) -1)
	       ((< x very-tiny) (* 0.125 (+ (* 8 x) (* x efx8)))) ; avoid underflow
	       ((< x small)     (+ x (* x efx)))
	       ((< x 0.84375)   (let ((z (* x x)))
				  (+ x (* x (/ (evaluate-polynomial pp z)
					       (1+ (evaluate-polynomial qq z)))))))
	       ((< x 1.25)      (let* ((s (1- x))
				       (p (evaluate-polynomial pa s))
				       (q (1+ (evaluate-polynomial qa s))))
				  (+  erx (/ p q))))
	       ((< x (/ 1 0.35d0)) (let* ((s (/ 1 (* x x))) ; |x| < 1 / 0.35  ~ 2.857143
					  (r (evaluate-polynomial ra s))
					  (i (evaluate-polynomial sa s)) ; The openlibm code reuses same variable with different case
					  (mask #xffffffff00000000) ; bit mask to zero lower word
					  (z (decode-float64 (logand (encode-float64 x) mask)))
					  (j (* (exp (- (* (- z) z)
							0.5625))
						(exp (+ (* (- z x) (+ z x))
							(/ r i))))))
  				     (- (1- (/ j x))))) ; Note negation. This algo handles signs inconsistently
	       ((< x 6) (let* ((s (/ 1 (* x x)))  ; 2.857143 < |x| < 6.0
			       ;; Code duplication noted but retained to avoid function call overhead
			       (r (evaluate-polynomial rb s))
			       (i (evaluate-polynomial sb s))
			       (mask #xffffffff00000000) ; bit mask to zero lower word
			       (z (decode-float64 (logand (encode-float64 x) mask)))
			       (j (* (exp (- (* (- z) z)
					     0.5625))
				     (exp (+ (* (- z x) (+ z x))
					     (/ r i))))))
			  (- (1- (/ j x)))))
	       ((>= x 6) 1)))))

  )					;let


;;;
;;; Complement of erf
;;;

;;; From Ehrhardt [3], in turn derived from Cephes

(defun erfc-scaled (x)
  "p/q := exp(x^2)*erfc(x), 1<=x<=128"
  (declare (double-float x))
  (let (;;erfc(x) = exp(-x^2)*P(1/x)/Q(1/x), 1/8<=1/x<=1, Peak relative error 5.8e-21
	(pp (make-array 10
			:initial-contents '(1.130609921802431462353D+9
					    2.290171954844785638925D+9
					    2.295563412811856278515D+9
					    1.448651275892911637208D+9
					    6.234814405521647580919D+8
					    1.870095071120436715930D+8
					    3.833161455208142870198D+7
					    4.964439504376477951135D+6
					    3.198859502299390825278D+5
					    -9.085943037416544232472D-6)
			:element-type 'double-float))
	(qq (make-array 11
			:initial-contents '(1d0
					    1.130609910594093747762D9
					    3.565928696567031388910D9
					    5.188672873106859049556D9
					    4.588018188918609726890D9
					    2.729005809811924550999D9
					    1.138778654945478547049D9
					    3.358653716579278063988D8
					    6.822450775590265689648D7
					    8.799239977351261077610D6
					    5.669830829076399819566D5)
			:element-type 'double-float))

	;; erfc(x) = exp(-x^2)*1/x*R(1/x^2)/S(1/x^2), 1/128<=1/x<1/8, Peak relative error 1.9e-21
	(rr (make-array 5
			:initial-contents '(3.621349282255624026891D0
					    7.173690522797138522298D0
					    3.445028155383625172464D0
					    5.537445669807799246891D-1
					    2.697535671015506686136D-2)
			:element-type 'double-float))
	(ss (make-array 6
			:initial-contents '(1d0
					    1.072884067182663823072D1
					    1.533713447609627196926D1
					    6.572990478128949439509D0
					    1.005392977603322982436D0
					    4.781257488046430019872D-2)
			:element-type 'double-float))
	(y (/ x))
	p q)
    (if (< x 8)
	(setf p (evaluate-polynomial pp y)
	      q (evaluate-polynomial qq y))
	(setf q (square y)
	      p (* y
		   (evaluate-polynomial rr q))
	      q (evaluate-polynomial ss q)))
    (values p q)))

(defun erfc (x)
  "Return the complementary error function erfc(x) = 1-erf(x)"
  (declare (double-float x))
  (let ((a (abs x))
	p q z)
     (cond ((eql x double-float-positive-infinity) 0d0)
	   ((eql x double-float-negative-infinity) 2d0)
	   ((< a 1) (- 1 (erf x)))
	   ((< x -7) 2d0)
	   ((> x 106.8) 0d0)
	   (t ; -7 < x < -1 OR 1 < x < 107
	    (multiple-value-setq (p q) (erfc-scaled a))
	    (setf z (exp (* a (- a)))
		  z (/ (* z p)
		       q))
	    (if (>= x 0)
		z
		(- 2 z))))))


