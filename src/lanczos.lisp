;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; -*-
;;; Package: LANCZOS ; Remove package file attribute to try and prevent Genera breakage
;;; Copyright (c) 2021 by Symbolics Pte. Ltd. All rights reserved.

(uiop:define-package #:lanczos
    (:use #:cl))
(in-package #:lanczos)

;;; Implementations of Lanczos approximations

#| Derived from Boost's Lanczos.hpp, copyright John Maddock 2006
https://www.boost.org/doc/libs/1_76_0/boost/math/special_functions/lanczos.hpp
https://www.boost.org/doc/libs/1_64_0/libs/math/doc/html/math_toolkit/lanczos.html
https://www.boost.org/doc/libs/1_76_0/boost/math/tools/rational.hpp

For the most part we focus on double-float, but lay the groundwork
for long-float or single-float

Optimal values for G for each N are taken from
http://web.mala.bc.ca/pughg/phdThesis/phdThesis.pdf, as are the
theoretical error bounds.

Constants calculated using the method described by Godfrey
http://my.fit.edu/~gabdo/gamma.txt and elaborated by Toth at
http://www.rskey.org/gamma.htm using NTL::RR at 1000 bit precision. |#


;;; From Boost
;;; Lanczos Coefficients for N=13 G=6.024680040776729583740234375
;;; Max experimental error (with arbitrary precision arithmetic) 1.196214e-17
;;; Generated with compiler: Microsoft Visual C++ version 8.0 on Win32 at Mar 23 2006

;;; These rational numbers are the 'rationalized' long-doubles, done
;;; on CLISP.  See the end of this file for the CLISP code for the
;;; coefficient conversion.

(defparameter n 13)
(defparameter g 808618867/134217728) ;6.024680040776729583740234375L0
(defparameter g-1/2 (- g (/ 2)))

(defparameter lanczos-13-numerator
  (make-array 13
	      :initial-contents '(2353137688041075968857200767445163675473/100000000000000000000000000000
				  4291980364264909876895789904700198885093/100000000000000000000000000000
				  892798980933891701236004636288679167649/25000000000000000000000000000
				  1792103442603720969991975575445893111267/100000000000000000000000000000
				  603954258635202800506429164430729792107/100000000000000000000000000000
				  359930101827930418415805768198728098493/250000000000000000000000000000
				  2488745578620541565114603864132294232163/10000000000000000000000000000000
				  3142641558540019438061423162831820536287/100000000000000000000000000000000
				  2876370628935372441225409051620849613599/1000000000000000000000000000000000
				  930281326976117475201474948580228496411/5000000000000000000000000000000000
				  4035836001182908105319001451136125306911/500000000000000000000000000000000000
				  2108242777515793458725097339207133627117/10000000000000000000000000000000000000
				  1253314137315500135082454088566918669313/500000000000000000000000000000000000000)
	      :element-type 'rational))

(defparameter lanczos-13-numerator-scaled
  (make-array 13
	      :initial-contents '(5690652191347156388090791033559122686859/100000000000000000000000000000000
				  518970215581722725953135526808035119277/5000000000000000000000000000000
				  4318156564406929572773463644488934211171/50000000000000000000000000000000
				  866777786493522766954744748118106663217/20000000000000000000000000000000
				  1460557808768506808414169982791359218571/100000000000000000000000000000000
				  87042803874516147720517754741193639117/25000000000000000000000000000000
				  6018596171681098786670226533699352302507/10000000000000000000000000000000000
				  1899982326003635662468825860899727284273/25000000000000000000000000000000000
				  3477999801257688070178155057757599493763/500000000000000000000000000000000000
				  4499445569063168119446858607650988409623/10000000000000000000000000000000000000
				  121999549265476092677991310389728258513/6250000000000000000000000000000000000
				  5098416655656676188125178644804694509993/10000000000000000000000000000000000000000
				  3030921173124453262891876982277968441611/500000000000000000000000000000000000000000)
	      :element-type 'rational))

(defparameter lanczos-13-denominator
  (make-array 13
	      :initial-contents '(0
				  39916800
				  120543840
				  150917976
				  105258076
				  45995730
				  13339535
				  2637558
				  357423
				  32677
				  1925
				  66
				  1)
	      :element-type 'rational))






;;; From Boost
;;; Lanczos Coefficients for N=13 G=6.024680040776729583740234375
;;; Max experimental error (with arbitrary precision arithmetic) 1.196214e-17
;;; Generated with compiler: Microsoft Visual C++ version 8.0 on Win32 at Mar 23 2006
;;; These double coefficients only give 5 decimal places of accuracy.  Need long-double.
#|
(defparameter n 13)
(defparameter g 6.024680040776729583740234375d0) ;this is what Cephes uses
(defparameter g-1/2 (- g 0.5d0))
(defparameter lanczos-13-numerator (make-array 13
					       :initial-contents '(2.353137688041076d10
								   4.29198036426491d10
								   3.571195923735567d10
								   1.792103442603721d10
								   6.039542586352028d9
								   1.4397204073117216d9
								   2.4887455786205417d8
								   3.1426415585400194d7
								   2876370.6289353725d0
								   186056.26539522348d0
								   8071.672002365816d0
								   210.82427775157936d0
								   2.5066282746310002d0)
				     :element-type 'double-float))

(defparameter lanczos-13-numerator-scaled (make-array 13
						      :initial-contents '(5.6906521913471565d7
									  1.0379404311634454d8
									  8.63631312881386d7
									  4.333888932467614d7
									  1.4605578087685067d7
									  3481712.154980646d0
									  601859.6171681099d0
									  75999.29304014542d0
									  6955.999602515376d0
									  449.9445569063168d0
									  19.519927882476175d0
									  0.5098416655656676d0
									  0.006061842346248907d0)
				     :element-type 'double-float))

(defparameter lanczos-13-denominator (make-array 13
						 :initial-contents '(0.0d0
								     3.99168d7
								     1.2054384d8
								     1.50917976d8
								     1.05258076d8
								     4.599573d7
								     1.3339535d7
								     2637558.0d0
								     357423.0d0
								     32677.0d0
								     1925.0d0
								     66.0d0
								     1.0d0)
						 :element-type 'double-float))
|#



;;; TODO: Move to num-utils, horner.lisp
;;; TODO The denominator/numerator should not be reversed, as we're using a loop instead of evaluate-rational
;;; See https://www.boost.org/doc/libs/1_68_0/libs/math/doc/html/math_toolkit/tuning.html
;;; https://en.wikipedia.org/wiki/Rational_function
;;; See: https://www.boost.org/doc/libs/1_76_0/boost/math/tools/rational.hpp"

(defun evaluate-rational (numerator denominator z)
  "Evaluate a rational function using Horner's method. Numerator and denominator must be equal in size.  These always have a loop and so may be less efficient than evaluating a pair of polynomials.  However, there are some tricks we can use to prevent overflow that might otherwise occur in polynomial evaluation if z is large.  This is important in our Lanczos code for example.

N.B. The order of coefficients for this function is NOT the same as evaluate-polynomial.  Here, it is from the constant term up to the highest order polynomial."
  ;; (declare (double-float z))
  (assert (= (length numerator)
	     (length denominator)) () "Numerator and denominator must be the same length")
  (let (s1 s2)
    (if (<= z 1)
	(progn
	  (setf s1 (aref numerator   (1- (length numerator)))
		s2 (aref denominator (1- (length denominator))))
	  (loop for i from (- (length numerator) 2) downto 0
		do  (setf s1 (* s1 z)
			  s1 (+ s1 (aref numerator i))
			  s2 (* s2 z)
			  s2 (+ s2 (aref denominator i)))))
	(progn
	  (setf z (/ z)
		s1 (aref numerator 0)
		s2 (aref denominator 0))
	  (loop for i from 1 below (length numerator)
		do (setf s1 (* s1 z)
			  s1 (+ s1 (aref numerator i))
			  s2 (* s2 z)
			  s2 (+ s2 (aref denominator i)))
		)))
    (/ s1 s2)))


(defun lanczos-sum (x &key (scaled t))
  "Return the Lanczos sum for x, exp(g), possibly normalised"
 ;; (declare (double-float x))
  (if scaled
      (evaluate-rational lanczos-13-numerator-scaled lanczos-13-denominator x)
      (evaluate-rational lanczos-13-numerator lanczos-13-denominator x)))


;;; This CFFI wrapper to Cephes rational polynomial function is for testing.

;;; Cephes polevl.h
;;; static double ratevl(double x, const double num[], int M, const double denom[], int N)
;;; Use this to check our evaluate-rational function above
;;; https://mov.im/?blog/phoe%40movim.eu/cffi-arrays-versus-static-vectors-a-comparison-SCutJQ
;;; https://github.com/tpapp/ffa/ -- perhaps static-vectors are better now? They do require special handling themselves though (static-v)
;;; https://cffi-devel.common-lisp.narkive.com/8wo9G5be/offer-of-code-contribution-with-array-as-foreign-pointer

;;; Trying to reproduce cephes:lanczos-sum-scaled After that works, we
;;; can ensure evaluate-rational is working properly.  We also need
;;; evaluate-rational for aux-gamma in gamma.lisp (q-taylor)
;;; LS-USER> (cephes:lanczos-sum-scaled 0.542d0)
;;; 1.5846062851718752d0
#+nil
(cffi:with-foreign-objects ((num   :double 13)
			    (denom :double 13))
  (cffi:lisp-array-to-foreign lanczos::lanczos-13-numerator-scaled num   '(:array :double 13))
  (cffi:lisp-array-to-foreign lanczos::lanczos-13-denominator      denom '(:array :double 13))
  (cephes::evlrat 0.542d0
		  num
		  (1- (length lanczos::lanczos-13-numerator-scaled))
		  denom
		  (1- (length lanczos::lanczos-13-denominator))
		  ))

#| From cephes lanczos.c
double lanczos_sum_expg_scaled(double x)
{
  return ratevl(x, lanczos_sum_expg_scaled_num,
		sizeof(lanczos_sum_expg_scaled_num) / sizeof(lanczos_sum_expg_scaled_num[0]) - 1,
		lanczos_sum_expg_scaled_denom,
		sizeof(lanczos_sum_expg_scaled_denom) / sizeof(lanczos_sum_expg_scaled_denom[0]) - 1);
}
|#

#+nil
(defun create-foreign-array (type contents) ; we don't really need this, and it doesn't free the memory
  "Allocate a foreign array and fill it with the contents of a Lisp array."
  (let* ((length (length contents))
         (array (foreign-alloc type :count length)))
    (lisp-array-to-foreign contents array
                           `(:array ,type ,length))
    array))





;;;
;;; CLISP Code for generating rational coefficients
;;;


;;; Set high precision floats (1000 digits)
;;(SETF (EXT:LONG-FLOAT-DIGITS) 3322)

;;; These coefficients came from Cephes, so need to be reversed
;;; because we took our implementation of evaluate-rational from
;;; Boost.

;;; Unscaled numerator
;;(map 'list #'rationalize '(2.506628274631000270164908177133837338626L0 210.8242777515793458725097339207133627117L0 8071.672002365816210638002902272250613822L0 186056.2653952234950402949897160456992822L0 2876370.628935372441225409051620849613599L0 31426415.58540019438061423162831820536287L0 248874557.8620541565114603864132294232163L0 1439720407.311721673663223072794912393972L0 6039542586.35202800506429164430729792107L0 17921034426.03720969991975575445893111267L0 35711959237.35566804944018545154716670596L0 42919803642.64909876895789904700198885093L0 23531376880.41075968857200767445163675473L0))

;;; Scaled numerator
;(map 'list #'rationalize '(0.006061842346248906525783753964555936883222L0 0.5098416655656676188125178644804694509993L0 19.51992788247617482847860966235652136208L0 449.9445569063168119446858607650988409623L0 6955.999602515376140356310115515198987526L0 75999.29304014542649875303443598909137092L0 601859.6171681098786670226533699352302507L0 3481712.15498064590882071018964774556468L0 14605578.08768506808414169982791359218571L0 43338889.32467613834773723740590533316085L0 86363131.28813859145546927288977868422342L0 103794043.1163445451906271053616070238554L0 56906521.91347156388090791033559122686859L0))

;;; Denominator
;(map 'list #'rationalize '(0.0d0 3.99168d7 1.2054384d8 1.50917976d8 1.05258076d8 4.599573d7 1.3339535d7 2637558.0d0 357423.0d0 32677.0d0 1925.0d0 66.0d0 1.0d0))
