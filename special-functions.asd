;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2021-2020 by Symbolics Pte. Ltd. All rights reserved.

(asdf:defsystem #:special-functions
  :description "Special functions in Common Lisp"
  :author      "Steve Nunez <steve@symbolics.tech>"
  :license     :MS-PL
  :version     (:read-file-form "version.sexp")
  :serial      t
  :depends-on  (:num-utils :float-features)
  :pathname    "src"
  :components ((:file #:pkgdcl)
	       (:file #:utils)
	       (:file #:erf)
	       (:file #:gamma)
	       (:file #:log-gamma))
  :in-order-to ((test-op (test-op special-functions/tests))))


(asdf:defsystem #:special-functions/tests
  :description "Tests for special functions"
  :version      (:read-file-form "version.sexp")
  :author "Steven Nunez <steve@symbolics.tech>"
  :license :MS-PL
  :depends-on (#:special-functions

	       ;; Test tools
	       #:cl-variates
	       #:fiveam
	       #:select)
  :serial t
  :components ((:file #:tests/tpkgdcl)
	       (:module data
		:pathname tests/data
		:components ((:file #:prologue)
			     (:file #:erf-small-data)
			     (:file #:erf-medium-data)
			     #-allegro (:file #:erf-large-data) ; ACL raises underflow condition
			     (:file #:erf-inverse-data)
			     (:file #:erfc-inverse-data)
			     #-allegro (:file #:erfc-inverse-small-data)
			     (:file #:gamma-data)
			     (:file #:epilogue)))
	       (:module tests
		:components (
			     (:file #:main)
			     (:file #:erf-test)
			     (:file #:gamma-test))))
  :perform (asdf:test-op (o s)
			 (uiop:symbol-call :fiveam '#:run!
					   (uiop:find-symbol* '#:all-tests
							      :special-functions-tests))))
