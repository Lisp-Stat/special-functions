;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2020-2022 by Symbolics Pte. Ltd. All rights reserved.

(asdf:defsystem #:special-functions
  :version     "1.2.0"
  :license     :MS-PL
  :author      "Steve Nunez <steve@symbolics.tech>"
  :long-name   "Mathematical Special Functions"
  :description "Special functions in Common Lisp"
  :long-description  #.(uiop:read-file-string
			(uiop:subpathname *load-pathname* "description.text"))
  ;:homepage    "https://lisp-stat.dev/docs/manuals/special-functions/"
  :source-control (:git "https://github.com/Lisp-Stat/special-functions.git")
  :bug-tracker "https://github.com/Lisp-Stat/special-functions/issues"

  :serial      t
  :depends-on  (:num-utils :float-features :let-plus)
  :pathname    "src"
  :components ((:file #:pkgdcl)
	       (:file #:utils)
	       (:file #:erf)
	       (:file #:lanczos)
	       (:file #:gamma)
	       (:file #:log-gamma)
	       (:file #:factorial))
  :in-order-to ((test-op (test-op special-functions/tests))))


(asdf:defsystem #:special-functions/tests
  :version     "1.0.0"
  :license :MS-PL
  :description "Tests for special functions"
  :author "Steven Nunez <steve@symbolics.tech>"

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
			     (:file #:incomplete-gamma-small-data)
			     (:file #:incomplete-gamma-medium-data)
			     (:file #:incomplete-gamma-large-data)
			     (:file #:incomplete-gamma-integer-data)
			     (:file #:epilogue)))
	       (:module tests
		:components ((:file #:main)
			     (:file #:erf-test)
			     (:file #:gamma-test)
			     (:file #:incomplete-gamma-test))))
  :perform (asdf:test-op (o s)
			 (uiop:symbol-call :fiveam '#:run!
					   (uiop:find-symbol* '#:all-tests
							      :special-functions-tests))))
