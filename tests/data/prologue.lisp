;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: SPECIAL-FUNCTIONS-TESTS -*-
;;; Copyright (c) 2020 by Symbolics Pte. Ltd. All rights reserved.
(in-package #:special-functions-tests)

(defparameter *old-read-default-float-format* *read-default-float-format*)
(setf cl:*read-default-float-format* 'double-float)
