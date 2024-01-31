;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: SELECT-T  -*-
;;; Copyright (c) 2019, 2024 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
(in-package :select-t)

#+genera (setf *print-array* t)

(defsuite utilities (select)) ;Other selections

(deftest mask-and-which (utilities)
  ;Test mask and which functions
  (let ((v #(0 1 2 3 4 5)))
    (assert-equalp #(0 2 4) (which v :predicate #'evenp)
      "Expected #(0 2 4) from WHICH, but got ~A." (which v :predicate #'evenp))
    (assert-equalp #*010101 (mask  v #'oddp)
      "Expected #*010101 from MASK, but got ~A."  (mask v #'oddp))
    (assert-equalp #(0 2 4) (which (mask v #'evenp) :predicate #'plusp)
      "Expected #(0 2 4) from WHICH & MASK together in the same form, but got ~A." (which (mask v #'evenp) :predicate #'plusp))))

