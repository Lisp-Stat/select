;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: SLCT-T  -*-
;;; Copyright (c) 2021, 2019 by Symbolics Pte. Ltd. All rights reserved.

(in-package :slct-t)

#+genera (setf *print-array* t)

(def-suite utilities
  :description "Other selections."
  :in all-tests)
(in-suite utilities)

(test mask-and-which
  :description "Test mask and which functions."
  (let ((v #(0 1 2 3 4 5)))
    (is (equalp #(0 2 4) (which v :predicate #'evenp)) "Expected #(0 2 4) from WHICH, but got ~A." (which v :predicate #'evenp))
    (is (equalp #*010101 (mask  v #'oddp))  "Expected #*010101 from MASK, but got ~A."  (mask v #'oddp))
    (is (equalp #(0 2 4) (which (mask v #'evenp) :predicate #'plusp)) "Expected #(0 2 4) from WHICH & MASK together in the same form, but got ~A." (which (mask v #'evenp) :predicate #'plusp))))

