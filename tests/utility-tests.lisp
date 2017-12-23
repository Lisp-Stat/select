;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: SLCT-T  -*-
;;;; Copyright (c) 2018 by Steven Nunez <steve.nunez@inference.sg>

(in-package "SLCT-T")

#+genera (setf *print-array* t)

(def-suite utilities
  :description "Other selections."
  :in all-tests)
(in-suite utilities)

(test mask-and-which
  :description "Test mask and which functions."
  (let ((v #(0 1 2 3 4 5)))
    (is (equalp #(0 2 4) (which #'evenp v)) "Expected #(0 2 4) from WHICH, but got ~A." (which #'evenp v))
    (is (equalp #*010101 (mask #'oddp v))   "Expected #*010101 from MASK, but got ~A." (mask #'oddp v))
    (is (equalp #(0 2 4) (which #'plusp (mask #'evenp v))) "Expected #(0 2 4) from WHICH & MASK together in the same form, but got ~A." (which #'plusp (mask #'evenp v)))))

