;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: SLCT-T  -*-
;;;; Copyright (c) 2019 by Symbolics Pte. Ltd. All rights reserved.

(in-package "SLCT-T")

(def-suite all-tests
    :description "The master suite of all SELECT tests.")
(in-suite all-tests)

#+genera (setf *print-array* t)

(defun test-slct ()
  (run! 'all-tests))

