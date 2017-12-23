;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: SLCT-T  -*-
;;;; Copyright (c) 2018 by Steven Nunez <steve.nunez@inference.sg>

(in-package "SLCT-T")

(def-suite all-tests
    :description "The master suite of all select tests.")
(in-suite all-tests)

#+genera (setf *print-array* t)

(defun test-slct ()
  (run! 'all-tests))

