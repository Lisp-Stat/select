;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2012 by Tamas K. Papp <tkpapp@gmail.com>
;;; Copyright (c) 2018-2021 by Symbolics Pte. Ltd. All rights reserved.

(asdf:defsystem :select
  :description "DSL for array slices."
  :long-description "\
Select is a facility for selecting portions of sequences, arrays or data-frames. It provides:

An API for taking slices (elements selected by the Cartesian product of vectors of subscripts for each axis) of array-like objects.  The most important function is `select`. Unless you want to define additional methods for `select`, this is pretty much all you need from this library.  See the documentation at https://lisp-stat.github.io/select/ for a tutorial.

An extensible DSL for selecting a subset of valid subscripts. This is useful if, for example, you want to resolve column names in a data frame in your implementation of slice.

A set of utility functions for traversing slices in array-like objects."
  :version        (:read-file-form #:version.sexp)
  :author         "Steve Nunez"
  :homepage       "https://lisp-stat.github.io/select/"
  :source-control (:git "git://github.com/Lisp-Stat/select")
  :bug-tracker    "https://github.com/Lisp-Stat/select/issues/"
  :license        :MS-PL
  :depends-on (#:alexandria
               #:anaphora
               #:let-plus)
  :in-order-to ((test-op (test-op "select/tests")))
  :serial t
  :components ((:file #:package)
	       (:file #:select-dev)
               (:file #:select)))


(asdf:defsystem #:select/tests
  :description "DSL for array slices - unit tests."
  :version     (:read-file-form #:version.sexp)
  :author      "Steven Nunez"
  :license     :MS-PL
  :depends-on (#:select
               #:fiveam)
  :serial t
  :pathname "tests/"
  :components ((:file #:test-package)
	       (:file #:main)
	       (:file #:array-tests)
	       (:file #:sequence-tests)
	       (:file #:utility-tests))
  :perform (asdf:test-op (o s)
  		    (uiop:symbol-call :fiveam :run! ; this is '#:run! in Fare's Postmodern ASD file
  				      (uiop:find-symbol* :all-tests
							 :slct-t))))

