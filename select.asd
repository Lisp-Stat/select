;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: CL-USER -*-
;;;; Copyright (c) 2012 by Tamas K. Papp <tkpapp@gmail.com>
;;;; Copyright (c) 2018 by Steven Nunez <steve.nunez@inference.sg>

(asdf:defsystem "select"
  :description "DSL for array slices in Common Lisp."
  :long-description "\
Select is a facility for selecting portions of sequences or arrays. It
provides:

An user interface for taking slices (elements selected by the Cartesian product of vectors of subscripts for each axis) of array-like objects. The most important function is 'select'. Unless you want to define a method for this (besides what is alreadyimplemented), this is all you need from this library.

An extensible DSL for selecting a subset of valid subscripts. This is useful if, for example, you want to resolve column names in a data frame in your implementation of slice.

A set of utility functions for traversing slices in array-like objects."
  :version "1.0.0"
  :author "Steven Nunez <steve.nunez@inference.sg>"
  :source-control (:git "git://github.com/Symbolics/select")
  :homepage "http://inference.sg/projects/select/"
  :license "MIT"
  :depends-on ("alexandria"
               "anaphora"
               "let-plus")
  :in-order-to ((test-op (test-op "select/tests")))
  :serial t
  :components ((:file "package")
	       (:file "select-dev")
               (:file "select")))


(asdf:defsystem "select/tests"
  :description "DSL for array slices in Common Lisp - unit tests."
  :version "1.0.0"
  :author "Steven Nunez <steve.nunez@inference.sg>"
  :license "MIT"
  :depends-on ("select"
               "fiveam")
  :serial t
  :pathname "tests/"  
  :components ((:file "test-package")
	       (:file "main")
	       (:file "array-tests")
	       (:file "sequence-tests")
	       (:file "utility-tests"))
  :perform (asdf:test-op (o s)
  		    (uiop:symbol-call :fiveam :run! ; this is '#:run! in Fare's Postmodern ASD file
  				      (uiop:find-symbol* :all-tests
							 :slct-t))))

