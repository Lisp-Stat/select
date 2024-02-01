;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: ASDF -*-
;;; Copyright (c) 2012 by Tamas K. Papp <tkpapp@gmail.com>
;;; Copyright (c) 2018-2022, 2024 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL

(defsystem "select"
  :version     "1.2.0"
  :license     :MS-PL
  :author      "Steve Nunez <steve@symbolics.tech>"
  :long-name   "Slicing and selection library for Data Frames"
  :description "DSL for array and data-frame slices"
  :long-description  #.(uiop:read-file-string
			(uiop:subpathname *load-pathname* "description.text"))
  :homepage    "https://lisp-stat.dev/docs/manuals/select"
  :source-control (:git "git://github.com/Lisp-Stat/select")
  :bug-tracker    "https://github.com/Lisp-Stat/select/issues/"

  :depends-on ("alexandria"
	       "alexandria+"
               "anaphora"
               "let-plus")
  :in-order-to ((test-op (test-op "select/tests")))
  :serial t
  :components ((:file "pkgdcl")
	       (:file "select-dev")
               (:file "select")
	       (:file "sample")))

(defsystem "select/tests"
  :version     "1.2.0"
  :description "DSL for array slices - unit tests."
  :author      "Steven Nunez"
  :license     :MS-PL
  :depends-on ("select"
               "clunit2")
  :serial t
  :pathname "tests/"
  :components ((:file "test-package")
	       (:file "main")
	       (:file "array-tests")
	       (:file "sequence-tests")
	       (:file "sample-tests")
	       (:file "utility-tests"))
  :perform (test-op (o s)
		    (let ((*print-pretty* t)) ;work around clunit issue #9
		      (symbol-call :clunit :run-suite
				   (find-symbol* :select ;the suite of tests for SELECT
						 :select-t)
					   :use-debugger nil))))

