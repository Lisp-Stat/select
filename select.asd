;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2012 by Tamas K. Papp <tkpapp@gmail.com>
;;; Copyright (c) 2018-2022 by Symbolics Pte. Ltd. All rights reserved.

(asdf:defsystem #:select
  :version     "1.0.0"
  :license     :MS-PL
  :author      "Steve Nunez <steve@symbolics.tech>"
  :long-name   "Slicing for Data Frames"
  :description "DSL for array and data-frame slices"
  :long-description  #.(uiop:read-file-string
			(uiop:subpathname *load-pathname* "description.text"))
  :homepage    "https://lisp-stat.dev/docs/manuals/select"
  :source-control (:git "git://github.com/Lisp-Stat/select")
  :bug-tracker    "https://github.com/Lisp-Stat/select/issues/"

  :depends-on (#:alexandria
               #:anaphora
               #:let-plus)
  :in-order-to ((test-op (test-op "select/tests")))
  :serial t
  :components ((:file #:package)
	       (:file #:select-dev)
               (:file #:select)))

(asdf:defsystem #:select/tests
  :version     "1.0.0"
  :description "DSL for array slices - unit tests."
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

