;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;;; Copyright (c) 2018 by Steven Nunez <steve.nunez@inference.sg>

(defpackage "SLCT-T"
  (:use "CL"
        "SELECT"
        "FIVEAM")
  (:import-from "SLCT"
   "SUBSCRIPTS"
   "TRAVERSE-REPRESENTATIONS"
   "CANONICAL-REPRESENTATIONS"
   "COLUMN-MAJOR-SETUP")
  (:export
   #:run!
   #:all-tests))
