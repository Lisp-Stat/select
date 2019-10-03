;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;;; Copyright (c) 2019 by Symbolics Pte. Ltd. All rights reserved.

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
