;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;;; Copyright (c) 2012 by Tamas K. Papp <tkpapp@gmail.com>
;;;; Copyright (c) 2018-2019 by Symbolics Pte. Ltd. All rights reserved.

(cl:defpackage "SLCT"
  (:nicknames "SELECT")
  (:use "CL" "ALEXANDRIA" "ANAPHORA" "LET-PLUS")
  (:export
   "SELECT"
   "REF"
   "INCLUDING"
   "NODROP"
   "HEAD"
   "TAIL"
   "RANGE"
   "MASK"
   "WHICH"
   "SELECT-RESERVED-SYMBOL?"
   "SINGLETON-REPRESENTATION?")
  (:documentation "SELECT is a facility for selecting portions of sequences or arrays."))

