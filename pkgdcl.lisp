;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2012 by Tamas K. Papp <tkpapp@gmail.com>
;;; Copyright (c) 2018-2020, 2024 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL

(uiop:define-package #:select-dev
    (:use #:cl #:alexandria #:alexandria+ #:anaphora #:let-plus)
  (:export
   ;; resolving slices into canonical representations
   #:canonical-singleton
   #:canonical-range
   #:canonical-sequence
   #:axis-dimension
   #:select-reserved-symbol?
   #:canonical-representation
   #:canonical-representations
   ;; traversing slices
   #:singleton-representation?
   #:all-singleton-representations?
   #:representation-dimensions
   #:row-major-setup
   #:column-major-setup
   #:traverse-representations)
  (:documentation "SELECT-DEV is used to implement SELECT operations on data structures other than arrays."))

(uiop:define-package #:select
  (:nicknames #:slct)
  (:use #:cl #:alexandria #:alexandria+ #:anaphora #:select-dev #:let-plus)
  (:export #:select
	   #:ref
	   #:including
	   #:nodrop
	   ;; #:head			;return a representation for the first n selections
	   ;; #:tail			;return a representation for the last n selections
	   #:range
	   #:mask
	   #:which
	   #:sample)
  (:documentation "SELECT is a facility for selecting portions of sequences or arrays."))

