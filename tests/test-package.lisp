;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;; Copyright (c) 2019, 2024 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL

(uiop:define-package "SELECT-T"
  (:use :cl :select :clunit :let-plus)
  (:import-from #:slct #:subscripts
                       #:traverse-representations
                       #:canonical-representations
                       #:column-major-setup
		       #:head
		       #:tail)
  (:export #:run #:test-select))

