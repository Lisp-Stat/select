;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: SELECT-T  -*-
;;; Copyright (c) 2019, 2024 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
(in-package #:select-t)

(defsuite select ())

#+genera (setf *print-array* t)

(defun run (&optional interactive?)
  (run-suite 'select :use-debugger interactive?))

(defun test-select ()
  "Run tests at REPL"
  (run-suite 'select))

