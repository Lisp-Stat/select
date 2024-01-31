;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: SELECT-T  -*-
;;; Copyright (c) 2019, 2024 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
(in-package :select-t)

#+genera (setf *print-array* t)

;;;;
;;;; Sequences
;;;;
(defsuite sequences (select))

(defvar vec10 #(0 1 2 3 4 5 6 7 8 9))
(defvar lst10 '(0 1 2 3 4 5 6 7 8 9))

(deftest singletons (sequences)
  ;Test singleton selections.

  ;;; Select single values
  (assert-true (= 1 (select vec10 1))  "Expected 1 from (select vec10 1) but got ~A."  (select vec10 1))
  (assert-true (= 9 (select vec10 -1)) "Expected 9 from (select vec10 -1) but got ~A." (select vec10 -1))
  (assert-true (= 1 (select lst10 1))  "Expected 1 from (select lst10 1) but got ~A."  (select lst10 1))
  (assert-true (= 9 (select lst10 -1)) "Expected 9 from (select lst10 -1) but got ~A." (select lst10 -1))

  ;;; Errors for out of range subscripts
  (assert-condition error (select vec10 10)   "SELECTing an out of range subscript must signal an error.")
  (assert-condition error (select vec10 nil)  "SELECTing nil as a subscript must signal an error.")
  (assert-condition error (select vec10 11)   "SELECTing an out of range positive subscript must signal an error.")
  (assert-condition error (select vec10 -11)  "SELECTing an out of range negative subscript must signal an error."))

(deftest ranges (sequences)
  :description "Test selecting a range of values."
  (assert-true (equalp #(3 4)   (select vec10 (range 3 5)))   "Expected #(3 4) but got ~A." (select vec10 (range 3 5)))
  (assert-true (equalp #(7 8 9) (select vec10 (range 7 nil))) "RANGE with NIL as END must return to end of sequence.")
  (assert-true (equalp #(7 8)   (select vec10 (range 7 -1)))  "RANGE must handle negative subscripts.")
  (assert-true (equalp '(7 8)   (select lst10 (range 7 -1)))  "RANGE must handle negative subscripts.")
  (assert-true (equalp #(8 9)   (select vec10 (range 8 10)))  "Expected #(8 9) but got ~A." (select vec10 (range 8 10)))
  (assert-true (equalp '(8 9)   (select lst10 (range 8 10)))  "Expected '(8 9) but got ~A." (select lst10 (range 8 10)))
  (assert-condition error            (select vec10 (range 1 1))    "(range 1 1) => nil and must signal error.")
  (assert-condition error            (select vec10 (range 5 4))    "If START > END for a RANGE an error must be signaled."))

(deftest representations (sequences)
  ;Test representations.

  ;; Vector source, vector selector
  (assert-equalp #(2 3 5)   (select vec10 #(2 3 5))
    "Expected #(2 3 5) from (select vec10 #(2 3 5)) but got ~A." (select vec10 #(2 3 5)))
  (assert-equalp #(2 3 3 7) (select vec10 #(2 3 3 -3))
    "Expected #(2 3 3 7) from (select vec10 #(2 3 3 -3)) but got ~A." (select vec10 #(2 3 3 -3)))

  ;; Vector source, list selector
  (assert-equalp #(2 3 5)   (select vec10 '(2 3 5))
    "Expected #(2 3 5) from (select vec10 '(2 3 5)) but got ~A." (select vec10 '(2 3 5)))
  (assert-equalp #(2 3 3 7) (select vec10 '(2 3 3 -3))
    "Expected #(2 3 3 7) from (select vec10 '(2 3 3 -3)) but got ~A." (select vec10 '(2 3 3 -3)))

  ;; List source, vector selector
  (assert-equalp '(2 3 5)   (select lst10 #(2 3 5))
    "Expected '(2 3 5) from (select lst10 #(2 3 5)) but got ~A." (select lst10 #(2 3 5)))
  (assert-equalp '(2 3 3 7) (select lst10 #(2 3 3 -3))
    "Expected '(2 3 3 7) from (select lst10 #(2 3 3 -3)) but got ~A." (select lst10 #(2 3 3 -3)))

  ;; List source, list selector
  (assert-equalp '(2 3 5)   (select lst10 '(2 3 5))
    "Expected '(2 3 5) from (select lst10 '(2 3 5)) but got ~A." (select lst10 '(2 3 5)))
  (assert-equalp '(2 3 3 7) (select lst10 '(2 3 3 -3))
    "Expected '(2 3 3 7) from (select lst10 '(2 3 3 -3)) but got ~A." (select lst10 '(2 3 3 -3)))

  ;; Masks
  (assert-equalp #(3 4 7) (select vec10 #*0001100100)
    "Expected #(3 4 7) from (select vec10 #*0001100100) but got ~A." (select vec10 #*0001100100))
  (assert-condition error (select vec10 #*00)
    "SELECTing with a bit mask shorter than the vector must signal an error.")

  ;; Vectors containing other forms
  (let ((answer1 (select vec10 (vector (range 1 3) 6 (range -2 -1))))
	(answer2 (select vec10 (vector (range 1 3) 6 (range 8 10))))
	(answer3 (select vec10 (vector (range 0 0) 6 (range -2 -1)))))
    (assert-equalp #(1 2 6 8)   answer1 "Expected #(1 2 6 8) but got ~A." answer1)
    (assert-equalp #(1 2 6 8 9) answer2 "Expected #(1 2 6 8 9) but got ~A." answer2)
    (assert-equalp #(6 8)       answer3 "Expected #(6 8) but got ~A." answer3)))


(deftest convenience-forms (sequences)
  ;Test short-hand convenience selection forms

  ;; Vector forms
  (let ((answer (select vec10 (range 3 5))))
	(assert-equalp #(3 4) answer "Expected #(3 4) from (select vec10 (range 3 5)) but got ~A." answer)
	(setf answer (select vec10 (including 3 5)))
	(assert-equalp #(3 4 5) answer "Expected #(3 4 5) from (select vec10 (including 3 5)) but got ~A." answer)
	(setf answer (select vec10 (nodrop 5)))
	(assert-equalp #(5) answer "Expected #(5) from (select vec10 (nodrop 5)) but got ~A." answer)
	(setf answer (select vec10 (head 3)))
	(assert-equalp #(0 1 2) answer "Expected #(0 1 2) from (select vec10 (head 3)) but got ~A." answer)
	(setf answer (select vec10 (tail 3)))
	(assert-equalp #(7 8 9) answer "Expected #(7 8 9) from (select vec10 (tail 3)) but got ~A." answer))

  ;; List forms
  (let ((answer (select lst10 (range 3 5))))
	(assert-equalp '(3 4) answer "Expected '(3 4) from (select lst10 (range 3 5)) but got ~A." answer)
	(setf answer (select lst10 (including 3 5)))
	(assert-equalp '(3 4 5) answer "Expected '(3 4 5) from (select lst10 (including 3 5)) but got ~A." answer)
	(setf answer (select lst10 (nodrop 5)))
	(assert-equalp '(5) answer "Expected '(5) from (select lst10 (nodrop 5)) but got ~A." answer)
	(setf answer (select lst10 (head 3)))
	(assert-equalp '(0 1 2) answer "Expected '(0 1 2) from (select lst10 (head 3)) but got ~A." answer)
	(setf answer (select lst10 (tail 3)))
	(assert-equalp '(7 8 9) answer "Expected '(7 8 9) from (select lst10 (tail 3)) but got ~A." answer)))
