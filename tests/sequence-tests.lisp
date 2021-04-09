;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: SLCT-T  -*-
;;; Copyright (c) 2019-2020 by Symbolics Pte. Ltd. All rights reserved.

(in-package #:slct-t)

#+genera (setf *print-array* t)

(def-suite sequence-selection
  :description "Selections from a sequence."
  :in all-tests)
(in-suite sequence-selection)

;(defvar vec10 #(0 1 2 3 4 5 6 7 8 9))
(defvar vec10 (make-array 10 :element-type '(unsigned-byte 8) :initial-contents #(0 1 2 3 4 5 6 7 8 9)))
(defvar lst10 '(0 1 2 3 4 5 6 7 8 9))

(test singletons
  :description "Test singleton selections."

  ;;; Select single values
  (is (= 1 (select vec10 1))  "Expected 1 from (select vec10 1) but got ~A."  (select vec10 1))
  (is (= 9 (select vec10 -1)) "Expected 9 from (select vec10 -1) but got ~A." (select vec10 -1))
  (is (= 1 (select lst10 1))  "Expected 1 from (select lst10 1) but got ~A."  (select lst10 1))
  (is (= 9 (select lst10 -1)) "Expected 9 from (select lst10 -1) but got ~A." (select lst10 -1))

  ;;; Errors for out of range subscripts
  (signals error (select vec10 10)   "SELECTing an out of range subscript must signal an error.")
  (signals error (select vec10 nil)  "SELECTing nil as a subscript must signal an error.")
  (signals error (select vec10 11)   "SELECTing an out of range positive subscript must signal an error.")
  (signals error (select vec10 -11)  "SELECTing an out of range negative subscript must signal an error."))

(test ranges
  :description "Test selecting a range of values."
  (is (equalp #(3 4)   (select vec10 (range 3 5)))   "Expected #(3 4) but got ~A." (select vec10 (range 3 5)))
  (is (equalp #(7 8 9) (select vec10 (range 7 nil))) "RANGE with NIL as END must return to end of sequence.")
  (is (equalp #(7 8)   (select vec10 (range 7 -1)))  "RANGE must handle negative subscripts.")
  (is (equalp '(7 8)   (select lst10 (range 7 -1)))  "RANGE must handle negative subscripts.")
  (is (equalp #(8 9)   (select vec10 (range 8 10)))  "Expected #(8 9) but got ~A." (select vec10 (range 8 10)))
  (is (equalp '(8 9)   (select lst10 (range 8 10)))  "Expected '(8 9) but got ~A." (select lst10 (range 8 10)))
  (signals error       (select vec10 (range 1 1))    "(range 1 1) => nil and must signal error.")
  (signals error       (select vec10 (range 5 4))    "If START > END for a RANGE an error must be signaled."))

(test representations
  :description "Test representations."
  ;; Vector source, vector selector
  (is (equalp #(2 3 5)   (select vec10 #(2 3 5)))     "Expected #(2 3 5) from (select vec10 #(2 3 5)) but got ~A." (select vec10 #(2 3 5)))
  (is (equalp #(2 3 3 7) (select vec10 #(2 3 3 -3)))  "Expected #(2 3 3 7) from (select vec10 #(2 3 3 -3)) but got ~A." (select vec10 #(2 3 3 -3)))

  ;; Vector source, list selector
  (is (equalp #(2 3 5)   (select vec10 '(2 3 5)))     "Expected #(2 3 5) from (select vec10 '(2 3 5)) but got ~A." (select vec10 '(2 3 5)))
  (is (equalp #(2 3 3 7) (select vec10 '(2 3 3 -3)))  "Expected #(2 3 3 7) from (select vec10 '(2 3 3 -3)) but got ~A." (select vec10 '(2 3 3 -3)))

  ;; List source, vector selector
  (is (equalp '(2 3 5)   (select lst10 #(2 3 5)))     "Expected '(2 3 5) from (select lst10 #(2 3 5)) but got ~A." (select lst10 #(2 3 5)))
  (is (equalp '(2 3 3 7) (select lst10 #(2 3 3 -3)))  "Expected '(2 3 3 7) from (select lst10 #(2 3 3 -3)) but got ~A." (select lst10 #(2 3 3 -3)))

  ;; List source, list selector
  (is (equalp '(2 3 5)   (select lst10 '(2 3 5)))     "Expected '(2 3 5) from (select lst10 '(2 3 5)) but got ~A." (select lst10 '(2 3 5)))
  (is (equalp '(2 3 3 7) (select lst10 '(2 3 3 -3)))  "Expected '(2 3 3 7) from (select lst10 '(2 3 3 -3)) but got ~A." (select lst10 '(2 3 3 -3)))

  ;; Masks
  (is (equalp #(3 4 7) (select vec10 #*0001100100)) "Expected #(3 4 7) from (select vec10 #*0001100100) but got ~A." (select vec10 #*0001100100))
  (signals error       (select vec10 #*00)          "SELECTing with a bit mask shorter than the vector must signal an error.")

  ;; Vectors containing other forms
  (let ((answer1 (select vec10 (vector (range 1 3) 6 (range -2 -1))))
	(answer2 (select vec10 (vector (range 1 3) 6 (range 8 10))))
	(answer3 (select vec10 (vector (range 0 0) 6 (range -2 -1)))))
    (is (equalp #(1 2 6 8)   answer1) "Expected #(1 2 6 8) but got ~A." answer1)
    (is (equalp #(1 2 6 8 9) answer2) "Expected #(1 2 6 8 9) but got ~A." answer2)
    (is (equalp #(6 8)       answer3) "Expected #(6 8) but got ~A." answer3)))


(test convenience-forms
  :description "Test short-hand convenience selection forms."

  ;; Vector forms
  (let ((answer (select vec10 (range 3 5))))
	(is (equalp #(3 4) answer) "Expected #(3 4) from (select vec10 (range 3 5)) but got ~A." answer)
	(setf answer (select vec10 (including 3 5)))
	(is (equalp #(3 4 5) answer) "Expected #(3 4 5) from (select vec10 (including 3 5)) but got ~A." answer)
	(setf answer (select vec10 (nodrop 5)))
	(is (equalp #(5) answer) "Expected #(5) from (select vec10 (nodrop 5)) but got ~A." answer))
	;; (setf answer (select vec10 (head 3)))
	;; (is (equalp #(0 1 2) answer) "Expected #(0 1 2) from (select vec10 (head 3)) but got ~A." answer)
	;; (setf answer (select vec10 (tail 3)))
	;; (is (equalp #(7 8 9) answer) "Expected #(7 8 9) from (select vec10 (tail 3)) but got ~A." answer))

  ;; List forms
  (let ((answer (select lst10 (range 3 5))))
	(is (equalp '(3 4) answer) "Expected '(3 4) from (select lst10 (range 3 5)) but got ~A." answer)
	(setf answer (select lst10 (including 3 5)))
	(is (equalp '(3 4 5) answer) "Expected '(3 4 5) from (select lst10 (including 3 5)) but got ~A." answer)
	(setf answer (select lst10 (nodrop 5)))
	(is (equalp '(5) answer) "Expected '(5) from (select lst10 (nodrop 5)) but got ~A." answer)))
	;; (setf answer (select lst10 (head 3)))
	;; (is (equalp '(0 1 2) answer) "Expected '(0 1 2) from (select lst10 (head 3)) but got ~A." answer)
	;; (setf answer (select lst10 (tail 3)))
	;; (is (equalp '(7 8 9) answer) "Expected '(7 8 9) from (select lst10 (tail 3)) but got ~A." answer)))
