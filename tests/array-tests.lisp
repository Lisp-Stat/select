;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: SELECT-T  -*-
;;; Copyright (c) 2019, 2024 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
(in-package #:select-t)

#+genera (setf *print-array* t)

(defsuite arrays (select))

(defvar arr35 #2A((0 1 2 3 4)
                  (5 6 7 8 9)
                  (10 11 12 13 14)))

(deftest selection (arrays)
  ;"Test various selections from an array."
  (assert-equalp #(0 5 10) (select arr35 t 0)
    "Expected #(0 5 10) when SELECTing all rows (T) in column 0, but got ~A." (select arr35 t 0))
  (assert-equalp #2A((0) (5) (10)) (select arr35 t (range 0 1))
    "Expected #2A((0) (5) (10)) when SELECTing all rows in columns 0,1 but got ~A ." (select arr35 t (range 0 1)))
  (assert-equalp #2A((1 4)
		  (6 9)
		  (11 14))
                 (select arr35 #(0 1 2) #(1 -1))
    "SELECTing rows 0,1,2 and columns 1,-1 must return an array.")
  (assert-equalp #(5 6 7 8 9) (select arr35 1 t)
    "Expected #(5 6 7 8 9) when SELECTing row 1 and all columns (T), but got ~A" (select arr35 1 t))
  (assert-equalp #2A((5 6 7 8 9)) (select arr35 (range 1 2) t)
    "Expected #2A((5 6 7 8 9)) when SELECTing rows 1,2 with RANGE and all columns, but got ~A." (select arr35 (range 1 2) t))
  (assert-equalp #(6 7 8) (select arr35 1 (range 1 -1))
    "Expected #(6 7 8  when SELECTing row 1 and columns 1,-1 with RANGE, but got ~A." (select arr35 1 (range 1 -1)))
  (assert-equalp #2A((6 7 8)) (select arr35 (range 1 2) (range 1 -1))
    "Expected #2A((6 7 8)) when SELECTing rows 1,2 with RANGE and columns 1,-1 with RANGE, but got ~A." (select arr35 (range 1 2) (range 1 -1)))
  (let ((my-array (make-array 0 :adjustable t :fill-pointer 0)))
    (loop for x from 0 below 100
	  do (vector-push-extend x my-array))
    (assert-true (= (length my-array) (length (select my-array t)))
      "The length of the SELECTion should be ~D but is ~D" (length my-array) (length (select my-array t)))))

(deftest singleton-selection (arrays)
  ;Test selections that return a single element of an array.
  (assert-equalp 7  (ref arr35 1 2)
    "Expected 7 from ref to row 1 and column 2, but got ~A." (ref arr35 1 2))
  (assert-equalp 12 (ref arr35 -1 2)
    "Expected 12 from ref to row -1 and column 2, but got ~A." (ref arr35 -1 2))
  (assert-condition error (ref arr35 t t) "REF can only return a single element.")
  (let ((a (make-array '(1 3) :initial-contents '((2 3 5)))))
    (setf (ref a 0 1) 7)
    (assert-equalp #2A((2 7 5)) a) "REF must return a PLACE."))

(deftest setf-selection (arrays)
  ;Test setting elements of a selection.
  (let ((a (make-array '(3 2) :initial-element 0)))
    (setf (select a (range 1 2) t) #2A((1 2)))
    (assert-equalp #2A((0 0)
		       (1 2)
		       (0 0))
                   a
      "SELECT must return a PLACE to setf using positive subscripts.")
    (setf (select a -1 t) #(3 4))
    (assert-equalp #2A((0 0)
		    (1 2)
		    (3 4))
		   a
      "SELECT must return a PLACE to setf using negative row subscripts.")
    (setf (select a t -1) #(5 6 7))
    (assert-equalp #2A((0 5)
		    (1 6)
		    (3 7))
		   a
      "SELECT must return a PLACE to setf using negative column subscripts.")
    (setf (select a 0 -2) 8)
    (assert-equalp #2A((8 5)
		    (1 6)
		    (3 7))
		   a
      "SELECT must return a PLACE to setf for a singleton value.")
    (assert-condition error (setf (select a 0 0) #(1)) "Must signal error if setting a singleton PLACE to an array.")
    (assert-condition error (setf (select a (range 1 2) t) #2A((1))) "Must signal error if SELECTion and setf value are not of same rank.")))


(deftest traversals (arrays)
  ;Test row- and column- major order traversals.
  (let* ((representations (canonical-representations (array-dimensions arr35) (list t (range 0 2))))
	 answer
	 answer2)

    ;; Column-major traversal
    (traverse-representations (subscripts representations :setup column-major-setup)
      (push (apply #'aref arr35 subscripts) answer))
    (assert-equalp '(0 5 10 1 6 11) (reverse answer) "Expected (0 5 10 1 6 11) but got ~A" (reverse answer))

    ;; Row-major traversal, the default
    (traverse-representations (subscripts representations)
      (push (apply #'aref arr35 subscripts) answer2))
    (assert-equalp '(0 1 5 6 10 11) (reverse answer2) "Expected (0 1 5 6 10 11) but got ~A" (reverse answer2))))

