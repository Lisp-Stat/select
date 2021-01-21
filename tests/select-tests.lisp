;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: SLCT-T  -*-
;;; Copyright (c) 2012 by Tamas K. Papp <tkpapp@gmail.com>
;;; Copyright (c) 2019-2021 by Symbolics Pte. Ltd. All rights reserved.

(in-package #:slct-t)

(def-suite all-tests
    :description "The master suite of all select tests.")
(in-suite all-tests)

#+genera (setf *print-array* t)

(defun test-slct ()
  (run! 'all-tests))

;;;;
;;;; Sequences
;;;;
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
  (signals error            (select vec10 (range 1 1))    "(range 1 1) => nil and must signal error.")
  (signals error            (select vec10 (range 5 4))    "If START > END for a RANGE an error must be signaled."))

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
	(is (equalp #(5) answer) "Expected #(5) from (select vec10 (nodrop 5)) but got ~A." answer)
	(setf answer (select vec10 (head 3)))
	(is (equalp #(0 1 2) answer) "Expected #(0 1 2) from (select vec10 (head 3)) but got ~A." answer)
	(setf answer (select vec10 (tail 3)))
	(is (equalp #(7 8 9) answer) "Expected #(7 8 9) from (select vec10 (tail 3)) but got ~A." answer))

  ;; List forms
  (let ((answer (select lst10 (range 3 5))))
	(is (equalp '(3 4) answer) "Expected '(3 4) from (select lst10 (range 3 5)) but got ~A." answer)
	(setf answer (select lst10 (including 3 5)))
	(is (equalp '(3 4 5) answer) "Expected '(3 4 5) from (select lst10 (including 3 5)) but got ~A." answer)
	(setf answer (select lst10 (nodrop 5)))
	(is (equalp '(5) answer) "Expected '(5) from (select lst10 (nodrop 5)) but got ~A." answer)
	(setf answer (select lst10 (head 3)))
	(is (equalp '(0 1 2) answer) "Expected '(0 1 2) from (select lst10 (head 3)) but got ~A." answer)
	(setf answer (select lst10 (tail 3)))
	(is (equalp '(7 8 9) answer) "Expected '(7 8 9) from (select lst10 (tail 3)) but got ~A." answer)))


;;;;
;;;; Arrays
;;;;
(def-suite array-selection
  :description "Selections from an array."
  :in all-tests)
(in-suite array-selection)

(defvar arr35 #2A((0 1 2 3 4)
                  (5 6 7 8 9)
                  (10 11 12 13 14)))

(test array-selection
  :description "Test various selections from an array."
  (is (equalp #(0 5 10)         (select arr35 t 0))           "Expected #(0 5 10) when SELECTing all rows (T) in column 0, but got ~A." (select arr35 t 0))
  (is (equalp #2A((0) (5) (10)) (select arr35 t (range 0 1))) "Expected #2A((0) (5) (10)) when SELECTing all rows in columns 0,1 but got ~A ." (select arr35 t (range 0 1)))
  (is (equalp #2A((1 4)
		  (6 9)
		  (11 14))
	      (select arr35 #(0 1 2) #(1 -1)))                "SELECTing rows 0,1,2 and columns 1,-1 must return an array.")
  (is (equalp #(5 6 7 8 9) (select arr35 1 t))                "Expected #(5 6 7 8 9) when SELECTing row 1 and all columns (T), but got ~A" (select arr35 1 t))
  (is (equalp #2A((5 6 7 8 9)) (select arr35 (range 1 2) t))  "Expected #2A((5 6 7 8 9)) when SELECTing rows 1,2 with RANGE and all columns, but got ~A." (select arr35 (range 1 2) t))
  (is (equalp #(6 7 8) (select arr35 1 (range 1 -1)))         "Expected #(6 7 8  when SELECTing row 1 and columns 1,-1 with RANGE, but got ~A." (select arr35 1 (range 1 -1)))
  (is (equalp #2A((6 7 8)) (select arr35 (range 1 2) (range 1 -1)))
      "Expected #2A((6 7 8)) when SELECTing rows 1,2 with RANGE and columns 1,-1 with RANGE, but got ~A." (select arr35 (range 1 2) (range 1 -1)))
  (let ((my-array (make-array 0 :adjustable t :fill-pointer 0)))
    (loop for x from 0 below 100
	  do (vector-push-extend x my-array))
    (is (= (length my-array) (length (select my-array t))) "The length of the SELECTion should be ~D but is ~D" (length my-array) (length (select my-array t)))))

(test array-singleton-selection
  :description "Test selections that return a single element of an array."
  (is (equalp 7  (ref arr35 1 2))   "Expected 7 from ref to row 1 and column 2, but got ~A." (ref arr35 1 2))
  (is (equalp 12 (ref arr35 -1 2))  "Expected 12 from ref to row -1 and column 2, but got ~A." (ref arr35 -1 2))
  (signals error (ref arr35 t t)    "REF can only return a single element.")
  (let ((a (make-array '(1 3) :initial-contents '((2 3 5)))))
    (setf (ref a 0 1) 7)
    (is (equalp #2A((2 7 5)) a) "REF must return a PLACE.")))

(test array-setf-selection
  :description "Test setting elements of a selection."
  (let ((a (make-array '(3 2) :initial-element 0)))
    (setf (select a (range 1 2) t) #2A((1 2)))
    (is (equalp #2A((0 0)
		    (1 2)
		    (0 0))
		a) "SELECT must return a PLACE to setf using positive subscripts.")
    (setf (select a -1 t) #(3 4))
    (is (equalp #2A((0 0)
		    (1 2)
		    (3 4))
		a) "SELECT must return a PLACE to setf using negative row subscripts.")
    (setf (select a t -1) #(5 6 7))
    (is (equalp #2A((0 5)
		    (1 6)
		    (3 7))
		a) "SELECT must return a PLACE to setf using negative column subscripts.")
    (setf (select a 0 -2) 8)
    (is (equalp #2A((8 5)
		    (1 6)
		    (3 7))
		a) "SELECT must return a PLACE to setf for a singleton value.")
    (signals error (setf (select a 0 0) #(1)) "Must signal error if setting a singleton PLACE to an array.")
    (signals error (setf (select a (range 1 2) t) #2A((1))) "Must signal error if SELECTion and setf value are not of same rank.")))

(test mask-and-which
  :description "Test mask and which functions."
  (let ((v #(0 1 2 3 4 5)))
    (is (equalp #(0 2 4) (which #'evenp v)) "Expected #(0 2 4) from WHICH, but got ~A." (which #'evenp v))
    (is (equalp #*010101 (mask #'oddp v))   "Expected #*010101 from MASK, but got ~A." (mask #'oddp v))
    (is (equalp #(0 2 4) (which #'plusp (mask #'evenp v))) "Expected #(0 2 4) from WHICH & MASK together in the same form, but got ~A." (which #'plusp (mask #'evenp v)))))

(test traversals
  :description "Test row- and column- major order traversals."
  (let* ((representations (canonical-representations (array-dimensions arr35) (list t (range 0 2))))
	 answer
	 answer2)

    ;; Column-major traversal
    (traverse-representations (subscripts representations :setup column-major-setup)
      (push (apply #'aref arr35 subscripts) answer))
    (is (equalp '(0 5 10 1 6 11) (reverse answer)) "Expected (0 5 10 1 6 11) but got ~A" (reverse answer))

    ;; Row-major traversal, the default
    (traverse-representations (subscripts representations)
      (push (apply #'aref arr35 subscripts) answer2))
    (is (equalp '(0 1 5 6 10 11) (reverse answer2)) "Expected (0 1 5 6 10 11) but got ~A" (reverse answer2))))

