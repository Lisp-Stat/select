;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: SELECT-T  -*-
;;; Copyright (c) 2024 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
(in-package :select-t)

#+genera (setf *print-array* t)

(defsuite sampling (select))

(defvar lst16 '(A B C D E F G H I J K L M N O P))
(defvar vec16 #(A B C D E F G H I J K L M N O P))

;; We need to wrap all the tests with a known random seed so the results will be reproducible.
(defvar state nil)
(let* ((lib-dir (asdf:system-relative-pathname "select" "tests/"))
       (state-file (make-pathname :directory (pathname-directory lib-dir)
				  :name "sample-tests-random-state"
				  :device (pathname-device lib-dir)
				  :type "lisp")))
  (with-open-file (stream state-file :direction :input)
    (setf state (read stream))))

(deftest arrays (sampling)
  (let+ ((*random-state* state)
	 ((&values train test) (sample arr35 2)) ;arr35 defined in array-tests
	 ((&values train-prop test-prop) (sample arr35 1/2))) ;prop = proportional

    ;; n = 0
    (assert-false (sample arr35 0))

    ;; n as integer
    (assert-equalp #2A((0 1 2 3 4)
		       (10 11 12 13 14))
      train)
    (assert-equalp #2A((5 6 7 8 9)) test)
    (assert-condition error (sample arr35 4))

    ;; n as proportion
    (assert-equalp #2A((0 1 2 3 4)
		       (10 11 12 13 14))
      train-prop)
    (assert-equalp #2A((5 6 7 8 9)) test-prop)
    (assert-condition error (sample arr35 1.2))

    ;; n = all rows of array
    (assert-equalp arr35 (sample arr35 3 :skip-unselected t))))


(deftest sequences (sampling)
  ;; TODO: test sampling with replacement
  (let+ ((*random-state* state)
	 ((&values train-lst test-lst) (sample lst16 3))
	 ((&values train-vec test-vec) (sample vec16 3))
	 ((&values train-lst-prop test-lst-prop) (sample lst16 1/2))
	 ((&values train-vec-prop test-vec-prop) (sample vec16 1/2)))

    ;; n=0
    (assert-false (sample vec16 0))
    (assert-false (sample lst16 0))

    ;; n=1
    (assert-equal  '(P) (sample lst16 1))
    (assert-equalp #(F) (sample vec16 1))

    ;; n as integer
    (assert-equal '(M J D) train-lst)
    (assert-equal '(K A B I C H O N E G L P F) test-lst)
    (assert-equalp #(J P G) train-vec)
    (assert-equalp #(O L H F K N C B A I E M D) test-vec)
    (assert-condition error (sample lst16 20))
    (assert-condition error (sample vec16 20))

    ;; n as proportion
    (assert-true (= 8 (length train-lst-prop)))
    (assert-equal '(D O E L G N M B) train-lst-prop)
    (assert-equal '(F P C H I K A J) test-lst-prop)
    (assert-equalp #(P B L A H F C I) train-vec-prop)
    (assert-equalp #(M J E D G N O K) test-vec-prop))
    (assert-condition error (sample lst16 1.5))
    (assert-condition error (sample vec16 1.1)))
