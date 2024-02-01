;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-Lisp; Package: SELECT -*-
;;; Copyright (c) 2023, 2024 by Symbolics Pte. Ltd. All rights reserved.
;;; SPDX-License-identifier: MS-PL
(in-package #:select)

(declaim (inline uniformrv))
(defun uniformrv ()
  (loop
    for x = (random 1d0) then (random 1d0)
    until (plusp x)
    finally (return x)))

(defun vitter-method-a (fn n not-processed)
  "Vitter's Method A.

Mostly faithful to Vitter's Pascal code, but uses advanced Common Lisp
features like exponents and subtraction."
  (check-type n (integer 1 *))
  (check-type fn function)
  (assert (<= 1 n not-processed))
  (let* ((S 0)
         (quot 0d0)
         (V 0d0)
         (top (coerce (- not-processed n) 'double-float))
         (Nreal (coerce not-processed 'double-float)))
    (declare (type double-float V quot Nreal top))
    (declare (type integer not-processed S))
    (loop while (>= n 2) do
      (setf V (uniformrv)
            S 0
            quot (/ top Nreal))
      (loop while (> quot V) do
        (incf S)
        (decf top)
        (decf Nreal)
        (setf quot (/ (* quot top) Nreal)))
      (funcall fn (1+ S))
      (decf Nreal)
      (decf n))
    ;; Special case: n = 1.
    (setf S (truncate (* (round Nreal) (uniformrv))))
    (funcall fn (1+ S))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +alpha+ 1/13		;should this be a defparameter so it can be changed by the user?
  "The parameter that decides whether to use method A or method D.  Typical values of α are in the range 0.05-0.15."))

(defun vitter-method-d (fn n not-processed)
  "Vitter's Method D"
  (check-type fn function)
  (assert (<= 1 n not-processed))
  (let* ((limit 0)
         ;; Initialize floats.
         (top 0d0)
         (bottom 0d0)
         (nmin1inv 0d0)
         (y1 0d0)
         (y2 0d0)
         (U 0d0)
         (X 0d0)
         (negSreal 0d0)
         ;; Vitter begins here.
         (nreal (coerce n 'double-float))
         (ninv (/ nreal))
         (N-real (coerce not-processed 'double-float))
         (Vprime (expt (uniformrv) ninv))
         (qu1 (+ (- n) 1 not-processed))
         (qu1real (+ (- nreal) 1 N-real))
         (negalphainv #.(- (/ +alpha+)))
         (threshold (* (- negalphainv) n))
         (S 0))
    (declare (type double-float
                   nreal N-real ninv nmin1inv
                   U X Vprime
                   y1 y2 top bottom
                   negSreal qu1real))
    (declare (type integer S n not-processed))
    (loop while (and (> n 1) (< threshold not-processed)) do
      (setf nmin1inv (/ (1- nreal)))
      (loop
         ;; Step D2: generate U and X.
         (loop
            (setf X (* N-real (1+ (- Vprime)))
                  S (truncate X))
            (when (< S qu1)
              (return))
            (setf Vprime (expt (uniformrv) ninv)))
         (setf U (uniformrv)
               negSreal (* S -1d0))
         ;; Step D3: accept?
         (setf y1 (expt (* U (/ N-real qu1real)) nmin1inv)
               Vprime (* y1 (/ (- X) (1+ N-real)) (/ qu1real (+ negSreal qu1real))))
         (when (<= Vprime 1)
           ;; Accept! Test (2.8) is true.
           (return))
         ;; Step D4. Accept?
         (setf y2 1d0
               top (1- N-real))
         (if (> (1- n) S)
             (setf bottom (- N-real nreal)
                   limit (- not-processed s))
             (setf bottom (+ -1 NegSreal N-real)
                   limit qu1))
         (loop for i from (1- not-processed) downto limit do
           (setf y2 (/ (* y2 top) bottom))
           (decf top)
           (decf bottom))
         (when (>= (/ N-real (- N-real X))
                   (* y1 (expt y2 nmin1inv)))
           ;; Accept!
           (setf Vprime (expt (uniformrv) nmin1inv))
           (return))
         (setf Vprime (expt (uniformrv) ninv)))
      ;; Step D5: select the (S+1)st record.
      (funcall fn (1+ S))
      (setf not-processed (- (1- not-processed) S)
            N-real (+ negSreal (1- N-real)))
      (decf n)
      (decf nreal)
      (setf ninv nmin1inv
            qu1 (- qu1 S)
            qu1real (+ negSreal qu1real)
            threshold (+ threshold negalphainv)))
    (if (> n 1)
        ;; Use Method A.
        (vitter-method-a fn n not-processed)
        (progn
          (setf S (truncate (* not-processed Vprime)))
          (funcall fn (1+ S))))))

(defun map-random-below (fn n len)
  "Generate N random indices for a sequence of length LEN, in ascending order, calling FN on each index as it is generated."
  (check-type fn function)
  (assert (<= 0 n len))
  (cond ((= n 0) nil)
        ((= n 1) (funcall fn (random len)))
        (t (let ((index 0))
             (vitter-method-d
              (lambda (skip)
                (incf index skip)
                (funcall fn (1- index)))
              n len)))))

(defun generate-index-array/replacement (n len)
  (loop with index-array = (make-array n)
        for i from 0 below n
        do (setf (aref index-array i) (random len))
        finally (return (sort index-array #'<))))

(defun generate-index-array (n len)
  (assert (<= n len))
  (let+ ((i -1)
         (a (make-array n))
	 ((&flet seta (x)
	    (setf (aref a (incf i)) x))))
    (declare (alexandria:array-index i))
    (map-random-below #'seta
		      n len)
    a))

(defun collect-sample (seq index-array)
  (check-type seq sequence)
  (if (listp seq)
      (loop for offset = 0 then index
            for index across index-array
            do (setf seq (nthcdr (- index offset) seq))
            collect (car seq))
      (map 'vector (lambda (x)			;not listp
		   (elt seq x))
	   index-array)))


(defgeneric sample (data n &key
			     with-replacement
			     skip-unselected)
  (:documentation "Return a random sample of SEQ of size N.

If WITH-REPLACEMENT is true, return a random sample with
replacement (a \"draw\").

If WITH-REPLACEMENT is false, return a random sample without
replacement (a \"deal\").

If SKIP-UNSELECTED is non-NIL, do not return the elements of DF that we not part of the selection.  Non-NIL by default, as the typical use case is to split a data set into training and test data sets.")

  ;; I wish this could specialise on SEQUENCE but, at least in SBCL,
  ;; SEQUENCE is a type and not a class, so we need a method for both
  ;; LIST and VECTOR.
  (:method ((seq cons) n &key
			   with-replacement
			   skip-unselected)
    (declare (sequence seq))
    (when (< n 1)
      (setf n (round (* n (length seq)))))
    (when (> n (length seq))
      (error "Requested sample size N is greater than SEQ length."))
    (cond ((= n 0) nil)
          ((= n 1) (list (random-elt seq)))
          (t (let* ((len (length seq))
                    (index-array (if with-replacement
				     (generate-index-array/replacement n len)
				     (generate-index-array n len)))
		    (selected (shuffle (collect-sample seq index-array))))
               (if skip-unselected
		   selected
		   (values selected
			   (shuffle (set-difference* seq selected))))))))

  (:method ((seq vector) n &key
			     with-replacement
			     skip-unselected)
    (declare (sequence seq))
    (when (< n 1)
      (setf n (round (* n (length seq)))))
    (when (> n (length seq))
      (error "Requested sample size N is greater than SEQ length."))
    (cond ((= n 0) nil)
          ((= n 1) (vector (random-elt seq)))
          (t (let* ((len (length seq))
                    (index-array (if with-replacement
				     (generate-index-array/replacement n len)
				     (generate-index-array n len)))
		    (selected (shuffle (collect-sample seq index-array))))
               (if skip-unselected
		   selected
		   (values selected
			   (shuffle (set-difference* seq selected))))))))

  (:method ((arr array) n &key
			    with-replacement
			    skip-unselected)
    (let* ((nrow (array-dimension arr 0)))
      (when (< n 1)
	(setf n (round (* n nrow))))
      (when (> n nrow)
	(error "Requested number of rows N is greater than rows in the array."))
      (cond ((= n 0) nil)
            ((= n 1) (select arr (random nrow) t)) ;return random row
	    ((= n nrow) (if skip-unselected
			    arr
			    (values arr #())))
	    (t (let+ ((indices (let ((a (make-array nrow)))
				 (dotimes (i nrow a)
				   (setf (svref a i) i))))
		      (selected (if with-replacement
				    (generate-index-array/replacement n nrow)
				    (generate-index-array n nrow)))
		      (not-selected (set-difference* indices selected)))

		 (if skip-unselected
		     (select arr selected t)
		     (values (select arr selected t)
			     (select arr not-selected t)))))))))
