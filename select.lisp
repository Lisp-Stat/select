;;; -*- Mode: LISP; Base: 10; Syntax: Ansi-Common-Lisp; Package: SLCT -*-
;;; Copyright (c) 2012 by Tamas K. Papp <tkpapp@gmail.com>
;;; Copyright (c) 2018-2021 by Symbolics Pte. Ltd. All rights reserved.

(in-package :slct)

;;;
;;; Public API
;;;

(defgeneric ref (object &rest subscripts)
  (:documentation "Return the element of OBJECT specified by SUBSCRIPTS."))

(defgeneric (setf ref) (value object &rest subscripts)
  (:documentation "Stores VALUE into the place specified by SUBSCRIPTS."))

;;; TODO:SN:20171209: Enhancement: add optional parameter for order, either row- or column-major.
(defgeneric select (object &rest selections)
  (:documentation "Return the slices of OBJECT specified by SELECTIONS."))

(defgeneric (setf select) (value object &rest selections)
  (:documentation "Stores VALUES into the locations given by SELECTIONS."))

;;;
;;; Convenience forms for common selections
;;;

(defstruct including
  "Range, including both ends."
  start end)

(defun including (start end)
  "Range, including both ends."
  (make-including :start start :end end))

(defmethod canonical-representation (axis (selection including))
  "The canonical representation for INCLUDING."
  (let+ (((&structure-r/o including- start end) selection)
         (start (canonical-representation axis start))
         (end (canonical-representation axis end)))
    (canonical-range start (1+ end))))


;;; These forms largely duplicate CANONICAL-RANGE, but RANGE is more usable
(defstruct range
  "Range, including start, excluding end."
  start end)

(defun range (start end)
  "Range, including START, excluding END."
  (make-range :start start :end end))

(defmethod canonical-representation (axis (selection range))
  "The canonical representation for RANGE."
  (let+ (((&structure-r/o range- start end) selection)
         (start (canonical-representation axis start))
         (end (canonical-representation axis end)))
    (canonical-range start end)))


(defstruct nodrop
  "Select a single index, but don't drop a dimension."
  index)

(defun nodrop (index)
  "Select a single index, but do not drop a dimension."
  (make-nodrop :index index))

(defmethod canonical-representation (axis (selection nodrop))
  "The canonical representation for NODROP."
  (let ((start (canonical-representation axis (nodrop-index selection))))
    (canonical-range start (1+ start))))
#+nil
(defun head (count)
  "First COUNT indexes."
  (check-type count alexandria:array-index)
  (range 0 count))
#+nil
(defun tail (count)
  "Last COUNT indexes."
  (check-type count alexandria:array-index)
  (range (- count) nil))



;;;
;;; Implementation for arrays and vectors
;;;

(defmethod select ((array array) &rest selections)
  "Return the SELECTIONS in the given ARRAY."
  (let* ((representations (canonical-representations (if (array-has-fill-pointer-p array)
							 (list (length array))
							 (array-dimensions array))
                                                     selections))
         (dimensions (representation-dimensions representations)))
    (if dimensions
        (aprog1 (make-array dimensions :element-type (array-element-type array))
          (traverse-representations (subscripts representations :index index)
            (setf (row-major-aref it index)
                  (apply #'aref array subscripts))))
        (apply #'aref array representations))))

(defmethod (setf select) ((value array) (array array) &rest selections)
  (let ((representations (canonical-representations (array-dimensions array)
                                                    selections)))
    (assert (equalp (representation-dimensions representations)
                    (array-dimensions value)) () "Incompatible dimensions.")
    (traverse-representations (subscripts representations :index index)
      (setf (apply #'aref array subscripts)
            (row-major-aref value index)))))

(defmethod (setf select) (value (array array) &rest selections)
  (let ((representations (canonical-representations (array-dimensions array)
                                                    selections)))
    (assert (all-singleton-representations? representations))
    (setf (apply #'aref array representations) value)))

(defmethod ref ((array array) &rest subscripts)
  (let ((representations (canonical-representations (array-dimensions array)
                                                   subscripts)))
    (assert (all-singleton-representations? representations))
    (apply #'aref array representations)))

(defmethod (setf ref) (value (array array) &rest subscripts)
  (let ((representations (canonical-representations (array-dimensions array)
                                                   subscripts)))
    (assert (all-singleton-representations? representations))
    (setf (apply #'aref array representations) value)))


;;;
;;; Implementation for lists
;;;

(defmethod select ((lst list) &rest selections)
  "Select from LST the subscripts or range specified in SELECTIONS. SELECTIONS must be a VECTOR, LIST or RANGE."
  (assert (or (typep selections 'vector)
	      (consp selections))
	  (selections)
	  "~A is not a vector or list." selections)
  (let ((representations (canonical-representations (list (length lst))
						    selections))
        values)
    (traverse-representations (subscripts representations)
      (push (nth (car subscripts) lst) values))
    (if (and (= (length values) 1)		;SN:20171125 Add so dimension is dropped in singleton selection, but not with nodrop
	     (not (typep (first selections) 'nodrop)))
	(car values)
	(nreverse values))))

;;;
;;; Masks
;;;

(defgeneric mask (sequence predicate)
  (:documentation "Map sequence into a simple-bit-vector, using 1 when PREDICATE yields true, 0 otherwise.")
  (:method ((sequence sequence) predicate)
    (map 'bit-vector (lambda (element)
                       (if (funcall predicate element)
                           1
                           0))
         sequence)))

(defgeneric which (sequence &key predicate)
  (:documentation "Return an index of the positions in SEQUENCE which satisfy PREDICATE. Defaults to return non-NIL indices.")
  (:method ((sequence sequence) &key (predicate #'identity))
    (let ((index 0)
          positions)
      (map nil (lambda (element)
                 (when (funcall predicate element)
                   (push index positions))
                 (incf index))
           sequence)
      (coerce (nreverse positions) '(simple-array fixnum (*))))))
