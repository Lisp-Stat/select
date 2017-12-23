;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: SLCT -*-
;;;; Copyright (c) 2012 by Tamas K. Papp <tkpapp@gmail.com>
;;;; Copyright (c) 2018 by Steven Nunez <steve.nunez@inference.sg>

(in-package "SLCT")

;;;;
;;;; Resolve selections into canonical representations
;;;;

(defun canonical-singleton (index)
  "Canonical representation of a singleton index (a nonnegative integer, which is a valid array index)."
  (assert (typep index 'array-index))
  index)


(defstruct canonical-range
  "Canonical representation of a contiguous set of array indices from START (inclusive) to END (exclusive)."
  (start nil :type array-index)
  (end nil :type array-index))

(defun canonical-range (start end)
  "Canonical representation of a contiguous set of array indices from START (inclusive) to END (exclusive)."
  (assert (and (typep start 'array-index)
               (typep end 'array-index)
               (<= start end))); SN:20171101: Changing < to <= partially fixes item 3 of issue 3 in Papp's repository.
  (make-canonical-range :start start :end end))


(defstruct canonical-sequence
  "Canonical representation of a sequence of array indexes."
  (vector nil :type (simple-array array-index (*))))

(defun canonical-sequence (sequence)
  "Canonical representation of array indexes from canonical-sequence SEQUENCE.

May share structure. Vectors of the upgraded type of (SIMPLE-ARRAY ARRAY-INDEX (*)) are preferred
for efficiency, otherwise they are coerced."

  (let ((vector (coerce sequence '(simple-array array-index (*)))))
    (assert (and (plusp (length vector))
                 (every (lambda (index)
                          (typep index 'array-index))
                        vector)))
    (make-canonical-sequence :vector vector)))


(defgeneric axis-dimension (axis)
  (:documentation "Return the dimension of axis.  Needs to be defined for non-integer axes."))

(defun select-reserved-symbol? (symbol)
  "Test if SYMBOL has special semantics for SELECTION."
  (or (eq symbol t) (eq symbol nil)))

(defgeneric canonical-representation (axis selection)
  (:documentation "Canonical representation of SELECTION, given information in AXIS. The default methods use dimensions as AXIS.

Each selection needs to be resolved into a canonical representation, which is either a singleton, a range, or a sequence of subscripts.  They should only be constructed with the corresponding CANONICAL-SINGLETION, CANONICAL-RANGE and CANONICAL-SEQUENCE functions.

@c(CANONICAL-REPRESENTATION) needs to ensure that the represented subscripts are valid for the axis.

Unless a specialized method is found, the dimension of the axis is queried with AXIS-DIMENSION and resolution is attempted using the latter.  Methods that resolve symbols should test them with SELECT-RESERVED-SYMBOL? and use CALL-NEXT-METHOD.")

  (:method (axis selection)
    ;; fallback: try to get dimension and proceed based that
    (canonical-representation (axis-dimension axis) selection))
  
  ;;; TODO: Should canonical representations resolve to themselves unchecked?
  (:method (axis (canonical-range canonical-range))
   (declare (ignore axis))			; Silence compiler warnings
    canonical-range)

  (:method (axis (canonical-sequence canonical-sequence))
   (declare (ignore axis))			; Silence compiler warnings
    canonical-sequence)

  ;;; DSL for selections
  (:method ((axis integer) (slice null))
    (canonical-singleton axis))

  (:method ((axis integer) (selection integer))
    (canonical-singleton
     (if (minusp selection)
         (aprog1 (+ axis selection)
           (assert (<= 0 it)))
         (aprog1 selection
           (assert (<= selection axis)))))) ; SN:20171104: Changing < to <= fixes item 2 of issue 3 in Papp's repository.

  (:method (axis (selection sequence)) ;SN:20171130 Added ability to pass in lists too
    (let+ (subscripts
           ((&flet collect (value)
              (push value subscripts))))
      (loop for s across (coerce selection 'vector)
            do (aetypecase (canonical-representation axis s)
                 (array-index
                  (collect it))
                 (canonical-range		
                  (loop for index
                        from (canonical-range-start it)
                        below (canonical-range-end it)
                        do (collect index)))
                 (canonical-sequence		;SN:20171209: Genera claims this clause can never be reached.
                  (map 'nil #'collect (canonical-sequence-vector it)))))
      (canonical-sequence (nreverse subscripts))))


  (:method ((axis integer) (selection (eql t)))	
    (canonical-range 0 axis))

  (:method (axis (selection bit-vector))
   (declare (ignore axis))			; Silence compiler warnings
    (canonical-sequence (loop for bit across selection
                              for index from 0
                              when (plusp bit) collect index))))

(defun canonical-representations (axes selections)
  "Return the canonical representations of SELECTIONS given the corresponding AXES, checking for matching length."
  (assert (length= axes selections)) ;SN:20171209: Genera claims this form will never be executed.
  (mapcar #'canonical-representation axes selections))



;;;;
;;;; Iterating over selections
;;;;

(defun singleton-representation? (representation)
  "Test if a canonical REPRESENTATION is a singleton."
  (integerp representation))

(defun all-singleton-representations? (representations)
  "Test if all canonical representations are singletons."
  (every #'singleton-representation? representations))

(defun representation-dimension (representation)
  "Return the dimension of a canonical-representation, or NIL for singleton selections (they are dropped)."
  (aetypecase representation
    (array-index nil)
    (canonical-range (- (canonical-range-end it) (canonical-range-start it)))
    (canonical-sequence (length (canonical-sequence-vector it)))))

(defun representation-dimensions (representations)
  "Return a list for the dimensions of canonical representations, dropping singletons."
  (loop for r in representations
        for d = (representation-dimension r)
        when d collect d))

(defun representation-initial-value (representation)
  "Initial value for iteration."
  (aetypecase representation
    (array-index it)
    (canonical-range (canonical-range-start it))
    (canonical-sequence (aref (canonical-sequence-vector it) 0))))

(defun representation-iterator (representation carry cons)
  "Return a closure that sets the car of CONS to the next value each time it is called, resetting and calling CARRY when it reaches the end of its range."
  (flet ((save (value)
           (setf (car cons) value))
         (carry ()
           (funcall carry)))
    (aetypecase representation
      (array-index carry)
      (canonical-range (let+ (((&structure-r/o canonical-range- start end) it)
                              (selection start))
                         (lambda ()
                           (let ((carry? (= (incf selection) end)))
                             (when carry?
                               (setf selection start))
                             (save selection)
                             (when carry?
                               (carry))))))
      (canonical-sequence (let* ((vector (canonical-sequence-vector it))
                                 (dimension (length vector))
                                 (position 0))
                            (lambda ()
                              (when (= (incf position) dimension)
                                (setf position 0)
                                (carry))
                              (save (aref vector position))))))))

(defun row-major-setup (representations terminator)
  "Return SUBSCRIPTS (a list) and ITERATOR (a closure, no arguments) that increments the contents of SUBSCRIPTS in row-major order.  TERMINATOR is called when all subscripts have been visited."
  (let ((iterator terminator)
        (subscripts (mapcar #'representation-initial-value representations)))
    (loop for r in representations
          for cons on subscripts
          do (setf iterator
                   (representation-iterator r iterator cons)))
    (values subscripts iterator)))

(defun column-major-setup (representations terminator)
  "Return SUBSCRIPTS (a list) and ITERATOR (a closure, no arguments) that increments the contents of SUBSCRIPTS in column-major order.  TERMINATOR is called when all subscripts have been visited."
  (let+ (((&values subscripts iterator)
          (row-major-setup (reverse representations) terminator)))
    (values (nreverse subscripts) iterator)))

(defmacro traverse-representations ((subscripts representations
						&key index
						(setup 'row-major-setup))
                                    &body body)
  "Loops over all possible subscripts in REPRESENTAITONS, making them available in SUBSCRIPTS during the execution of BODY.  The iterator is constructed using the function SETUP (see for example ROW-MAJOR-SETUP).  When INDEX is given, a variable with that name is provided, containing an index that counts iterations."
  (with-unique-names (block-name next)
    `(block ,block-name
       (let+ (((&values ,subscripts ,next)
               (,setup ,representations (lambda () (return-from ,block-name)))))
         (loop ,@(when index `(for ,index from 0))
               do ,@body
                  (funcall ,next))))))
