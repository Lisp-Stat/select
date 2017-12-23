;;;; -*- Mode: LISP; Base: 10; Syntax: ANSI-Common-lisp; Package: CL-USER -*-
;;;; Copyright (c) 2010-2013, 2015-2017 Didier Verna
;;;; Copyright (c) 2017-2018 by Steven Nunez <steve.nunez@inference.sg>

(require "asdf")

(defconstant +copyright-years+ "2017--2018")

(defconstant +introduction+
  "@macro select
@t{Select}
@end macro
@macro cl
Common-Lisp
@end macro
@macro etc
@i{etc.}
@end macro

@select{} is a library for taking slices from array-like objects. The
most frequently used form is:
@lisp
(select object selection1 selection2 ...)
@end lisp
where each @i{selection} specifies a subset of subscripts along the
corresponding axis. The selection specifications are found
below.

@section Selection Specifiers
@heading Selecting Single Values
A non-negative integer selects the corresponding index, while a
negative integer selects an index counting backwards from the last
index. For example:
@lisp
(select #(0 1 2 3) 1)                  ; => 1
(select #(0 1 2 3) -2)                 ; => 2
@end lisp
These are called @i{singleton} slices. Each singleton slice drops the
dimension: vectors become atoms, matrices become vectors, @etc{}.

@heading Selecting Ranges
@code{(range start end)} selects subscripts @i{i} where start <= i <=
end. When end is nil, the last index is included (cf. subseq). Each
boundary is resolved according to the other rules if applicable, so
you can use negative integers:
@lisp
(select #(0 1 2 3) (range 1 3))         ; => #(1 2)
(select #(0 1 2 3) (range 1 -1))        ; => #(1 2)
@end lisp

@heading Selecting All Subscripts of a Dimension
@code{t} selects all subscripts:
@lisp
(select #2A((0 1 2)
            (3 4 5))
         t 1)                           ; => #(1 4)
@end lisp

@heading Selecting with a Sequence
Sequences can be used to make specific selections from the object. For
example:
@lisp
(select #(0 1 2 3 4 5 6 7 8 9)
	(vector (range 1 3) 6 (range -2 -1))) ; => #(1 2 3 6 8 9)
(select #(0 1 2) '(2 2 1 0 0))                ; => #(2 2 1 0 0)
@end lisp

@heading Using Bit Vectors as a Mask
Bit vectors can be used to select elements of arrays and sequences as
well:
@lisp
(select #(0 1 2 3 4) #*00110)          ; => #(2 3)
@end lisp

@subsection Extensions

Section 1.1 describes the core functionality. The semantics can be
extended, as you will see in the next section. The extensions in this
section are provided by the library and prove useful in
practice. Their implementation provides good examples of extending the
library.

@code{including} is convenient if you want the selection to include
the end of the range:
@lisp
(select #(0 1 2 3) (including 1 2))
                          ; => #(1 2), cf. (select ... (range 1 3)) 
@end lisp

@code{nodrop} is useful if you do not want to drop dimensions:
@lisp
(select #(0 1 2 3) (nodrop 2))
                          ; => #(2), cf. (select ... (range 2 3))
@end lisp

@code{head} and @code{tail} do the obvious:
@lisp
(select #(0 1 2 3) (head 2))            ; => #(0 1)
(select #(0 1 2 3) (tail 2))            ; => #(2 3)
@end lisp

All of these are trivial to implement. If there is something you are
missing, you can easily extend @code{select}. Pull request are welcome.

@code{ref} is a version of @code{select} that always returns a single
element, so it can only be used with singleton slices.

@section Select Semantics
Arguments of @code{select}, except the first one, are meant to be
resolved using @code{canonical-representation}, in the
@code{select-dev} package. If you want to extend @code{select}, you
should define methods for @code{canonical-representation}. See the
source code for the best examples. Below is a simple example that
extends the semantics with ordinal numbers.
@lisp
(defmacro define-ordinal-selection (number)
  (check-type number (integer 0))
  `(defmethod select-dev:canonical-representation
       ((axis integer)
	(slice (eql ',(intern (format nil \"~:@@(~:r~)\" number)))))
     (assert (< ,number axis))
     (select-dev:canonical-singleton ,number)))

(define-ordinal-selection 1)
(define-ordinal-selection 2)
(define-ordinal-selection 3)

(select #(0 1 2 3 4 5) (range 'first 'third)) ; => #(1 2)
@end lisp
Note the following:
@itemize @bullet
@item
The value returned by @code{canonical-representation} needs to be
constructed using @code{canonical-singleton}, @code{canonical-range},
or @code{canonical-sequence}. You should not use the internal
representation directly as it is subject to change.
@item
You can assume that @code{axis} is an integer: this is the default. An
object may define a more complex mapping (such as, for example, named
rows & columns), but unless a method specialized to that is found,
@code{canonical-representation} will just query its dimension (with
@code{axis-dimension}) and try to find a method that works on
integers.
@item
You need to make sure that the subscript is valid, hence the assertion.
@end itemize
")

(defconstant +change-log+
  "@select{} was originally called
@uref{https://github.com/tpapp/cl-slice, slice} and written by Tamas
K. Papp. Since it was
@uref{https://tpapp.github.io/post/orphaned-lisp-libraries/, abandoned
in 2017}, I have taken it over to be part of a rebooted Common Lisp
statistics library. Changes in this version include:

@heading Documentation Improvements
@itemize @bullet
@item Move to HTML based documentation system
@item Docs now on github.io
@end itemize

@heading Test Improvements
@itemize @bullet
@item Ported to FiveAM and refactored
@item Improved test coverage 
@item Added failure messages to aid debugging
@item Added tests for selection iteration
@end itemize

@heading Enhancements
@itemize @bullet
@item Renamed to 'cons' to 'range'
@item Range now handles (range x x) => nil
@item Selections work identically on sequences; previously differed between lists and vectors
@item Selections may be specified using a list; previously could only be a vector
@item Sequence selections now honor fill-pointer, if any
@end itemize

@heading Bug Fixes
@itemize @bullet
@item Range now handles END = (length <sequence>)
@item Selecting from a list no longer drops dimension
@end itemize
")

(asdf:load-system :net.didierverna.declt)
(net.didierverna.declt:nickname-package)

(declt:declt :select
	     :library "Select"
	     :copyright +copyright-years+
	     :license :boost
	     :declt-notice nil
	     :introduction +introduction+
	     :conclusion +change-log+
	     :texi-file "select.texi"
	     :hyperlinks t)
(uiop:quit)
