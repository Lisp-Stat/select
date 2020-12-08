# Selections from Arrays and Vectors

This library provides:

1. An API for taking slices (elements selected by the Cartesian product of vectors of subscripts for each axis) of array-like objects. The most important function is `select`. Unless you want to define additional methods for `select`, this is pretty much all you need from this library. See the [documentation](https://symbolics.github.io/select/) for a tutorial.
2. An extensible DSL for selecting a subset of valid subscripts. This is useful if, for example, you want to resolve column names in a data frame in your implementation of slice.
3. A set of utility functions for traversing slices in array-like objects.

# Documentation
Documentation for select can be found at the [Symbolics github.io page](https://symbolics.github.io/select/).

# Known Issues

`Select` is known to pass all tests on SBCL and ACL (Allegro Common Lisp), and fails two tests on CCL related to vector/array representations. The specifics are described in [issue #3](https://github.com/Symbolics/select/issues/3). I suspect this to be a bug somewhere in CCL. Version 1.12 of CCL doesn't run on MS Windows, so the obvious next step is not available (install 1.12 and try). It could also be specific to MS Windows. If someone successfully gets this to run on another platform, I'd appreciate hearing about it.

# Changes from cl-slice

## Version 1.0

### Documentation Improvements
- Move to HTML based documentation system
- Docs now on github.io
- Documented selection iteration
- Improved examples and explanations

### Test Improvements
- Ported to FiveAM and refactored
- Improved test coverage
- Added failure messages to aid debugging
- Added tests for selection iteration

### Enhancements
- Renamed 'cons' to 'range'
- Range now handles (range x x) => nil
- Selections work identically on sequences; previously differed between lists and vectors
- Selections may be specified using a list; previously could only be a vector
- Sequence selections now honor fill-pointer (fixes first issue on [Papp's issue 3](https://github.com/tpapp/cl-slice/issues/3)

### Bug Fixes
- Range now handles END = (length <sequence>)
- Selecting from a list no longer drops dimension

