
<!-- PROJECT SHIELDS -->

[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![MS-PL License][license-shield]][license-url]
[![LinkedIn][linkedin-shield]][linkedin-url]



<!-- PROJECT LOGO -->
<br />
<p align="center">
  <a href="https://github.com/lisp-stat/select">
    <img src="http://www.lisp-stat.dev/images/stats-image.svg" alt="Logo" width="80" height="80">
  </a>

  <h3 align="center">Select</h3>

  <p align="center">
	Selecting subsets of data from arrays, vectors and data-frames
	<br />
    <a href="https://lisp-stat.dev/docs/manuals/select"><strong>Explore the docs »</strong></a>
    <br />
    <br />
    <a href="https://github.com/lisp-stat/select/issues">Report Bug</a>
    ·
    <a href="https://github.com/lisp-stat/select/issues">Request Feature</a>
    ·
	<a href="https://lisp-stat.github.io/select/">Reference Manual</a>
  </p>
</p>



<!-- TABLE OF CONTENTS -->
<details open="open">
  <summary><h2 style="display: inline-block">Table of Contents</h2></summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
      <ul>
        <li><a href="#built-with">Built With</a></li>
      </ul>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
      </ul>
    </li>
    <li><a href="#usage">Usage</a></li>
    <li><a href="#roadmap">Roadmap</a></li>
	<li><a href="#resources">Resources</a></li>
    <li><a href="#contributing">Contributing</a></li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
  </ol>
</details>



<!-- ABOUT THE PROJECT -->
## About the Project

This library provides:

1. An API for taking slices (elements selected by the Cartesian product of vectors of subscripts for each axis) of array-like objects. The most important function is `select`. Unless you want to define additional methods for `select`, this is pretty much all you need from this library. See the [documentation](https://lisp-stat.dev/docs/manuals/select/) for a tutorial.
2. An extensible DSL for selecting a subset of valid subscripts. This is useful if, for example, you want to resolve column names in a data frame in your implementation of select.
3. A set of utility functions for traversing selections in array-like objects.


### Built With

* [anaphora](https://github.com/tokenrove/anaphora)
* [alexandria](https://gitlab.common-lisp.net/alexandria/alexandria)
* [array-operations](https://github.com/bendudson/array-operations)
* [select](https://github.com/Symbolics/select)
* [let-plus](https://github.com/sharplispers/let-plus)



<!-- GETTING STARTED -->
## Getting Started

To get a local copy up and running follow these steps:

### Prerequisites

An ANSI Common Lisp implementation. Developed and tested with
[SBCL](http://www.sbcl.org/) and
[CCL](https://github.com/Clozure/ccl).

### Installation

1. Clone the repository
   ```sh
   cd ~/quicklisp/local-projects &&
   git clone https://github.com/Lisp-Stat/select.git
   ```
2. Reset the ASDF source-registry to find the new system (from the REPL)
   ```lisp
   (asdf:clear-source-registry)
   ```
3. Load the system
   ```lisp
   (ql:quickload :select)
   ```

### Documentation

The API documentation is in the `docs/` directory and is available in
emacs info format, PDF and HTML.  You can also [view the documentation
online](https://lisp-stat.github.io/select/).

<!-- USAGE EXAMPLES -->
## Usage

The most frequently used form is:

```lisp
(select object selection1 selection2 ...)
```

where each selection specifies a set of subscripts along the
corresponding axis.  The selection specifications are found in the
documentation.

For more examples, please refer to the [Reference Manual](https://lisp-stat/github.io/select/)


<!-- ROADMAP -->
## Roadmap

See the [open issues](https://github.com/lisp-stat/select/issues) for a list of proposed features (and known issues).


# Known Issues

`Select` is known to pass all tests on SBCL and ACL (Allegro Common Lisp), and fails two tests on CCL related to vector/array representations. The specifics are described in [issue #3](https://github.com/Symbolics/select/issues/3). I suspect this to be a bug somewhere in CCL. Version 1.12 of CCL doesn't run on MS Windows, so the obvious next step is not available (install 1.12 and try). It could also be specific to MS Windows. If someone successfully gets this to run on another platform, I'd appreciate hearing about it.

## Changes from cl-slice

### Documentation Improvements
- Move to HTML based documentation system
- Reference docs now on github.io
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
- Vector selections may be specified using a list; previously could only be a vector. See also [issue #2](https://github.com/lisp-stat/select/issues/2)
- Sequence selections now honor fill-pointer (fixes first issue on [Papp's issue 3](https://github.com/tpapp/cl-slice/issues/3))

### Bug Fixes
- Range now handles END = (length SEQUENCE)
- Selecting from a list no longer drops dimension



## Resources

This system is part of the [Lisp-Stat](https://lisp-stat.dev/)
project; that should be your first stop for information. Also see the
[resources](https://lisp-stat.dev/resources) and
[community](https://lisp-stat.dev/community) pages for more
information.

<!-- CONTRIBUTING -->
## Contributing

Contributions are what make the open source community such an amazing place to be learn, inspire, and create. Any contributions you make are **greatly appreciated**. Please see [CONTRIBUTING.md](CONTRIBUTING.md) for details on our code of conduct, and the process for submitting pull requests.

<!-- LICENSE -->
## License

Distributed under the MS-PL License. See `LICENSE` for more information.



<!-- CONTACT -->
## Contact

Project Link: [https://github.com/lisp-stat/select](https://github.com/lisp-stat/select)



<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[contributors-shield]: https://img.shields.io/github/contributors/lisp-stat/select.svg?style=for-the-badge
[contributors-url]: https://github.com/lisp-stat/select/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/lisp-stat/select.svg?style=for-the-badge
[forks-url]: https://github.com/lisp-stat/select/network/members
[stars-shield]: https://img.shields.io/github/stars/lisp-stat/select.svg?style=for-the-badge
[stars-url]: https://github.com/lisp-stat/select/stargazers
[issues-shield]: https://img.shields.io/github/issues/lisp-stat/select.svg?style=for-the-badge
[issues-url]: https://github.com/lisp-stat/select/issues
[license-shield]: https://img.shields.io/github/license/lisp-stat/select.svg?style=for-the-badge
[license-url]: https://github.com/lisp-stat/select/blob/master/LICENSE
[linkedin-shield]: https://img.shields.io/badge/-LinkedIn-black.svg?style=for-the-badge&logo=linkedin&colorB=555
[linkedin-url]: https://www.linkedin.com/company/symbolics/
