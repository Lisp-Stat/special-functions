
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
  <a href="https://github.com/lisp-stat/special-functions">
    <img src="https://lisp-stat.dev/images/stats-image.svg" alt="Logo" width="80" height="80">
  </a>

  <h3 align="center">Special Functions</h3>

  <p align="center">
  Common lisp implementations of special functions
	<br />
    <a href="https://lisp-stat.dev/docs/resources/special-functions"><strong>Explore the docs »</strong></a>
    <br />
    <br />
    <a href="https://github.com/lisp-stat/special-functions/issues">Report Bug</a>
    ·
    <a href="https://github.com/lisp-stat/special-functions/issues">Request Feature</a>
    ·
    <a href="https://lisp-stat.github.io/special-functions/">Reference Manual</a>
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

[Wikipedia](https://en.wikipedia.org/wiki/Special_functions) describes
special functions as:

> particular mathematical functions that have more or less established names and notations due to their importance in mathematical analysis, functional analysis, geometry, physics, or other applications. The term is defined by consensus, and thus lacks a general formal definition ...

This library implements those special functions required to support
statistical distributions.

### Built With

* [float-features](https://github.com/Shinmera/float-features)
* [numerical-utilities](https://github.com/lisp-stat/numerical-utilities)


<!-- GETTING STARTED -->
## Getting Started

To get a local copy up and running follow these steps:

### Prerequisites

An ANSI Common Lisp implementation. Developed and tested with
[SBCL](https://www.sbcl.org/).

### Installation

#### Automated Installation

If you have [Quicklisp](https://www.quicklisp.org/beta/) installed,
you can load `special-functions` and all of its dependencies with:

```lisp
(ql:quickload :special-functions)
```


#### Manual Installation
To make the system accessible to [ASDF](https://common-lisp.net/project/asdf/) (a build facility, similar to `make` in the C world), clone the repository in a directory ASDF knows about.  By default the `common-lisp` directory in your home directory is known. Create this if it doesn't already exist and then:

1. Clone the repositories
```sh
cd ~/common-lisp && \
git clone https://github.com/Lisp-Stat/special-functions.git && \
git clone https://github.com/Lisp-Stat/numerical-utilities.git && \
git clone https://github.com/Shinmera/float-features.git
```
2. Reset the ASDF source-registry to find the new system (from the REPL)
   ```lisp
   (asdf:clear-source-registry)
   ```
3. Load the system
   ```lisp
   (asdf:load-system :lisp-stat)
   ```

If you have installed the slime ASDF extensions, you can invoke this
with a comma (',') from the slime REPL.

#### Getting dependencies

To get the third party systems that these system may depend on, you can use a dependency manager, such as [Quicklisp](https://www.quicklisp.org/beta/) or [CLPM](https://www.clpm.dev/) Once installed, get the dependencies with either of:

```lisp
(clpm-client:sync :sources "clpi") ;sources may vary
```

```lisp
(ql:quickload :special-functions)
```

You need do this only once. After obtaining the dependencies, you can
load the system with `ASDF` as described above without first syncing
sources.


<!-- USAGE EXAMPLES -->
## Usage

Obtain a log-gamma value

```lisp
(spfn:log-gamma -9.99999237060546875d0) ;=> -3.3208925610275326d0
```

For more examples, please refer to the
[Documentation](https://lisp-stat.dev/docs/resources/special-functions).


<!-- ROADMAP -->
## Roadmap

See the [open issues](https://github.com/lisp-stat/special-functions/issues) for a list of proposed features (and known issues).

## Resources

This system is part of the [Lisp-Stat](https://lisp-stat.dev/)
project; that should be your first stop for information. Also see the
[community](https://lisp-stat.dev/community) page for more
information.

<!-- CONTRIBUTING -->
## Contributing

Contributions are what make the open source community such an amazing place to be learn, inspire, and create. Any contributions you make are greatly appreciated.  Please see [CONTRIBUTING.md](CONTRIBUTING.md) for details on the code of conduct, and the process for submitting pull requests.

<!-- LICENSE -->
## License

Distributed under the MS-PL License. See [LICENSE](LICENSE) for more information.



<!-- CONTACT -->
## Contact

Project Link: [https://github.com/lisp-stat/special-functions](https://github.com/lisp-stat/special-functions)



<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[contributors-shield]: https://img.shields.io/github/contributors/lisp-stat/special-functions.svg?style=for-the-badge
[contributors-url]: https://github.com/lisp-stat/special-functions/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/lisp-stat/special-functions.svg?style=for-the-badge
[forks-url]: https://github.com/lisp-stat/special-functions/network/members
[stars-shield]: https://img.shields.io/github/stars/lisp-stat/special-functions.svg?style=for-the-badge
[stars-url]: https://github.com/lisp-stat/special-functions/stargazers
[issues-shield]: https://img.shields.io/github/issues/lisp-stat/special-functions.svg?style=for-the-badge
[issues-url]: https://github.com/lisp-stat/special-functions/issues
[license-shield]: https://img.shields.io/github/license/lisp-stat/special-functions.svg?style=for-the-badge
[license-url]: https://github.com/lisp-stat/special-functions/blob/master/LICENSE
[linkedin-shield]: https://img.shields.io/badge/-LinkedIn-black.svg?style=for-the-badge&logo=linkedin&colorB=555
[linkedin-url]: https://www.linkedin.com/company/symbolics/
