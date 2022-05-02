
### Status

[![GitHub release](https://img.shields.io/github/release/jacobwilliams/mersenne-twister-fortran.svg)](https://github.com/jacobwilliams/mersenne-twister-fortran/releases/latest)
[![Build Status](https://github.com/jacobwilliams/mersenne-twister-fortran/actions/workflows/CI.yml/badge.svg)](https://github.com/jacobwilliams/mersenne-twister-fortran/actions)
[![codecov](https://codecov.io/gh/jacobwilliams/mersenne-twister-fortran/branch/master/graph/badge.svg)](https://codecov.io/gh/jacobwilliams/mersenne-twister-fortran)
[![last-commit](https://img.shields.io/github/last-commit/jacobwilliams/mersenne-twister-fortran)](https://github.com/jacobwilliams/mersenne-twister-fortran/commits/master)

### Description

Mersenne Twister pseudorandom number generator.

### License

The sourcecode is released under a [permissive BSD-style license](https://github.com/jacobwilliams/mersenne-twister-fortran/blob/master/LICENSE).

### Building mersenne-twister-fortran

The library can be built with the [Fortran Package Manager](https://github.com/fortran-lang/fpm) using the provided `fpm.toml` file like so:

```bash
fpm build --release
```

To use mersenne-twister-fortran within your fpm project, add the following to your `fpm.toml` file:

```yml
[dependencies]
mersenne-twister-fortran = { git="https://github.com/jacobwilliams/mersenne-twister-fortran.git" }
```

### Documentation

  The latest API documentation can be found [here](https://jacobwilliams.github.io/mersenne-twister-fortran/). This was generated from the source code using [FORD](https://github.com/Fortran-FOSS-Programmers/ford).

### References
  * T. Nishimura, "[Tables of 64-bit Mersenne Twisters](https://dl.acm.org/doi/10.1145/369534.369540)" ACM Transactions on Modeling and Computer Simulation 10. (2000) 348--357.
  * M. Matsumoto and T. Nishimura, "[Mersenne Twister: a 623-dimensionally equidistributed uniform pseudorandom number generator](https://dl.acm.org/doi/10.1145/272991.272995)" ACM Transactions on Modeling and Computer Simulation 8. (Jan. 1998) 3--30.
  * [Original soucecode](http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/VERSIONS/FORTRAN/mt19937-64.f95) from math.sci.hiroshima-u.ac.jp
  * [pikaia](https://github.com/jacobwilliams/pikaia) -- this module was originally created for that library, but was split off into a separate package.
