
<!-- README.md is generated from README.Rmd. Please edit that file -->

# centile

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/centile)](https://CRAN.R-project.org/package=centile)
[![R-CMD-check](https://github.com/growthcharts/centile/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/growthcharts/centile/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The `centile` package

- Defines the reference interchange format (RIF), a simple text format
  for exchanging growth references;
- Provides tools for reading and validating RIF files;
- Names and loads references as `R` objects;
- Converts between measurements, Z-scores and centiles;
- Contains built-in WHO Multicentre Growth Standard for height, weight,
  head circumference and body mass index.

The currently supported distributions are normal (`NO`), Lambda-Mu-Sigma
(`LMS`), Box-Cox Green Cole (`BCCG`), Box-Cox Power Exponential (`BCPE`)
and Box-Cox t (`BCT`).

## Installation

You can install the development version of `centile` with:

``` r
remotes::install_github("growthcharts/centile")
```

## Examples
