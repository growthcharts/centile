
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

Generate a reference table for the WHO Multicentre Growth Standard for
weight of boys aged 0-5 years:

``` r
library(centile)
calculate_centile_table(x = 0:5, z = -2:2, refcode = "who_2006_wgt_male_")
#>        Z-2   Z-1    Z0    Z1    Z2
#> [1,]  2.46  2.88  3.35  3.86  4.42
#> [2,]  7.74  8.65  9.65 10.76 11.99
#> [3,]  9.67 10.84 12.15 13.62 15.28
#> [4,] 11.28 12.71 14.34 16.20 18.31
#> [5,] 12.71 14.40 16.35 18.59 21.18
#> [6,] 14.07 16.04 18.34 21.02 24.16
```

We obtain percentiles as follows:

``` r
library(centile)
calculate_centile_table(x = 0:5, p = c(10, 50, 90), refcode = "who_2006_wgt_male_")
#>        P10   P50   P90
#> [1,]  2.76  3.35  4.01
#> [2,]  8.38  9.65 11.09
#> [3,] 10.50 12.15 14.07
#> [4,] 12.29 14.34 16.77
#> [5,] 13.90 16.35 19.28
#> [6,] 15.45 18.34 21.86
```
