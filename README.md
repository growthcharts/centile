
<!-- README.md is generated from README.Rmd. Please edit that file -->

# yzy

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/yzy)](https://CRAN.R-project.org/package=yzy)
<!-- badges: end -->

The goal of `yzy` is to

-   Convert measurements (`y`) to Z-scores (`z`);
-   Convert Z-scores (`z`) to measurements (`y`);
-   Provide a simple text format for exchanging reference tables;
-   Validate, name and load references as `R` objects.

The currently supported distributions are normal (`NO`), Lambda-Mu-Sigma
(`LMS`), Box-Cox Green Cole (`BCCG`), Box-Cox Power Exponential (`BCPE`)
and Box-Cox t (`BCT`).

## Installation

You can install the development version of `yzy` with:

``` r
remotes::install_github("growthcharts/yzy")
```
