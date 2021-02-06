
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

You can install the released version of yzy from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("yzy")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(yzy)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist    
#>  Min.   : 4.0   Min.   :  2  
#>  1st Qu.:12.0   1st Qu.: 26  
#>  Median :15.0   Median : 36  
#>  Mean   :15.4   Mean   : 43  
#>  3rd Qu.:19.0   3rd Qu.: 56  
#>  Max.   :25.0   Max.   :120
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/master/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
