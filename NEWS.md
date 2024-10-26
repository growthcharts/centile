# centile 0.15.0

* Adds `calculate_centile_table()` for generating a table of centiles from a reference

# centile 0.14.2

* Sets `RoxygenNote` to 7.3.1

# centile 0.14.1

* Replaces `BCT()` by robust internal `pBCT()` and `qBCT()`

# centile 0.14.0

* Adds the `BCT` distribution to the validation code
* Removes `renv`

# centile 0.13.0

* Adds `renv` package management
* Revert back to `readr 1.4.0` from CRAN

# centile 0.12.0

* Make dependent on `readr 1.9.9` in order to evade GHA error `/lib/x86_64-linux-gnu/libm.so.6: version 'GLIBC_2.29' not found (required by /home/runner/work/_temp/Library/readr/libs/readr.so)`

# centile 0.11.0

* Switches on continuous integration
* Adds Github action `pkgdown`
* Adds Github action `R-CMD-check`
* Removes `docs` folder in favour of github-pages branch

# centile 0.10.0

* Returns `NA` for `NULL` `refcode`

# centile 0.9.0

* Adds `refcodes` to `sysdata.rda`
* Removes superfluous `R/data.R`
* Rounds vectors `y` and `z` explicitly

# centile 0.8.0

* Adds `make_agegrid()` to calculate age grids
* Adds an article looking into more compact WHO Standards
* Make package more self-standing

# centile 0.7.0 

* Adds specification of RIF format in vignette
* Improves documentation for WHO standard
* Tweak examples so as to use only internal data
* Introduce informal S3 class `reference`
* Make argument `refcode` accept object of class `reference`

# centile 0.6.0

* Major rewrite including several breaking changes
* Renames the package to `centile`
* Renames `y()` and `z()` to `z2y()` and `y2z()`
* Introduces `y2p()` and `p2y()`
* Renames `read_ref()` to `import_rif()`
* Allow for scalar `x` and `refcode` arguments in `z()` and `y()`

# yzy 0.5.0

* Adds `rule` argument to `z()` and `y()`

# yzy 0.4.0

* Adds `pkg` and `verbose` arguments to `z()`, `y()` and `load_reference()`
* Weeds out imports

# yzy 0.3.0

* Extends functionality of `load_reference()` to loaded packages
* Adds test for `load_reference()`
* Clean-up `globalVariables()`

# yzy 0.2.0

* Includes basic functionality
* Adds a `NEWS.md` file to track changes to the package.

# yzy 0.1.0

* First commit from template
