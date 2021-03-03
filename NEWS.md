# centile 0.10.0

* Returns `NA` for `NULL` `refcode`

# centile 0.9.0

* Adds `refcodes` to `sysdata.rda`
* Removes superfluous `R/data.R`

# centile 0.8.1

* Rounds vectors `y` and `z` explicitly

# centile 0.8.0

* Adds `make_agegrid()` to calculate age grids
* Adds an article looking into more compact WHO Standards
* Make package more self-standing

# centile 0.7.0 

* Adds specification of RIF format in vignette

# centile 0.6.1

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

# yzy 0.5.1

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
