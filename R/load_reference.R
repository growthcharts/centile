#' Load growth reference
#'
#' This function provides access to the built-in growth references.
#' @param refcode String, code of a reference. Only the first element is loaded.
#' @param element Keyword, either `"all"`, `"table"`, `"index"` or `"study"`.
#' The default is `"all"`.
#' @param pkg The package containing the reference. The package must be loaded
#' and attached. The default `pkg = "yzy"` searches in the home package.
#' @param verbose For more verbose output, set to `TRUE`.
#' @return The return value depends on the `element` parameter.
#'
#' `element` | Return value
#' --------- | ---------------------------------------
#' `"all"`   | All stored information
#' `"table"` | A `tibble` with reference values
#' `"index"` | Numeric vector with index values
#' `"study"` | Named character vector with study data
#'
#' @examples
#' ref <- load_reference("gc_2019_dsc_male_")
#' @export
load_reference <- function(refcode = NULL,
                           element = c("all", "table", "index", "study"),
                           pkg = "yzy",
                           verbose = FALSE) {
  if (is.null(refcode)) {
    return(NULL)
  }
  if (!pkg %in% loadedNamespaces()) {
    if (verbose) warning("Package ", pkg, " not loaded.")
    return(NULL)
  }
  ref <- get0(refcode[[1L]], envir = asNamespace(pkg))
  if (is.null(ref)) {
    if (verbose) warning("Reference ", refcode[[1L]], " not found in package ", pkg, ".")
    return(NULL)
  }

  element <- match.arg(element)
  switch(element,
    all = ref,
    table = {
      attr(ref, "study") <- NULL
      ref
    },
    index = ref[["x"]],
    study = attr(ref, "study")
  )
}
