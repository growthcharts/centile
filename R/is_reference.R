#' Test whether object is a reference
#'
#' @param x An R object, typically produced by `import_rif()`. The object is
#' a data frame with an attribute named `study`.
#' @returns Logical
#' @examples
#' is_reference("this is a string")
#'
#' # import reference and check
#' fn <- system.file("testdata/nl_2009_hgt_female_nl.txt", package = "centile")
#' myref <- import_rif(fn)
#' is_reference(myref)
#' @export
is_reference <- function(x) {
  study <- attr(x, "study")
  is.data.frame(x) && !is.null(study)
}
