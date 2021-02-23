#' Convert centiles into measurements
#'
#' Converts centile scores into measurements
#' (KG, CM, ...) using one or more external reference distributions.
#'
#' @note The type of reference distribution is defined by the `distribution` field
#' of the `study` attribute in the reference. The function executes
#' transformations specified by the `tx` field (before calculation of `y`) and
#' the `yt` field (after calculating of `y`), so that the `y` have the original
#' units even if the reference is based on some transform of `y`.
#'
#' Functions \code{p2y()} and \code{y2p()} functions are the inverse of each other.
#'
#' @inheritParams p2z
#' @inheritParams y2z
#' @inheritParams stats::approx
#' @return A vector with `length(p)` elements containing the measurements.
#' @author Stef van Buuren, 2021
#' @examples
#' # Obtain the 10th, 50th and 90th height centile for a boy aged 1 yr on WHO
#' p2y(c(10, 50, 90), rep(1, 3), "who_2011_hgt_male_")
#' @export
p2y <- function(p, x, refcode, pkg = "centile", verbose = FALSE,
                dec = 3L, rule = 1L, tail_adjust = FALSE,
                scale = 100, ...) {
  z2y(z = p2z(p, scale = scale),
      x = x, refcode = refcode, pkg = pkg, verbose = verbose,
      dec = dec, rule = rule, tail_adjust = tail_adjust, ...)
}
