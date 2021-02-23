#' Convert measurements into centiles
#'
#' Converts measurements into age- or hgt-conditional centile scores using one
#' or more external reference distributions.
#'
#' @note The type of reference distribution is defined by the `distribution` field
#' of the `study` attribute in the reference. The function executes
#' transformations specified by the `tx` and `ty` fields before calculating the
#' Z-score.
#'
#' Functions `y2p()` and `p2y()` functions are the inverse of each other.
#'
#' @inheritParams y2z
#' @param scale The maximum of the scale. The default (`100`) returns
#' percentiles. Set `scale = 1` to obtain probabilities.
#' @return A vector with `length(y)` elements containing centiles.
#' @author Stef van Buuren, 2021
#' @examples
#' y <- c(50, 50, 60, 60, 4, 4, 4, 4)
#' x <- c(rep(0.1, 4), rep(0.1, 4))
#' refcode <- c(
#'   rep(c("who_2006_hgt_male_", "who_2006_hgt_female_"), 2),
#'   rep(c("who_2006_wgt_male_", "who_2006_wgt_female_"), 2)
#' )
#' y2p(y, x, refcode)
#' @export
y2p <- function(y, x, refcode, pkg = "centile", verbose = FALSE,
                dec = 3L, rule = 1L, tail_adjust = FALSE, scale = 100, ...) {
  z2p(y2z(
    y = y, x = x, refcode = refcode, pkg = pkg, verbose = verbose,
    dec = dec, rule = rule, tail_adjust = tail_adjust, ...
  ),
  scale = scale
  )
}
