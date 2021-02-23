#' Convert Z-scores into centiles
#'
#' Functions `z2p()` and `p2z()` functions are the inverse of each other.
#'
#' @param z A numerical vector with Z-scores.
#' @param scale The maximum of the scale. The default (`100`) returns for
#' percentiles. Set `scale = 1` to obtain probabilities.
#' @return A vector with `length(z)` elements containing centiles.
#' @author Stef van Buuren, 2021
#' @examples
#' z <- c(-1.96, -1.28, 0, 1.28, 1.96)
#' z2p(z)
#' @export
z2p <- function(z, scale = 100) {
  pnorm(z) * scale
}

