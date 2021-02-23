#' Convert centiles into Z-scores
#'
#' Functions `z2p()` and `p2z()` functions are the inverse of each other.
#'
#' @param p A numerical vector with centiles.
#' @param scale The maximum of the scale. The default (`100`) returns for
#' percentiles. Set `scale = 1` to obtain probabilities.
#' @return A vector with `length(p)` elements containing centiles.
#' @author Stef van Buuren, 2021
#' @examples
#' p <- c(2.5, 10, 50, 90, 97.5)
#' p2z(p)
#' @export
p2z <- function(p, scale = 100) {
  qnorm(p / scale)
}

