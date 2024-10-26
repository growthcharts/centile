#' Calculate a centile table from a given reference
#'
#' This function calculates centiles based on specified z-scores or percentiles
#' using a given reference. It requires the reference to be provided as a code
#' compatible with the `centile` package.
#'
#' @param z A vector containing the z values of the desired centiles.
#' @param p A vector containing the desired percentile values (between 0 and
#'   100). The default `p = NULL` does not calculate percentiles.
#' @param x A vector with the grid (usually years, sometimes cm) on the
#'   horizontal axis.
#' @param refcode A reference code according to the specifications of the
#'   `centile` package. This code should match a pre-defined reference table
#'   containing LMS values for various age or measurement groups.
#' @param dec Number of decimals needed for the output.
#' @param \dots Additional arguments passed down to `z2y()`.
#' @return A `matrix` containing the desired centiles, with columns named
#'   according to either the z-score values (e.g., `Z-2`, `Z0`, `Z2`) or
#'   percentiles (e.g., `P3`, `P50`, `P97`).
#' @examples
#' # Centile table from WHO weight standard for boys aged 0-5 years
#' calculate_centile_table(z = -2:2, x = 0:5, refcode = "who_2006_wgt_male_")
#' @export
calculate_centile_table <- function(
    x,
    z = NULL,
    p = NULL,
    refcode = NULL,
    dec = 4,
    ...) {

  # Calculate centiles z
  if (!is.null(z)) {
    w <- centile::z2y(
      z = rep(z, each = length(x)),
      x = rep(x, times = length(z)),
      ref = refcode,
      dec = dec,
      drop = TRUE, ...
    )
    w <- matrix(round(w, dec), ncol = length(z))
    dimnames(w) <- list(NULL, paste0("Z", as.character(z)))
    if (is.null(p)) {
      return(w)
    }
  }

  # Calculate percentiles p
  if (!is.null(p)) {
    if (min(p) <= 0 | max(p) >= 100) stop("Percentiles must be between 0 and 100.")
    pr <- qnorm(p / 100)
    v <- centile::z2y(
      z = rep(pr, each = length(x)),
      x = rep(x, times = length(pr)),
      ref = refcode,
      dec = dec,
      drop = TRUE, ...
    )
    v <- matrix(round(v, dec), ncol = length(p))
    dimnames(v) <- list(NULL, paste0("P", as.character(p)))

    if (is.null(z)) {
      return(v)
    }
  }

  # Return combined centiles for z and p if both provided
  return(cbind(w, v))
}
