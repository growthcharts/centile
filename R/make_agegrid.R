#' Creates an age grid according to a specified format
#'
#' @param grid A character string specifying one of the following:
#' `"compact"`, `"classic"`, `"extensive"`, `"0-104w"`,
#' `"0-24m"` and `"0-21y"`.  The
#' default produces an age grid between 0 and 21
#' years with 95 points.
#' @param unit Character string specifying the time unit. Choices
#' are `"year"`, `"month"`, `"week"` and `"day"`.
#' @param dec Number of decimals for the result.
#' @return Numeric vector or `NULL`
#' @author Stef van Buuren, 2021
#' @examples
#' head(make_agegrid("classic"))
#' @export
make_agegrid <- function(grid = c("compact", "classic", "extensive", "0-104w", "0-24m", "0-21y"),
                         unit = c("year", "month", "week", "day"),
                         dec = 4L) {
  grid <- match.arg(grid)
  unit <- match.arg(unit)
  a <- switch(grid,
    compact =
      c(
        (0:14) / 365.25,
        (3:13) * 7 / 365.25,
        seq(3, 11.5, 0.5) / 12,
        (12:23) / 12,
        seq(2, 21, 0.5)
      ),
    classic = {
      grid_weeks <- c(
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 14, 16, 18, 20, 22, 24, 26,
        28, 32, 36, 40, 44, 48, 52, 56, 60, 64
      )
      c(grid_weeks * 7 / 365.25, seq(1.5, 21, 0.5))
    },
    extensive = (0:(365.25 * 21 + 1)) / 365.25,
    `0-104w` = (0:104) * 7 / 365.25,
    `0-24m` = (0:24) / 12,
    `0-21y` = 0:21,
  )

  switch(unit,
    year = round(a, dec),
    month = round(a * 12, dec),
    week = round(a * 365.25 / 7, dec),
    day = round(a * 365.25, dec),
    NULL
  )
}
