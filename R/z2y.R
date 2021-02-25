#' Convert Z-scores into measurements
#'
#' Converts age- or hgt-conditional standard deviation scores into measurements
#' (KG, CM, ...) using one or more external reference distributions.
#'
#' @note The type of reference distribution is defined by the `distribution` field
#' of the `study` attribute in the reference. The function executes
#' transformations specified by the `tx` field (before calculation of `y`) and
#' the `yt` field (after calculating of `y`), so that the `y` have the original
#' units even if the reference is based on some transform of `y`.
#'
#' Functions \code{y2z()} and \code{z2y()} functions are the inverse of each other.
#'
#' @param z A numerical vector with Z-scores.
#' @inheritParams y2z
#' @inheritParams stats::approx
#' @return A vector with `length(z)` elements containing the measurements.
#' @author Stef van Buuren, 2021
#' @examples
#' # height centiles SD -2:2 according to WHO standard for 6 month year old girl
#' # using built-in reference
#' z2y(z = -2:2, x = rep(0.5, 5), "who_2006_hgt_female_", pkg = "centile")
#'
#' # same using external Dutch reference
#' fn <- system.file("testdata/nl_2009_hgt_female_nl.txt", package = "centile")
#' myref <- import_rif(fn)
#' head(myref)
#' head(attr(myref, "study"))
#' z2y(z = -2:2, x = rep(0.5, 5), myref)
#'
#' # find location of -2 SD line for 6 months old boys and girls, WHO Standard
#' z2y(c(-2, -2), c(0.5, 0.5), c("who_2006_hgt_male_", "who_2006_hgt_female_"))
#' @export
z2y <- function(z, x, refcode, pkg = "centile", verbose = FALSE,
                dec = 3L, rule = 1L, tail_adjust = FALSE, ...) {
  if (length(z) != length(x) && length(x) > 1L) {
    message("z2y(): Non-conformable arguments z and x")
    return(rep(NA_real_, length(z)))
  }

  # if refcode is a reference, use that for all z and x
  if (is_reference(refcode)) {
    y <- y_grp(z = z, x = x, refcode = refcode, verbose = verbose,
               pkg = pkg, rule = rule, tail_adjust = tail_adjust)
    return(round(y, digits = dec))
  }

  if (is.data.frame(refcode)) {
    message("z2y(): Argument refcode has no `study` attribute.")
    return(rep(NA_real_, length(z)))
  }

  if (length(z) != length(refcode) && length(refcode) > 1L) {
    message("z2y(): Non-conformable arguments z and refcode")
    return(rep(NA_real_, length(z)))
  }
  if (!length(z)) {
    return(numeric(0))
  }

  # alternatively, split calculations by refcode
  y <- data.frame(z = z, x = x, refcode = refcode) %>%
    group_by(.data$refcode) %>%
    mutate(y = y_grp(
      z = .data$z,
      x = .data$x,
      refcode = first(.data$refcode),
      verbose = verbose,
      pkg = pkg,
      rule = rule,
      tail_adjust = tail_adjust
    )) %>%
    ungroup() %>%
    pull(.data$y)

  round(y, digits = dec)
}

y_grp <- function(z, x, refcode, pkg, verbose, rule, tail_adjust = FALSE) {
  r <- load_reference(refcode = refcode, pkg = pkg, verbose = verbose)

  # do not process in absence of study attribute
  study <- attr(r, "study")
  if (is.null(study)) {
    return(rep(NA_real_, length(z)))
  }

  # handle transforms, if any
  yt <- function(y, study) {
    if ("yt" %in% names(study)) {
      return(eval(parse(text = study[["yt"]])))
    }
    y
  }
  if ("tx" %in% names(study)) {
    x <- eval(parse(text = study[["tx"]]))
  }

  dist <- toupper(study[["distribution"]])

  if (dist == "NO") {
    check_names(df = r, needed = c("x", "mean", "sd"))
    mean <- approx(x = r[["x"]], y = r[["mean"]], xout = x, rule = rule)$y
    sd <- approx(x = r[["x"]], y = r[["sd"]], xout = x, rule = rule)$y
    return(yt(mean + z * sd, study))
  }
  if (dist == "LMS") {
    check_names(df = r, needed = c("x", "L", "M", "S"))
    L <- approx(x = r[["x"]], y = r[["L"]], xout = x, rule = rule)$y
    M <- approx(x = r[["x"]], y = r[["M"]], xout = x, rule = rule)$y
    S <- approx(x = r[["x"]], y = r[["S"]], xout = x, rule = rule)$y
    y <- ifelse(L > 0.01 | L < (-0.01), M * (1 + L * S * z)^(1 / L), M * exp(S * z))
    if (tail_adjust) y <- adjust_tail_y(y, z, L, M, S)
    return(yt(y, study))
  }
  if (dist == "BCCG") {
    check_names(df = r, needed = c("x", "nu", "mu", "sigma"))
    nu <- approx(x = r[["x"]], y = r[["nu"]], xout = x, rule = rule)$y
    mu <- approx(x = r[["x"]], y = r[["mu"]], xout = x, rule = rule)$y
    sigma <- approx(x = r[["x"]], y = r[["sigma"]], xout = x, rule = rule)$y
    return(yt(qBCCG(pnorm(z), mu = mu, sigma = sigma, nu = nu), study))
  }
  if (dist == "BCPE") {
    check_names(df = r, needed = c("x", "nu", "mu", "sigma", "tau"))
    mu <- approx(x = r[["x"]], y = r[["mu"]], xout = x, rule = rule)$y
    sigma <- approx(x = r[["x"]], y = r[["sigma"]], xout = x, rule = rule)$y
    nu <- approx(x = r[["x"]], y = r[["nu"]], xout = x, rule = rule)$y
    tau <- approx(x = r[["x"]], y = r[["tau"]], xout = x, rule = rule)$y
    return(yt(qBCPE(pnorm(z), mu = mu, sigma = sigma, nu = nu, tau = tau), study))
  }
  if (dist == "BCT") {
    check_names(df = r, needed = c("x", "nu", "mu", "sigma", "tau"))
    mu <- approx(x = r[["x"]], y = r[["mu"]], xout = x, rule = rule)$y
    sigma <- approx(x = r[["x"]], y = r[["sigma"]], xout = x, rule = rule)$y
    nu <- approx(x = r[["x"]], y = r[["nu"]], xout = x, rule = rule)$y
    tau <- approx(x = r[["x"]], y = r[["tau"]], xout = x, rule = rule)$y
    return(yt(qBCT(pnorm(z), mu = mu, sigma = sigma, nu = nu, tau = tau), study))
  }

  stop(paste("Reference type", dist, "not implemented."))
}


adjust_tail_y <- function(y, z, L, M, S) {
  stop("Tail adjustmunt for z2y() not implemented.")
}
