#' Convert measurements into Z-scores
#'
#' Converts measurements into age- or hgt-conditional standard deviation scores
#' (SDS, Z-score) using one or more external reference distributions.
#'
#' @note The type of reference distribution is defined by the `distribution` field
#' of the `study` attribute in the reference. The function executes
#' transformations specified by the `tx` and `ty` fields before calculating the
#' Z-score.
#'
#' Functions \code{y2z()} and \code{z2y()} functions are the inverse of each other.
#'
#' @param y A numerical vector with measurements`.
#' @param x A vector containing `length(y)` values of the numerical covariate (typically
#' decimal age or height) at which conversion is desired. A scalar `x` will be
#' expanded to `length(y)`.
#' @param refcode A character vector with `length(y)` elements, each of which
#' is the name of a reference. Scalar `refcode` expands to `length(y)`.
#' Alternatively, `refcode` may be an object of class `reference`, typically
#' produced by [import_rif()].
#' @param pkg The package containing the references in the `R/sysdata.dta` object.
#' The package needs to be loaded. The default `pkg = "centile"` searches in
#' the `centile` package.
#' @param verbose Set to `TRUE` to turn on warnings
#' @param dec A scalar value indicating the number of decimals used to round the
#' value. The default is 3 decimals.
#' @param tail_adjust Logical. If \code{TRUE} the procedure applies the WHO
#' method for tail adjustment under the LMS distribution. We do not recommend
#' this. The default is \code{FALSE}.
#' @param \dots Not used.
#' @inheritParams stats::approx
#' @return A vector with `length(y)` elements containing the Z-scores.
#' @author Stef van Buuren, 2021
#' @examples
#' # Weight SD 1 month old boy and girl of 4 KG using built-in WHO references
#' y <- c(4, 4)
#' x <- c(1/12, 1/12)
#' refcode <- c("who_2006_wgt_male_", "who_2006_wgt_female_")
#' y2z(y, x, refcode)
#'
#' # using external reference, for females only
#' fn <- system.file("testdata/nl_2009_wgt_female_nl.txt", package = "centile")
#' myref <- import_rif(fn)
#' head(myref)
#' head(attr(myref, "study"))
#' y2z(y, x, myref)
#' @export
y2z <- function(y, x, refcode, pkg = "centile", verbose = FALSE,
                dec = 3L, rule = 1L, tail_adjust = FALSE, ...) {
  if (length(y) != length(x) && length(x) > 1L) {
    message("y2z(): Non-conformable arguments y and x")
    return(rep(NA_real_, length(y)))
  }

  # if refcode is a reference, use that for all y and x
  if (is_reference(refcode)) {
    z <- z_grp(y = y, x = x, refcode = refcode, verbose = verbose,
               pkg = pkg, rule = rule, tail_adjust = tail_adjust)
    return(round(z, digits = dec))
  }

  if (is.data.frame(refcode)) {
    message("z2y(): Argument refcode has no `study` attribute.")
    return(rep(NA_real_, length(z)))
  }

  if (length(y) != length(refcode) && length(refcode) > 1L) {
    message("y2z(): Non-conformable arguments y and refcode")
    return(rep(NA_real_, length(y)))
  }
  if (!length(y)) {
    return(numeric(0))
  }

  # alternatively, split calculations by refcode
  data.frame(y = y, x = x, refcode = refcode) %>%
    group_by(.data$refcode) %>%
    mutate(z = z_grp(
      y = .data$y,
      x = .data$x,
      refcode = first(.data$refcode),
      pkg = pkg,
      verbose = verbose,
      rule = rule,
      tail_adjust = tail_adjust)) %>%
    ungroup() %>%
    pull(.data$z) %>%
    round(digits = dec)
}

z_grp <- function(y, x, refcode, pkg, verbose, rule, tail_adjust = FALSE) {
  r <- load_reference(refcode = refcode, pkg = pkg, verbose = verbose)

  # do not process in absence of study attribute
  study <- attr(r, "study")
  if (is.null(study)) {
    return(rep(NA_real_, length(y)))
  }

  # handle transforms, if any
  if ("ty" %in% names(study)) {
    y <- eval(parse(text = study[["ty"]]))
  }
  if ("tx" %in% names(study)) {
    x <- eval(parse(text = study[["tx"]]))
  }

  dist <- toupper(study[["distribution"]])
  if (dist == "NO") {
    check_names(df = r, needed = c("x", "mean", "sd"))
    mean <- approx(x = r[["x"]], y = r[["mean"]], xout = x, rule = rule)$y
    sd <- approx(x = r[["x"]], y = r[["sd"]], xout = x, rule = rule)$y
    return((y - mean) / sd)
  }
  if (dist == "LMS") {
    check_names(df = r, needed = c("x", "L", "M", "S"))
    L <- approx(x = r[["x"]], y = r[["L"]], xout = x, rule = rule)$y
    M <- approx(x = r[["x"]], y = r[["M"]], xout = x, rule = rule)$y
    S <- approx(x = r[["x"]], y = r[["S"]], xout = x, rule = rule)$y
    z <- ifelse(L > 0.01 | L < (-0.01), (((y / M)^L) - 1) / (L * S), log(y / M) / S)
    if (tail_adjust) z <- adjust_tail_z(y, z, L, M, S)
    return(z)
  }
  if (dist == "BCCG") {
    check_names(df = r, needed = c("x", "nu", "mu", "sigma"))
    nu <- approx(x = r[["x"]], y = r[["nu"]], xout = x, rule = rule)$y
    mu <- approx(x = r[["x"]], y = r[["mu"]], xout = x, rule = rule)$y
    sigma <- approx(x = r[["x"]], y = r[["sigma"]], xout = x, rule = rule)$y
    return(qnorm(pBCCG(y, mu = mu, sigma = sigma, nu = nu)))
  }
  if (dist == "BCPE") {
    check_names(df = r, needed = c("x", "nu", "mu", "sigma", "tau"))
    mu <- approx(x = r[["x"]], y = r[["mu"]], xout = x, rule = rule)$y
    sigma <- approx(x = r[["x"]], y = r[["sigma"]], xout = x, rule = rule)$y
    nu <- approx(x = r[["x"]], y = r[["nu"]], xout = x, rule = rule)$y
    tau <- approx(x = r[["x"]], y = r[["tau"]], xout = x, rule = rule)$y
    return(qnorm(pBCPE(y, mu = mu, sigma = sigma, nu = nu, tau = tau)))
  }
  if (dist == "BCT") {
    check_names(df = r, needed = c("x", "nu", "mu", "sigma", "tau"))
    mu <- approx(x = r[["x"]], y = r[["mu"]], xout = x, rule = rule)$y
    sigma <- approx(x = r[["x"]], y = r[["sigma"]], xout = x, rule = rule)$y
    nu <- approx(x = r[["x"]], y = r[["nu"]], xout = x, rule = rule)$y
    tau <- approx(x = r[["x"]], y = r[["tau"]], xout = x, rule = rule)$y
    return(qnorm(pBCT(y, mu = mu, sigma = sigma, nu = nu, tau = tau)))
  }

  stop(paste("Reference type", dist, "not implemented."))
}

adjust_tail_z <- function(y, z, L, M, S) {
  idx <- !is.na(z) & z > 3
  if (any(idx)) {
    sd3 <- ifelse(L > 0.01 | L < (-0.01), M * (1 + L * S * 3)^(1 / L), M * exp(S * 3))
    sd2 <- ifelse(L > 0.01 | L < (-0.01), M * (1 + L * S * 2)^(1 / L), M * exp(S * 2))
    z[idx] <- (3 + (y - sd3) / (sd3 - sd2))[idx]
  }
  idx <- !is.na(z) & z < (-3)
  if (any(idx)) {
    sd3 <- ifelse(L > 0.01 | L < (-0.01), M * (1 + L * S * (-3))^(1 / L), M * exp(S * (-3)))
    sd2 <- ifelse(L > 0.01 | L < (-0.01), M * (1 + L * S * (-2))^(1 / L), M * exp(S * (-2)))
    z[idx] <- (-3 + (y - sd3) / (sd2 - sd3))[idx]
  }
  z
}

