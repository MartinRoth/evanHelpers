#' Normalizes covariates
#' @param covariate Covariate to be normalized
#' @return list with three entries, \code{val} the normalized covariate,
#'  \code{meanCov} the mean of the covariate, and \code{sdCov} its standard
#'  deviation
#' @export
SetNormCovariate <- function(covariate) {
  meanCov <- mean(covariate)
  sdCov   <- sd(covariate)
  val     <- (covariate - meanCov) / sdCov
  return(list(meanCov = meanCov, sdCov = sdCov, val = val ))
}

#' Obtain new normalized covariates
#' @description Returns the normalized value of a new covariate value with respect to an
#' existing normCov object
#' @param covariate The new value of the covariate
#' @param normCov The normalized covariate object
#' @export
GetNormCovariate <- function(covariate, normCov) {
  return((covariate - normCov$meanCov)/normCov$sdCov)
}
