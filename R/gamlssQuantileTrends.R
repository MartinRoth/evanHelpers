#' Computes change using gamlss
#' @description Fits a gamlss model and returns trend for the 0.05, 0.5, and 0.95 quantile
#' @param formula Formula object
#' @param data Data (so far only data.table is checked)
#' @param family String either "BCCG" or "SN2"
#' @param ... Other arguments passed to gamlss
#' @return data.table with columns for  family, predictors, tau, and value
#' @export
#' @import data.table
#' @import gamlss
#' @importFrom gamlss.dist gamlss.family
GetGamlssTrend <- function(formula, data, family = "BCCG", ...) {
  if (!family %in% c("BCCG", "SN2")) stop(paste(family, "not available"))
  result <- tryCatch({
  adjust <- NA
  #responseCol <- formula[[2]]
  #dat <- copy(data)
  dat <- get_all_vars(formula, data)
  if (family == "BCCG") {
    #adjust <- evanHelpers::GetBoxCoxAdjustment(dat[, eval(responseCol)])
    #dat[, eval(responseCol) := eval(responseCol) + adjust]
    adjust <- GetBoxCoxAdjustment(dat[, 1])
    dat[, 1] <- dat[, 1] + adjust #eval(responseCol) := eval(responseCol) + adjust]
  }
  capture.output(fit <- gamlss(formula=formula, data = dat, family = gamlss.family(family), ...))
  params <- list(mu = fit$mu.fv, sigma = fit$sigma.fv, nu = fit$nu.fv)
  tau <- fitted <- NULL
  values <- data.table(family = family,
                       tau    = rep(c(0.05, 0.5, 0.95), each = nrow(dat)),
                       fitted = c(do.call(paste0("q", family), c(p=0.05, params)),
                                  do.call(paste0("q", family), c(p=0.5,  params)),
                                  do.call(paste0("q", family), c(p=0.95, params))))
  if (family == "BCCG") values[, fitted := fitted - adjust]
  result = values[, cbind(data[], family, fitted), by = tau]
  #result[, eval(responseCol) := NULL]
  }, #warning = function(w) {},
  error = function(e) {
    result <- NULL
    return(result)
  })
  return(result)
}

#' Prepare for Box-Cox transformation
#' @description Box-Cox transformation is only allowed for positive values. In
#' the case that the minimal value min one standard deviation is negative we add
#' this difference.
#' @param data Numeric vector consisting of the data
#' @export
GetBoxCoxAdjustment <- function(data) {
  if(!class(data)=="numeric") stop("data should be numeric")
  if(length(data)<2) stop("data should have length > 1")
  x <- min(data)
  y <- sd(data)
  if (x < y) return(y - x)
  else return(0)
}
