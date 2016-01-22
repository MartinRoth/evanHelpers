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
