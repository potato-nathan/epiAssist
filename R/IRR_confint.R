#' Calculate confidence interval for incidence rate ratios
#'
#' @param IR_ratio An incidence rate ratio
#' @param x1 The number of events in the reference group ("unexposed")
#' @param x2 The number of events in the index group ("exposed")
#'
#' @return A numeric vector of length 2
#' @export
#'
#' @examples IRR_confint(1.22, 938, 1227)
IRR_confint <- function(IR_ratio, x1, x2){
  log <- log(IR_ratio)
  se1 <- (1/x1)
  se2 <- (1/x2)

  se <- sqrt(se1 + se2)

  upper <- exp(log + (1.96 * se))
  lower <- exp(log - (1.96 * se))

  return(c(lower, upper))
}
