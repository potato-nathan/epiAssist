IRR_confint <- function(IR_ratio, x1, x2){
  log <- log(IR_ratio)
  se1 <- (1/x1)
  se2 <- (1/x2)

  se <- sqrt(se1 + se2)

  upper <- exp(log + (1.96 * se))
  lower <- exp(log - (1.96 * se))

  return(c(lower, upper))
}
