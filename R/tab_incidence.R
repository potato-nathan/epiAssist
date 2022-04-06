#' Calculate incidence rate and measures of association between strata
#'
#' @param pyears_object An object generated using survival package function pyears()
#' @param index The position of the index group in the vector pyears_object$event
#' @param ref The position of the reference group in the vector pyears_object$pyears
#'
#' @return A tibble with calculations for reference group in row 1 and index group in row 2.
#' @export
#'
#' @examples tab_incidence(pyears_male)
tab_incidence <- function(pyears_object, index = 2, ref = 1){
  # index specifies the position of the index group ('exposed')
  # ref specifies the position of the reference group ('unexposed')

  # convert to numeric vectors
  e <- as.numeric(pyears_object$event)
  p <- as.numeric(pyears_object$pyears)

  # create vectors that reflect specified index and reference groups
  events <- c(e[ref], e[index])
  person_time <- c(p[ref], p[index])

  IR <- events / person_time

  IRD <- IR[2] - IR[1]

  # using ratedifference() function to calculate CI for IRD (from package fmsb)
  IRD_calc <- fmsb::ratedifference(events[2], events[1], person_time[2], person_time[1])

  #we have to manually extract the upper and lower bounds from the ratedifference() function:
  IRD_CI <- c(IRD_calc$conf.int[1], IRD_calc$conf.int[2])

  IRR <- IR[2] / IR[1]

  # obtain CI for IRR using helper function IR_confint():
  IRR_CI <- IRR_confint(IRR, events[1], events[2])

  # create printable confidence intervals
  print_IRD_CI <- paste("(", round(IRD_CI[1], 10), ", ", round(IRD_CI[2],10), ")", sep = '')
  print_IRR_CI <- paste("(", round(IRR_CI[1], 2), ", ", round(IRR_CI[2],2), ")", sep = '')
  # return in tibble for ease of use:
  tibble::tibble('deaths' = events,
         'Person-months' = person_time,
         "Incidence rate" = IR,
         "Inc. rate difference" = c(0, IRD),
         "IRD conf." = c(NA, print_IRD_CI),
         "Inc. rate ratio" = c(1, IRR),
         "IRR conf." = c(NA, print_IRR_CI))
}
