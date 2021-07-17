#' Stratified test of homogeneity
#'
#' Calculates chi-square test statistic and p-value for risk differences
#' and risk ratios of stratified 2x2 tables.
#'
#' For calculation to be correct, table must be formatted same as those
#' used for epi.2by2(), where counts for the outcome of interest and the
#' exposed group appear in the upper-left corner of each stratified table.
#'
#' If your 2x2 tables are composed of factors where the referent variable is
#' level 0 and the index group is level 1, use flipTable() to re-orient to the
#' format required here.
#'
#' @param tab a table object
#' @param metric "Risk Difference" or "Risk Ratio"
#'
#' @return test statistic and p-value
#' @export
#'
#' @examples
#' food <- dplyr::tibble(
#' fruits = sample(c("Orange", "Tomato"), 100, replace = TRUE),
#' vegs = sample(c("Okra", "Fingernails"), 100, replace = TRUE),
#' grain = sample(c("Baked", "Raw", "Monsanto"), 100, replace = TRUE))
#'
#' tab <- flipTable(table(food))
#'
#' epiHomog(tab)
#'
epiHomo <- function(tab, metric = "Risk Difference"){
  if(class(tab) != "table"){
    stop('flip table only accepts objects that are tables created with function table()')
  }

  if(length(dim(tab)) < 3){
    stop('This function tests homogeneity between stratified 2x2 tables')
  }

  if(length(dim(tab)) > 3){
    stop('What do you plan on doing with a table of more than 3 dimensions?')
  }
  #browser()
  x <- c(1:dim(tab)[3])

  total.in <- c()
  total.ref <- c()
  outcome.in <- c()
  outcome.ref <- c()

  for(i in x){

    # calculate total datapoints in the various index/reference groups
    total.in <- append(total.in,tab[1,1,i] + tab[1,2,i])
    total.ref <- append(total.ref, tab[2,1,i] + tab[2,2,i])

    outcome.in <- append(outcome.in, tab[1,1,i])
    outcome.ref <- append(outcome.ref, tab[2,1,i])

  }


  # calculate risk of the outcome in each exposure/treatment group
  risk.in <- outcome.in / total.in
  risk.ref <- outcome.ref / total.ref

  # calculation of variance of risk betweent treatment groups
  w <- ((risk.in * (1 - risk.in)) / (total.in - 1)) + ((risk.ref * (1 - risk.ref)) / (total.ref - 1))

  # calculation of risk difference in each strata
  Y <- risk.in - risk.ref

  # calculation of Weighted Least Squares estimate
  r <- sum(Y/w) / sum(1/w)

  # calculation of WLS chi-square test statistic (Q)
  Q <- sum((Y - r)^2/w)

  # compute the p-value of Q, using degrees of freedom = (# of strata - 1) and lower.tail = FALSE
  p <- stats::pchisq(Q, df = (dim(tab)[3]-1), lower.tail = FALSE)

  # TODO: Add equation for homogeneity of risk ratios

  return(dplyr::tibble("Test statistic" = Q, "p-value" = p))

}
