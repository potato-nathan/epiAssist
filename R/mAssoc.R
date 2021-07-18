#' Measures of Association
#'
#' This is a wrapper function for the {epiR} function epi.2by2(). It allows the user to obtain measures of association in cases where the exposure variable has more than 2 levels.
#'
#' @param tab table object in epi.2by2() format (referent variable in bottom left, index variable(s) in upper left)
#' @param method specification of study type. just like epi.2by2(), options are cohort.count, cohort.time, case.control, or cross.sectional.
#' @param conf.level magnitude of the returned confidence intervals. Must be a single number between 0 and 1.
#'
#' @return a list of objects of class epi.2by2 (type ?epi.2by2 for more details)
#' @export
#'
#' @examples
#' risk_rural <- flipTable(table(kenya$rural, kenya$death))
#' mAssoc(risk_rural)
#'
mAssoc <- function(tab, method = "cohort.count", conf.level = 0.95) {

  if(length(dim(tab)) != 2) {
    stop("Measures of association only possible on contingency tables of dimensions 2x2 or more")
  }

  if(length(dim(tab)) == 2) {
    #browser()
    lev <- dim(tab)[1]

    names <- rownames(tab)

    out <- list()
    # generate output for each index level in exposure
    for(i in 1:(lev-1)){

      epi.tab <- rbind(tab[i,], tab[lev,])
      cat(paste("MEASURES OF ASSOCIATION FOR: ", names[i], "(index) vs. ", names[lev]), "(referent)")

      print(epiR::epi.2by2(epi.tab, method = method, conf.level = conf.level, unit = 1, outcome = 'as.columns'))

      cat('\n\n\n')

    }
  }
}
