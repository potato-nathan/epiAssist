mAssoc <- function(tab, method = "cohort.count", conf.level = 0.95) {

  if(length(dim(tab)) < 2) {
    stop("Measures of association only possible on contingency tables of dimensions 2x2 or more")
  }

  if(length(dim(tab)) == 2) {

  lev <- dim(tab)[1]

  out <- list()
    # generate output for each index level in exposure
    for(i in 2:lev){

      epi.tab <- rbind(tab[1,], tab[i,])

      x <- epiR::epi.2by2(epi.tab, method = method, conf.level = conf.level, unit = 1, outcome = 'as.columns')

      out <- append(out, x)

    }
  }
}
