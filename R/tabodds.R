#' @title Tabulation to display odds
#' @description
#' \code{tabodds} generates cross-tabulation between two variables and
#' display odds of failure \code{var_case} against a categorical
#' explanatory variable \code{var_exp}. It is used with cross-sectional
#' data.
#' @param var_case Case or outcome variable should be binary vector.
#' @param var_exp Exposure variable.
#' @param data a data frame object (Optional)
#' @param na.rm A logical value to specify missing values, <NA> in the table
#' @param rnd specify rounding of numbers. See \code{\link{round}}.
#' @param plot logical value to display plots of rates across a categorical
#' variable
#' @param print.table logical value to display formatted outputs
#' @details
#' The variable \code{var_case} should coded 1 for case and 0 for non-case.
#' A simple table illustrating cases and controls as well as odds for each
#' category is generated.
#'
#' \strong{Calculating Odds}
#'
#' \deqn{OR = d1 x h0 / d0 x h1}
#'
#' \strong{Error Factor} (EF)
#'
#' \deqn{EF = exp(1.96 x SE(log odds))}
#'
#' \deqn{SE(log odds) = \sqrt{1/d + 1/h}}
#'
#' @references
#'
#' 1. Essential Medical Statistics, Betty R. Kirwood, Second
#' Edition
#'
#' 2. Statistics Notes: The odds ratio; J Martin Bland, Douglas G Altman
#' BMJ 2000;320:1468
#'
#' @keywords odds, odds ratio, frequency table, statistics, descriptive
#' @author Myo Minn Oo (Email: \email{dr.myominnoo@@gmail.com} |
#' Website: \url{https://myominnoo.github.io/})
#' @examples
#' \dontrun{
#'
#' ## Asthma Example (page 160, Essential Medical Statistics)
#' asthma <- expandTables(c(81, 995, 57, 867),
#'              exp_name = "Sex",
#'              exp_lvl = c("Women", "Man"),
#'              case_name = "Asthma",
#'              case_lvl = c("Yes", "No"))
#'
#' ## labelling
#' asthma <- labelData(asthma, "Hypothetical Data of Asthma Prevalence")
#' asthma <- labelVars(asthma,
#'     c(Sex, Asthma), c("Man or Woman", "Asthma or No Asthma"))
#'
#' ## Checking codebook
#' codebook(asthma)
#'
#' ## Odds
#' tabodds(Sex, Asthma, asthma)
#'
#' ## Odds ratios
#' mhodds(Sex, Asthma, asthma)
#'
#' ## The odds ratio, J Martin Bland, Douglas G Altman, BMJ 2000;320:1468
#' hay <- expandTables(c(141, 420, 928, 13525),
#'                     exp_name = "eczema",
#'                     exp_lvl = c("Yes", "No"),
#'                     case_name = "hayFever",
#'                     case_lvl = c("Yes", "No"))
#' hay <- labelData(hay, "hay fever and eczema in 11 year old children")
#' hay <- labelVars(hay,
#'                  c(eczema, hayFever),
#'                 c("prevalence of eczema", "prevalence of hay fever"))
#'
#'
#' tabodds(eczema, hayFever, hay)
#' mhodds(eczema, hayFever, hay, "Yes")
#' }

#' @export
tabodds <-  function(var_exp, var_case, data = NULL,
                     na.rm = FALSE, rnd = 3,
                     plot = TRUE,
                     print.table = TRUE)

{
  arguments <- as.list(match.call())
  exp_name <- deparse(substitute(var_exp))
  case_name <- deparse(substitute(var_case))

  if (!is.null(data)) {
    var_exp <- eval(substitute(var_exp), data)
    var_case <- eval(substitute(var_case), data)
  }

  na.rm <- ifelse(na.rm, "no", "ifany")

  t <- table(var_exp, var_case, useNA = na.rm)
  case_lvl <- colnames(t)

  odds <- t[,2] / t[,1]
  SE <- sqrt((1/sum(t[, 2])) + (1/sum(t[, 1])))
  EF <- exp(1.96 * SE)
  lower <- odds / EF
  upper <- odds * EF
  t <- data.frame(cbind(t,
                        sprintf(odds, fmt = paste0('%#.', rnd, 'f')),
                        sprintf(lower, fmt = paste0('%#.', rnd, 'f')),
                        sprintf(upper, fmt = paste0('%#.', rnd, 'f'))),
                  stringsAsFactors = FALSE)
  names(t) <- c(case_lvl, "odds",
                "[95% Conf.", "Interval]")

  if (print.table) {
    printText(t,
              paste0("Tabulation: ", exp_name, " ~ ", case_name, "\n",
                     "Note: showing odds with 95% CI"),
              split = "Note:")

    if (!is.null(attr(var_case, "label")) |
        !is.null(attr(var_exp, "label"))) {
      printMsg("Labels:")
      printMsg(paste0(exp_name, ": ",
                      attr(var_exp, "label"), collapse = ""))
      printMsg(paste0(case_name, ": ",
                      attr(var_case, "label"), collapse = ""))
    }
  }

  if (plot) {
    by <- as.factor(row.names(t))
    plot(by, odds, ylim = c(0, max(upper)),
         main = paste0("Odds of ", exp_name, " among ",
                       case_name),
         xlab = exp_name,
         ylab = "Odds")
    nrow_by <- nrow(t)
    graphics::segments(1:nrow_by, odds, 1:nrow_by, upper, col = "blue", lwd = 2)
    graphics::segments(1:nrow_by, odds, 1:nrow_by, lower, col = "blue", lwd = 2)
  }

  invisible(t)
}
