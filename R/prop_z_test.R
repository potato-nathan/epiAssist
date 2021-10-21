#' @title Two proportion z-test
#' @description Uses z distribution to test for statistical equality of two proportions
#' @param x a vector of length two, providing count
#' of "successes" or "outcomes of interest" for
#' either group or a two-dimensional table (or matrix)
#' with 2 columns, giving the counts of successes and
#' failures, respectively.
#' @param n a vector of length two for counts of trials/observations in either group;
#' ignored if x is a matrix or a table.
#' @param p a vector of length two representing the expected probability
#' of "success" in either group. The vector's elements must be greater than 0 and less than 1.
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default),
#' "greater" or "less". You can specify just the initial letter. Only used for testing the null that a single proportion equals
#' a given value, or that two proportions are equal; ignored otherwise.
#' @param conf.level confidence level of the returned confidence interval. Must be a single number between 0 and 1.
#' Only used when testing the null that a single proportion equals a given value, or that two proportions are equal; ignored otherwise.
#' @param correct a logical indicating whether Yates' continuity correction should be applied where possible.
#' @param case_col if x is a table or matrix, a single integer for the index of the column in which cases appear
#'
#' @details Takes a 2x2 table object where default is that cases appear in the first column and counts
#' of the exposed appear in the second row.
#'
#' Only groups with finite numbers of successes and failures are used. Counts of
#' successes and failures must be nonnegative and hence not greater than the corresponding
#' numbers of trials which must be positive. All finite counts should be integers.
#'
#' If p is NULL and there is more than one group, the null tested is that the proportions in
#' each group are the same. If there are two groups, the alternatives are that the probability
#' of success in the first group is less than, not equal to, or greater than the probability of
#' success in the second group, as specified by alternative. A confidence interval for the
#' difference of proportions with confidence level as specified by conf.level and clipped to {-1,1} is returned.
#' @author Although DGHI Biostat was responsible for the modification of this code, this code was originally
#' adapted from that published as a part of the R stats package, which contained the following message:
#'
#'  File src/library/stats/R/prop.test.R
#'  Part of the R package, https://www.R-project.org
#'
#'  Copyright (C) 1995-2012 The R Core Team
#'
#'  This program is free software; you can redistribute it and/or modify
#'  it under the terms of the GNU General Public License as published by
#'  the Free Software Foundation; either version 2 of the License, or
#'  (at your option) any later version.
#'
#'  This program is distributed in the hope that it will be useful,
#'  but WITHOUT ANY WARRANTY; without even the implied warranty of
#'  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#'  GNU General Public License for more details.
#'
#'  A copy of the GNU General Public License is available at
#'  https://www.R-project.org/Licenses/
#'  @examples
#'  prop_z_test(x = c(200, 300), n = c(400, 400))
#'  @importFrom stats qnorm pnorm complete.cases setNames

prop_z_test <-
  function(x, n, p = NULL, alternative = c("two.sided", "less", "greater"),
           conf.level = 0.95, correct = FALSE, case_col = 1)
  {
    DNAME <- deparse(substitute(x))

    if (is.table(x) && length(dim(x)) == 1L) {
      if (dim(x) != 2L)
        stop("table 'x' should have 2 entries")
      l <- 1
      n <- sum(x)
      x <- x[case_col]
    }
    else if (is.matrix(x)) {
      if (ncol(x) != 2L)
        stop("'x' must have 2 columns")
      l <- nrow(x)
      n <- rowSums(x)
      x <- x[, 1L]
    }
    else {
      DNAME <- paste(DNAME, "out of", deparse(substitute(n)))
      if ((l <- length(x)) != length(n))
        stop("'x' and 'n' must have the same length")
    }

    OK <- stats::complete.cases(x, n)
    x <- x[OK]
    n <- n[OK]
    if ((k <- length(x)) < 1L)
      stop("not enough data")
    if (any(n <= 0))
      stop("elements of 'n' must be positive")
    if (any(x < 0))
      stop("elements of 'x' must be nonnegative")
    if (any(x > n))
      stop("elements of 'x' must not be greater than those of 'n'")

    if (is.null(p) && (k == 1))
      p <- .5
    if (!is.null(p)) {
      DNAME <- paste0(DNAME, ", null ",
                      if(k == 1) "probability " else "probabilities ",
                      deparse(substitute(p)))
      if (length(p) != l)
        stop("'p' must have the same length as 'x' and 'n'")
      p <- p[OK]
      if (any((p <= 0) | (p >= 1)))
        stop("elements of 'p' must be between 0 and 1")
    }

    alternative <- match.arg(alternative)
    if (k > 2 || (k == 2) && !is.null(p))
      alternative <- "two.sided"

    if ((length(conf.level) != 1L) || is.na(conf.level) ||
        (conf.level <= 0) || (conf.level >= 1))
      stop("'conf.level' must be a single number between 0 and 1")

    correct <- as.logical(correct)

    ESTIMATE <- stats::setNames(x/n,
                         if (k == 1) "p" else paste("prop", 1L:l)[OK])
    NVAL <- p
    CINT <- NULL
    YATES <- if(correct && (k <= 2)) .5 else 0

    if (k == 1) {
      z <- stats::qnorm(if(alternative == "two.sided")
        (1 + conf.level) / 2 else conf.level)
      YATES <- min(YATES, abs(x - n * p)) # what are the conditions under which a Yates continuity correction would be applied? Any one cell less than 5? Do we even need to include this here for a z test of two proportions?
      z22n <- z^2 / (2 * n)
      p.c <- ESTIMATE + YATES / n
      p.u <- if(p.c >= 1) 1 else (p.c + z22n
                                  + z * sqrt(p.c * (1 - p.c) / n + z22n / (2 * n))) / (1+2*z22n)
      p.c <- ESTIMATE - YATES / n
      p.l <- if(p.c <= 0) 0 else (p.c + z22n
                                  - z * sqrt(p.c * (1 - p.c) / n + z22n / (2 * n))) / (1+2*z22n)
      CINT <- switch(alternative,
                     "two.sided" = c(max(p.l, 0), min(p.u, 1)),
                     "greater" = c(max(p.l, 0), 1),
                     "less" = c(0, min(p.u, 1)))
    }
    else if ((k == 2) & is.null(p)) {
      DELTA <- ESTIMATE[1L] - ESTIMATE[2L]
      YATES <- min(YATES, abs(DELTA) / sum(1/n))
      WIDTH <- (switch(alternative,
                       "two.sided" = stats::qnorm((1 + conf.level) / 2),
                       stats::qnorm(conf.level))
                * sqrt(sum(ESTIMATE * (1 - ESTIMATE) / n))
                + YATES * sum(1/n))
      CINT <- switch(alternative,
                     "two.sided" = c(max(DELTA - WIDTH, -1),
                                     min(DELTA + WIDTH, 1)),
                     "greater" = c(max(DELTA - WIDTH, -1), 1),
                     "less" = c(-1, min(DELTA + WIDTH, 1)))
    }
    if (!is.null(CINT))
      attr(CINT, "conf.level") <- conf.level

    METHOD <- paste(if(k == 1) "1-sample proportions test" else
      paste0(k, "-sample Z-test of proportions"))

    if (is.null(p)) {
      p <- sum(x)/sum(n)
      PARAMETER <- k - 1
    }
    else {
      PARAMETER <- k
      names(NVAL) <- names(ESTIMATE)
    }
    names(PARAMETER) <- "df"

    x <- cbind(x, n - x)
    E <- cbind(n * p, n * (1 - p))
    if (any(E < 30))
      warning("Z approximation may be incorrect; consider using nonparametric test")
    STATISTIC <- DELTA / sqrt(p*(1-p) * sum(1/n))

    names(STATISTIC) <- "Z-score"

    if (alternative == "two.sided")
      PVAL <- stats::pnorm(STATISTIC)
    else {
      if (k == 1) # this implies a test of one proportion...include here? or refer user to binom.test()?
        z <- sign(ESTIMATE - p) * sqrt(STATISTIC) # also just a note that z doesn't appear to be used beyond this. is it even necessary?
      else
        z <- sign(DELTA) * sqrt(STATISTIC)
      PVAL <- stats::pnorm(STATISTIC, lower.tail = (alternative == "less"))
    }

    RVAL <- list(statistic = STATISTIC,
                 #parameter = PARAMETER,
                 p.value = as.numeric(PVAL),
                 estimate = ESTIMATE,
                 null.value = NVAL,
                 conf.int = CINT,
                 alternative = alternative,
                 method = METHOD,
                 data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
  }
