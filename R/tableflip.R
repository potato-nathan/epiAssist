#' Flip Table
#'
#' @param tab table object
#'
#' @return table object
#' @export
#'
#' @examples
#' set.seed(234)
#' require(magrittr)
#'
#'sample <- dplyr::tibble("A" = stats::rnorm(100, 50, 4.5),
#'                        "B" = seq(1,300,3))
#'
#'sample <- sample %>%
#'  dplyr::mutate(position = factor(ifelse(B %% 2 == 0, "Even", "Odd")))
#'
#'fruits <- c("Kiwi", "Melon", "Peach")
#'
#'treatment <- c("Chemical", "Organic", "Squish 'em")
#'
#'died <- c("Dead", "Not dead")
#'
#'sample$fruit <- sample(fruits, 100, replace = TRUE)
#'sample$pesticide <- sample(treatment, 100, replace = TRUE)
#'sample$death <- sample(died, 100, replace = TRUE)

#'tab <- table(sample$pesticide, sample$death, sample$fruit)
#'
#'tab
#'
#' fliptable(tab)
fliptable <- function(tab){
  # also need to check if it's a table object? how to do that?

  if(class(tab) != "table"){
    stop('flip table only accepts objects that are tables created with function table()')
  }
  # check if table is stratifed by third variable
  if(is.na(dim(tab)[3])){

    # when it's a single 2x2 table, only flip that table
    x <- dim(tab)[1]
    y <- dim(tab)[2]
    flip <- tab[x:1, y:1]
  }

  # when it's stratified by multiple variables:

  # save the number of strata to x
  else{
    x <- dim(tab)[1]
    y <- dim(tab)[2]
    z <- dim(tab)[3]

    flip <- tab[x:1, y:1, 1:z]

  }
  return(flip)
}
