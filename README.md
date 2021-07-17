
<!-- README.md is generated from README.Rmd. Please edit that file -->

# epiAssist

A package with functions designed to help R beginners with superfluous
programming tasks involved with Biostat 705 Lab

## Welcome, lonesome traveler\!

If you’ve somehow made it to this site, I can only imagine what you must
be doing with your time. I specifically created this package so that you
wouldn’t have to go digging around for things as you complete the Labs
for this semester. Or, at least, not so much.

But alas, here we are and I think I’ve been discovered.

(Don’t tell Larry.)

<!-- badges: start -->

<!-- badges: end -->

## Installation

You can install the development version of epiAssist from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("potato-nathan/epiAssist")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(epiAssist)

# the {epiR} function epi.2by2() requires that tables be in a particular orientation in order
#to provide proper output. Essentially, it needs to be able to identify what's an exposure, and
#what's an outcome (and their opposites).

# {epiAssist}'s fliptable() takes a 2x2 table object, stratified by as many levels as your
#heart desires, and flips it. This is done assuming that you have coded your variables to
#be factors, and that the referent variable is always at factor level 0.

#For example:

set.seed(234)
require(magrittr)
#> Loading required package: magrittr

sample <- dplyr::tibble("A" = stats::rnorm(100, 50, 4.5),
                       "B" = seq(1,300,3))

sample <- sample %>%
 dplyr::mutate(position = factor(ifelse(B %% 2 == 0, "Even", "Odd")))

fruits <- c("Kiwi", "Melon", "Peach")

treatment <- c("Chemical", "Organic", "Squish 'em")

died <- c("Dead", "Not dead")

sample$fruit <- sample(fruits, 100, replace = TRUE)
sample$pesticide <- sample(treatment, 100, replace = TRUE)
sample$death <- sample(died, 100, replace = TRUE)
tab <- table(sample$pesticide, sample$death, sample$fruit)

tab
#> , ,  = Kiwi
#> 
#>             
#>              Dead Not dead
#>   Chemical      6        6
#>   Organic       3        6
#>   Squish 'em    4        6
#> 
#> , ,  = Melon
#> 
#>             
#>              Dead Not dead
#>   Chemical      3        5
#>   Organic       9        4
#>   Squish 'em    8        4
#> 
#> , ,  = Peach
#> 
#>             
#>              Dead Not dead
#>   Chemical      3        6
#>   Organic       5        8
#>   Squish 'em    7        7

fliptable(tab)
#> , ,  = Kiwi
#> 
#>             
#>              Not dead Dead
#>   Squish 'em        6    4
#>   Organic           6    3
#>   Chemical          6    6
#> 
#> , ,  = Melon
#> 
#>             
#>              Not dead Dead
#>   Squish 'em        4    8
#>   Organic           4    9
#>   Chemical          5    3
#> 
#> , ,  = Peach
#> 
#>             
#>              Not dead Dead
#>   Squish 'em        7    7
#>   Organic           8    5
#>   Chemical          6    3
```
