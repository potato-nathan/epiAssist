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
