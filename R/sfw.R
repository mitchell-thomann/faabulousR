#### Example to follow - delete

#' Add together two numbers.
#' 
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)
add <- function(x, y) {
  x + y
}

#' Generate team data by week for testing
#' 
#' @param labels vector of team labels
#' @param weeks total number of weeks
#' @param distribution list with elements, dist = c("norm"), mean, sd
#' @param decimal indicator for decimal scoring 
#' 
gen_team_dat <- function(labels, weeks, distribution, decimal) {
  
}