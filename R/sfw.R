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
#'
#' @param team_labels vector of team labels
#' @param weeks total number of weeks
#' @param random_dist list with elements, dist = c("norm"), mean, sd
#' @param decimal indicator for decimal scoring
#'
#' @return data_frame with team, week and random score (normal distribution)
#'
#'
gen_team_dat <- function(team_labels = paste0("team", 1:10),
                         weeks = 10,
                         random_dist = list(dist = "norm", mean = 100, sd = 10),
                         decimal = F) {
  if (random_dist$dist == "norm") {
    team_dat <- dplyr::data_frame(
      team = rep(team_labels, weeks),
      week = rep(1:weeks, each = length(team_labels))
    )
    team_dat$scores <- rnorm(n = nrow(team_dat), mean = random_dist$mean, sd = random_dist$sd)
    if (!decimal) floor(team_dat$scores)
  }
  return(team_dat)
}

#' Calculate Schedule Free Wins (SFW)
#'
#'
#' @param
#' @param
#' @param
#' @param
#'
#' @return data_frame with team's SFW by week & cumulative. May include plots
#' (may add separate functions), bar plot of each teams SFW vs actual wins and scatterplot of
#' SFW vs actual wins
#'
#' @export
#'
sfw <- function(team_dat) {

}
