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
    if (!decimal) team_dat$scores <- floor(team_dat$scores)
  }
  return(team_dat)
}

#' Calculate Schedule Free Wins (SFW)
#' 
#' Schedule Free Wins Description
#'
#' @param team_dat tibble of teams scores by week, columns must include week, scores by week 
#'   (may be decimal), team labels
#'
#' @return data_frame with team's SFW by week & cumulative. May include plots
#' (may add separate functions), bar plot of each teams SFW vs actual wins and scatterplot of
#' SFW vs actual wins
#'
#' @export
#' 
#' @importFrom magrittr %>%
#' @import dplyr 
#'
sfw <- function(team_dat) {
  ranks <- team_dat %>% group_by(week) %>% mutate(rank = rank(scores))
  return(ranks %>% group_by(team) %>% summarize(sfw=sum(rank-1)/(length(unique(team_dat$team))-1)))
}

