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

#' Generate actual wins by week for testing
#'
#'
#' @param team_labels vector of team labels
#' @param weeks total number of weeks
#'
#' @return data_frame with team and wins
#'
#'
gen_team_wins <- function(team_labels = paste0("team", 1:10),
                          weeks = 10) {
  team_dat <- dplyr::data_frame(
    team = team_labels,
    wins = round(rnorm(length(team_labels), mean = 5, sd = 2))
  )
  return(team_dat)
}

#' Calculate Schedule Free Wins (SFW)
#'
#' Schedule Free Wins Description
#'
#' @param team_dat tibble of teams scores by week, columns must include week, scores
#'   (may be decimal), team labels, wins
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
  sfw_week <- ranks %>%
    group_by(week) %>%
    mutate(sfw = (rank(scores) - 1) / (length(unique(team_dat$team)) - 1))
  sfw_cum <- sfw_week %>% 
    group_by(team) %>% 
    mutate(sfw_cum = cumsum(sfw),
           actual_cum = cumsum(win))
  return(sfw_cum)
}

#' Plots cumulative SFW
#'
#' Generates Longitudinal SFW Plot
#'
#' @param sfw_obj sfw data_frame object from sfw()
#'
#' @return plot of SFW across week
#'
#' @export
#'
#' @import ggplot2
#' @import plotly
#'
plot_cumsfw <- function(sfw_obj) {
  p <- ggplotly(
    ggplot(data = sfw_obj, aes(x = week, y = sfw_cum, colour = team)) + 
    geom_line() + 
    geom_point() +
    ylab("Cumulative SFW") + 
    guides(colour = guide_legend(title = "Team")) +
    scale_x_continuous(name = "Week", breaks = unique(sfw_obj$week))
  )
  return(p)
}

#' Plots SFW vs Wins
#'
#' Generates Scatterplot of SFW vs Wins
#'
#' @param sfw_cum sfw data_frame object from sfw(), cumulative object
#'
#' @return plot of SFW vs Wins
#'
#' @export
#'
#' @import ggplot2
#' @import dplyr
#' @import plotly
#'
plot_scatsfw <- function(sfw_cum) {
  #plot_dat <- inner_join(filter(sfw_obj, week == max(sfw_obj$week)), wins_actual, by = "team")
  p <- ggplotly(
    ggplot(data = sfw_cum, aes(x = actual_cum, y = sfw_cum, colour = team)) + 
    geom_point(size = 3) +
    ylim(0, max(sfw_cum$actual_cum)) + 
    ylab("Cumulative SFW") +
    scale_x_continuous(name = "Wins", breaks = seq(0, max(sfw_cum$actual_cum))) +
    guides(colour = guide_legend(title = "Team")) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed")
  )
  return(p)
}

#' Plots Difference between SFW and Wins - Barplots ... Luck
#'
#' Generates Barplots of SFW vs Wins
#'
#' @param sfw_obj sfw data_frame object from sfw(), cumulative
#'
#' @return plot of SFW vs Wins
#'
#' @export
#'
#' @import ggplot2
#' @import dplyr
#' @import plotly
#'
plot_windiff <- function(sfw_cum){
  plot_cum <- sfw_cum %>% 
    mutate(luck = actual_cum - sfw_cum,
           Team = team) 
  p <- ggplotly(
    ggplot(data = plot_cum, aes (x = reorder(Team, -luck), y = luck, fill = Team, colour = Team)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ylab("Luck") +
    xlab("")
  )
  return(p)
}

#' Plots Difference SFW and Actual Wins
#'
#' Generates Barplots of SFW & Wins
#'
#' @param sfw_obj sfw data_frame object from sfw(), cumulative
#'
#' @return plot of SFW vs Wins
#'
#' @export
#'
#' @import ggplot2
#' @import dplyr
#' @import plotly
#'
plot_sfw_wins <- function(sfw_cum){
  plot_cum <- sfw_cum %>% 
    mutate(luck = actual_cum - sfw_cum,
           Team = team)
  ordering <- arrange(plot_cum, -sfw_cum)$Team
  plot_both_cum <- tibble(
    value = c(plot_cum$sfw_cum,
              plot_cum$actual_cum),
    Type = factor(
      c(rep("SFW", nrow(plot_cum)),
        rep("Actual", nrow(plot_cum))),
      levels = c("SFW", "Actual")),
    Team = factor(rep(plot_cum$team, 2),
                  levels = ordering)
  )
  p <- ggplotly(
    ggplot(data = plot_both_cum, 
           aes(x = Team, y = value, fill = Type, colour = Type)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ylab("Wins") +
      xlab("")
  )
  return(p)
}

