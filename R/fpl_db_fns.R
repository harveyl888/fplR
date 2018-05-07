#' Get weekly scores
#'
#' Retrieve weekly scores for a fantasy football league
#'
#' @param l List of obtained from read_database
#' @param weeks Vector of weeks.  If empty then include all weeks
#'
#' @return dataframe containing table
#'
#' @import dplyr
#' @import tidyr
#' @export
fpl_league_weekly <- function(l, weeks = c()) {
  if (length(weeks) == 0) weeks <- seq(max(l[['league_weeks']]$week))
  df <- l[['league_weeks']] %>%
    filter(week %in% weeks) %>%
    left_join(l[['stats']] %>% select(id, week, total_points), by = c('element' = 'id', 'week')) %>%
    mutate(score = total_points * multiplier) %>%
    group_by(entry, week) %>%
    slice(1:11) %>%
    summarise(points = sum(score)) %>%
    left_join(l[['league']], by = c('entry')) %>%
    unite(team, player_name, entry_name) %>%
    ungroup() %>%
    select(team, week, points) %>%
    spread(week, points) %>%
    separate(team, into = c('manager', 'team'), sep = '_')
  df
}
