.week_score <- function(d, inc_transfers, entry_row) {
  if (entry_row$chip == '3xc') {  ## account for triple captain
    d[d$multiplier == 2, ]$multiplier <- 3
  }
  if (!entry_row$chip == 'bboost') {  ## account for bench boost
    d <- d[1:11, ]
  }
  sum(d$total_points * d$multiplier)
}


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
  entry_use <- l[['entry_weeks']] %>%
    rename(use_entry = entry, use_week = week)

  df <- l[['league_weeks']] %>%
    filter(week %in% weeks) %>%
    left_join(l[['stats']] %>% select(id, week, total_points), by = c('element' = 'id', 'week')) %>%
    group_by(entry, week) %>%
    do(points = .week_score(., inc_transfers = FALSE, entry_row = entry_use %>% filter(entry == use_entry & week == use_week))) %>%
    mutate(points = unlist(points)) %>%
    left_join(l[['league']], by = c('entry')) %>%
    unite(team, player_name, entry_name) %>%
    ungroup() %>%
    select(team, week, points) %>%
    spread(week, points) %>%
    separate(team, into = c('manager', 'team'), sep = '_')
  df
}

