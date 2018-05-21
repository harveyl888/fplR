#' Internal function to return a weekly total for a grouped team and week
#'
#' Internal function to return a weekly total for a grouped team and week
#'
#' @param d Data frame for a specific manager and week
#' @param inc_transfers Boolean.  Should points lost by transferring players be taken into account?
#' @param entry_use Dataframe.  A data frame containing the entry_weeks table as obtained from
#'     read_database.  It requires that the two columns be remaned (use_entry = entry, use_week = week)
#'
#' @import dplyr
.week_score <- function(d, inc_transfers, entry_use) {

  entry_row <- entry_use %>%
    filter(use_entry == d[1, ]$entry & use_week == d[1, ]$week) %>%
    slice(1)

  # if (entry_row$chip == '3xc') {  ## account for triple captain
  #   d[d$multiplier == 2, ]$multiplier <- 3
  # }
  if (!entry_row$chip == 'bboost') {  ## account for bench boost
    d <- d[1:11, ]
  }
  score <- sum(d$total_points * d$multiplier)
  if (inc_transfers == TRUE) score <- score - entry_row$cost_transfers
  return (score)
}

#' Internal function to get weekly scores
#'
#' Retrieve weekly scores for a fantasy football league
#' Used by fpl_league_weekly and fpl_league functions
#'
#' @param l List of obtained from read_database
#' @param weeks Vector of weeks.  If empty then include all weeks
#' @param inc_transfsers Boolean.  Should points lost by transfers be taken into account.  A value
#'     of TRUE subtracts points from weekly totals when transfers cost points
#'
#' @return dataframe containing table
#'
#' @import dplyr
#' @import tidyr
.points_by_week <- function(l, weeks = c(), inc_transfers) {
  if (length(weeks) == 0) weeks <- seq(max(l[['league_weeks']]$week))
  entry_use <- l[['entry_weeks']] %>%
    rename(use_entry = entry, use_week = week)

  df <- l[['league_weeks']] %>%
    filter(week %in% weeks) %>%
    left_join(l[['stats']] %>% select(id, week, total_points), by = c('element' = 'id', 'week')) %>%
    group_by(entry, week) %>%
    do(points = .week_score(., inc_transfers, entry_use)) %>%
    mutate(points = unlist(points)) %>%
    left_join(l[['league']], by = c('entry')) %>%
    unite(team, player_name, entry_name) %>%
    ungroup() %>%
    select(team, week, points) %>%
    spread(week, points) %>%
    separate(team, into = c('manager', 'team'), sep = '_')
  df
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
  .points_by_week(l, weeks, inc_transfers = FALSE)
}


#' Get league table
#'
#' Retrieve a cumulative league table a fantasy football league
#'
#' @param l List of obtained from read_database
#' @param max_week Maximum week.  Defaults to maximum in database
#' @param out_type Type of output.  If total then return the total to the maximum week.
#'     If cumulative then return a cumulative score by week
#'
#' @return dataframe containing table
#'
#' @import dplyr
#' @import tidyr
#' @export
fpl_league <- function(l, max_week = 0, out_type = 'total') {
  if (max_week == 0) max_week <- max(l[['league_weeks']]$week)
  weeks <- seq(max_week)
  df <- .points_by_week(l, weeks, inc_transfers = TRUE)
  if (out_type == 'total') {
    df <- cbind(df[, 1:2], total = apply(df[, 3:ncol(df)], 1, sum))
  } else {
    df[, 3:ncol(df)] <- t(apply(df[, 3:ncol(df)], 1, cumsum))
  }
  df[ order(-df[, ncol(df)]), ]
}

