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


#' Return a list of teams
#'
#' Return a list of team IDs from a list of manager names, team names or simply the IDs
#'     themselves.
#'
#' @param l List of obtained from read_database
#' @param teams Vector of teams.  Vector of manager names, manager IDs or team
#'     names.  If empty then include all teams
#'
#' @return List of team IDs
.teamIDs <- function(l, ids = c()) {
  if (length(ids) == 0) return(l$league$entry)
  out.entries <- c(match(ids, l$league$entry),
                   match(ids, l$league$entry_name),
                   match(ids, l$league$player_name))
  out.entries <- sort(unique(out.entries[!is.na(out.entries)]))
  return(l$league$entry[out.entries])
}


#' Was the best captain selected?
#'
#' Retrieve a table indicating points differential between the captain chosen and the
#'     best choice
#'
#' @param l List of obtained from read_database
#' @param weeks Vector of weeks.  If empty then include all weeks
#' @param teams Vector of teams.  Vector of manager names, manager IDs or team
#'     names.  If empty then include all teams
#'
#' @return dataframe containing table
#'
#' @import dplyr
#' @import tidyr
#' @export
captainChoice <- function(l, weeks = c(), managers = c()) {
  if (length(weeks) == 0) weeks <- seq(max(l[['league_weeks']]$week))
  entries <- .teamIDs(l, managers)
  df_chosen <- l$league_weeks %>%
    filter(entry %in% entries) %>%
    filter(week %in% weeks) %>%
    group_by(entry, week) %>%
    slice(1:11) %>%
    filter(multiplier > 1) %>%
    left_join(l[['stats']] %>% select(id, week, total_points), by = c('element' = 'id', 'week')) %>%
    mutate(score_capt = total_points * multiplier)

  df_best <- l$league_weeks %>%
    filter(entry %in% entries) %>%
    filter(week %in% weeks) %>%
    group_by(entry, week) %>%
    left_join(l[['stats']] %>% select(id, week, total_points), by = c('element' = 'id', 'week')) %>%
    mutate(score_best = total_points * max(multiplier)) %>%
    top_n(n = 1, wt = score_best) %>%
    slice(1)

  df_out <- df_chosen %>%
    select(entry, week, score_capt) %>%
    right_join(df_best %>% select(entry, week, score_best), by = c('entry', 'week')) %>%
    mutate(score_capt = ifelse(is.na(score_capt), 0, score_capt)) %>%
    mutate(score_delta = score_best - score_capt) %>%
    left_join(l[['league']], by = c('entry')) %>%
    ungroup() %>%
    select(player_name, entry_name, week, score_capt, score_best, score_delta)

  df_spread <- df_out %>%
    select(entry_name, week, score_delta) %>%
    spread(week, score_delta) %>%
    mutate(total = rowSums(.[-1])) %>%
    arrange(desc(total), entry_name)

  return(list(complete = df_out, summary = df_spread))
}


#' Return the formation played
#'
#' Return the formation played in given game weeks
#'
#' @param l List of obtained from read_database
#' @param weeks Vector of weeks.  If empty then include all weeks
#' @param teams Vector of teams.  Vector of manager names, manager IDs or team
#'     names.  If empty then include all teams
#'
#' @return dataframe containing table
#'
#' @import dplyr
#' @import tidyr
#' @export
playedFormation <- function(l, weeks = c(), managers = c()) {
  if (length(weeks) == 0) weeks <- seq(max(l[['league_weeks']]$week))
  entries <- .teamIDs(l, managers)

  df_formation <- l$league_weeks %>%
    filter(entry %in% entries) %>%
    filter(week %in% weeks) %>%
    select(entry, week, element, position) %>%
    group_by(entry, week) %>%
    slice(1:11) %>%
    left_join(l[['players']] %>% select(id, element_type), by = c('element' = 'id')) %>%
    summarise(n_def = sum(element_type == 2), n_mid = sum(element_type == 3), n_fwd = sum(element_type == 4)) %>%
    mutate(formation = paste(n_def, n_mid, n_fwd, sep = '-')) %>%
    select(entry, week, formation) %>%
    ungroup() %>%
    spread(week, formation) %>%
    left_join(l[['league']] %>% select(entry, entry_name), by = 'entry') %>%
    select(-entry) %>%
    select(entry_name, everything())
}
