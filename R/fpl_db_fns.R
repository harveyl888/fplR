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
week_score <- function(d, inc_transfers, entry_use) {

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
#' @param f an fpl object
#' @param weeks Vector of weeks.  If empty then include all weeks
#' @param inc_transfsers Boolean.  Should points lost by transfers be taken into account.  A value
#'     of TRUE subtracts points from weekly totals when transfers cost points
#'
#' @return dataframe containing table
#'
#' @import dplyr
#' @importFrom tidyr unite
points_by_week <- function(f, weeks = c(), inc_transfers) {
  if (length(weeks) == 0) weeks <- seq(max(f$league_weeks$week))
  entry_use <- f$entry_weeks %>%
    rename(use_entry = entry, use_week = week)

  df <- f$league_weeks %>%
    filter(week %in% weeks) %>%
    left_join(f$stats %>% select(id, week, total_points), by = c('element' = 'id', 'week')) %>%
    group_by(entry, week) %>%
    do(points = week_score(., inc_transfers, entry_use)) %>%
    mutate(points = unlist(points)) %>%
    left_join(f$league, by = c('entry')) %>%
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
#' @param f an fpl object
#' @param weeks Vector of weeks.  If empty then include all weeks
#'
#' @return dataframe containing table
#'
#' @export
fpl_league_weekly <- function(f, weeks = c()) {
  points_by_week(f, weeks, inc_transfers = FALSE)
}


#' Get league table
#'
#' Retrieve a cumulative league table a fantasy football league
#'
#' @param f an fpl object
#' @param max_week Maximum week.  Defaults to maximum in database
#' @param out_type Type of output.  If total then return the total to the maximum week.
#'     If cumulative then return a cumulative score by week
#'
#' @return dataframe containing table
#'
#' @export
fpl_league <- function(f, max_week = 0, out_type = 'total') {
  if (max_week == 0) max_week <- f$last_week
  weeks <- seq(max_week)
  df <- points_by_week(f, weeks, inc_transfers = TRUE)
  if (out_type == 'total') {
    df <- cbind(df[, 1:2], total = apply(df[, 3:ncol(df)], 1, sum))
  } else {
    df[, 3:ncol(df)] <- t(apply(df[, 3:ncol(df)], 1, cumsum))
  }
  df[ order(-df[, ncol(df)]), ]
}


#' Return a manager's squad for a particular week
#'
#' Return a manager's squad for a particular week
#'
#' @param f an fpl object
#' @param week Week number
#' @param teams Vector of teams.  Vector of manager names, manager IDs or team
#'     names.  If empty then include all teams
#'
#' @return list of data frames containing teams
#'
#' @import dplyr
#' @export
squad_by_week <- function(f, week = 1, teams = c()) {
  entries <- teamIDs(f, teams)
  my_week <- week
  if(!is.numeric(my_week)) stop('error - week should be numeric')

  df_teams <- f$league_weeks %>%
    filter(entry %in% entries) %>%
    select(entry, week, element) %>%
    filter(week == my_week) %>%
    left_join(f$players %>% select(id, web_name, element_type, team_code), by = c('element' = 'id')) %>%
    left_join(f$teams %>% select(code, short_name), by = c('team_code' = 'code')) %>%
    arrange(entry, element_type, short_name, web_name) %>%
    select(entry, web_name, short_name)

  l.teams <- split.data.frame(df_teams, df_teams$entry)
  return(l.teams)
}



#' Return a list of teams
#'
#' Return a list of team IDs from a list of manager names, team names or simply the IDs
#'     themselves.
#'
#' @param f an fpl object
#' @param teams Vector of teams.  Vector of manager names, manager IDs or team
#'     names.  If empty then include all teams
#'
#' @return List of team IDs
teamIDs <- function(f, teams = c()) {
  if (length(teams) == 0) return(f$league$entry)
  out.entries <- c(match(teams, f$league$entry),
                   match(teams, f$league$entry_name),
                   match(teams, f$league$player_name))
  out.entries <- sort(unique(out.entries[!is.na(out.entries)]))
  return(f$league$entry[out.entries])
}


#' Was the best captain selected?
#'
#' Retrieve a table indicating points differential between the captain chosen and the
#'     best choice
#'
#' @param f an fpl object
#' @param weeks Vector of weeks.  If empty then include all weeks
#' @param managers Vector of teams.  Vector of manager names, manager IDs or team
#'     names.  If empty then include all teams
#'
#' @return dataframe containing table
#'
#' @import dplyr
#' @importFrom tidyr spread
#' @export
captainChoice <- function(f, weeks = c(), managers = c()) {
  if (length(weeks) == 0) weeks <- seq(max(f$league_weeks$week))
  entries <- teamIDs(f, managers)
  df_chosen <- f$league_weeks %>%
    filter(entry %in% entries) %>%
    filter(week %in% weeks) %>%
    group_by(entry, week) %>%
    slice(1:11) %>%
    filter(multiplier > 1) %>%
    left_join(f$stats %>% select(id, week, total_points), by = c('element' = 'id', 'week')) %>%
    mutate(score_capt = total_points * multiplier)

  df_best <- f$league_weeks %>%
    filter(entry %in% entries) %>%
    filter(week %in% weeks) %>%
    group_by(entry, week) %>%
    left_join(f$stats %>% select(id, week, total_points), by = c('element' = 'id', 'week')) %>%
    mutate(score_best = total_points * max(multiplier)) %>%
    top_n(n = 1, wt = score_best) %>%
    slice(1)

  df_out <- df_chosen %>%
    select(entry, week, score_capt) %>%
    right_join(df_best %>% select(entry, week, score_best), by = c('entry', 'week')) %>%
    mutate(score_capt = ifelse(is.na(score_capt), 0, score_capt)) %>%
    mutate(score_delta = score_best - score_capt) %>%
    left_join(f$league, by = c('entry')) %>%
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
#' @param f an fpl object
#' @param weeks Vector of weeks.  If empty then include all weeks
#' @param managers Vector of teams.  Vector of manager names, manager IDs or team
#'     names.  If empty then include all teams
#'
#' @return dataframe containing table
#'
#' @import dplyr
#' @importFrom tidyr spread
#' @export
playedFormation <- function(f, weeks = c(), managers = c()) {
  if (length(weeks) == 0) weeks <- seq(max(f$league_weeks$week))
  entries <- teamIDs(f, managers)

  df_formation <- f$league_weeks %>%
    filter(entry %in% entries) %>%
    filter(week %in% weeks) %>%
    select(entry, week, element, position) %>%
    group_by(entry, week) %>%
    slice(1:11) %>%
    left_join(f$players %>% select(id, element_type), by = c('element' = 'id')) %>%
    summarise(n_def = sum(element_type == 2), n_mid = sum(element_type == 3), n_fwd = sum(element_type == 4)) %>%
    mutate(formation = paste(n_def, n_mid, n_fwd, sep = '-')) %>%
    select(entry, week, formation) %>%
    ungroup() %>%
    spread(week, formation) %>%
    left_join(f$league %>% select(entry, entry_name), by = 'entry') %>%
    select(-entry) %>%
    select(entry_name, everything())
}


#' Return the best formation
#'
#' Return the formation leading to the best score in given game weeks
#'
#' @param f an fpl object
#' @param weeks Vector of weeks.  If empty then include all weeks
#' @param managers Vector of teams.  Vector of manager names, manager IDs or team
#'     names.  If empty then include all teams
#'
#' @return dataframe containing table
#'
#' @import dplyr
#' @importFrom tidyr spread
#' @export
bestFormation <- function(f, weeks = c(), managers = c()) {
  if (length(weeks) == 0) weeks <- seq(max(f$league_weeks$week))
  entries <- teamIDs(f, managers)

  df_formation <- f$league_weeks %>%
    filter(entry %in% entries) %>%
    filter(week %in% weeks) %>%
    select(entry, week, element, position) %>%
    group_by(entry, week) %>%
    left_join(f$players %>% select(id, element_type), by = c('element' = 'id')) %>%
    filter(element_type != 1) %>%
    left_join(f$stats %>% select(id, week, total_points), by = c('element' = 'id', 'week')) %>%
    arrange(week, entry, element_type, desc(total_points)) %>%
    summarise('3-4-3' = sum(total_points[1:3], total_points[6:9], total_points[11:13]),
              '3-5-2' = sum(total_points[1:3], total_points[6:10], total_points[11:12]),
              '4-3-3' = sum(total_points[1:4], total_points[6:8], total_points[11:13]),
              '4-4-2' = sum(total_points[1:4], total_points[6:9], total_points[11:12]),
              '4-5-1' = sum(total_points[1:4], total_points[6:10], total_points[11:11]),
              '5-2-3' = sum(total_points[1:5], total_points[6:7], total_points[11:13]),
              '5-3-2' = sum(total_points[1:5], total_points[6:8], total_points[11:12]),
              '5-4-1' = sum(total_points[1:5], total_points[6:9], total_points[11:11])) %>%
    ungroup()

  df_formation$max_score <- apply(df_formation[, -c(1:2)], 1, function(x) which(x == max(x)))
  df_formation$max_id <- apply(df_formation, 1, function(x) paste0(names(x['max_score'][[1]]), collapse = '; '))
  df_formation <- df_formation %>%
    select(entry, week, max_id) %>%
    spread(week, max_id) %>%
    left_join(f$league %>% select(entry, entry_name), by = 'entry') %>%
    select(-entry) %>%
    select(entry_name, everything())
  df_formation

}


#' List chip usage
#'
#' Return a data frame of weeks when chips have been used
#'
#' @param f an fpl object
#' @param managers Vector of teams.  Vector of manager names, manager IDs or team
#'     names.  If empty then include all teams
#'
#' @return dataframe containing weekly table of chips
#'
#' @import dplyr
#' @importFrom tidyr spread
#' @export
use_chip <- function(f, managers = c()) {
  entries <- teamIDs(f, managers)
  df_chips <- f$entry_weeks %>%
    filter(entry %in% entries) %>%
    filter(!chip == '') %>%
    select(entry, week, chip) %>%
    mutate(chip = if_else(grepl('wildcard', chip), 'wildcard_1', chip))

  # identify second use of wildcard
  wild_2 <- which(duplicated(df_chips[, c('entry', 'chip')]))
  if (length(wild_2) > 0) {
    df_chips[wild_2, 'chip'] <- 'wildcard_2'
  }

  # convert to wide
  df_chips <- df_chips %>%
    spread(chip, week)

  df <- f$league %>%
    filter(entry %in% entries) %>%
    select(entry, entry_name) %>%
    left_join(df_chips, by = 'entry') %>%
    select(-entry)

  return(df)
}


#' Substitutions by week
#'
#' Return a table of substitutions by manager by week
#'
#' @param f an fpl object
#' @param weeks Vector of weeks.  If empty then include all weeks.
#'     If a vector of length one then determine substitutes up to this week.
#'     If a vector of length two then determine substitutes between these weeks.
#' @param managers Vector of teams.  Vector of manager names, manager IDs or team
#'     names.  If empty then include all teams
#'
#' @return list of three dataframes.  The first contains all data and the second contains a summary table
#'     #' \itemize{
#'       \item substitute data - team, week, position, player out (and team id), player in (and team id)
#'       \item summary of subs by weekly count
#'       \item summary of subs by player names (out -> in)
#'     }
#'
#' @import dplyr
#' @importFrom tidyr spread
#' @export
substitutions <- function(f, weeks = c(), managers = c()) {

  if (length(weeks) == 0) {
    weeks <- seq(max(f$league_weeks$week))
  } else if (length(weeks) == 2) {
    weeks <- seq(weeks[1], weeks[2])
  } else if (length(weeks) == 1) {
    if (weeks == 1) {
      stop ('Cannot run just on week 1')
    } else {
      weeks <- c(weeks -1, weeks)
    }
  }
  entries <- teamIDs(f, managers)

  df_pos <- data.frame(type = c(1,2,3,4), pos = c('GLK', 'DEF', 'MID', 'FWD'), stringsAsFactors = FALSE)

  df_sub_split <- f$league_weeks %>%
    filter(entry %in% entries) %>%
    filter(week %in% weeks) %>%
    select(entry, week, element, position) %>%
    left_join(f$players %>% select(id, element_type), by = c('element' = 'id')) %>%
    arrange(entry, week, element_type, element)

  # separate by manager
  l.sub_split <- split.data.frame(df_sub_split, df_sub_split$entry)

  # look for changes in team between two weeks by position
  l.sub <- lapply(l.sub_split, function(x) {
    out <- lapply(2:length(weeks), function(i) {
      entry_id <- x[1, 'entry']
      df_w1 <- x %>%
        filter(week == weeks[i-1]) %>%
        select(element, element_type)
      df_w2 <- x %>%
        filter(week == weeks[i]) %>%
        select(element, element_type)
      delta <- setdiff(df_w1, df_w2)
      if (nrow(delta) > 0) {
        delta_rev <- setdiff(df_w2, df_w1)
        data.frame(entry = entry_id, week = weeks[i], type = delta$element_type, id_out = delta$element, id_in = delta_rev$element)
      }
    })
    out <- Filter(Negate(is.null), out)   # remove nulls
    bind_rows(out)
  })
  df_sub <- bind_rows(l.sub) %>%
    left_join(f$players %>% select(id, web_name, team_code), by = c('id_out' = 'id')) %>%
    rename(name_out = web_name, team_id_out = team_code) %>%
    left_join(f$players %>% select(id, web_name, team_code), by = c('id_in' = 'id')) %>%
    rename(name_in = web_name, team_id_in = team_code) %>%
    left_join(f$teams %>% select(code, short_name), by = c('team_id_out' = 'code')) %>%
    rename(team_out = short_name) %>%
    left_join(f$teams %>% select(code, short_name), by = c('team_id_in' = 'code')) %>%
    rename(team_in = short_name) %>%
    left_join(df_pos, by = 'type') %>%
    left_join(f$league %>% select(entry, entry_name), by = 'entry') %>%
    select(entry_name, week, pos, name_out, team_out, name_in, team_in)

  df_sub_summary_count <- f$league %>%
    filter(entry %in% entries) %>%
    select(entry, entry_name) %>%
    left_join(df_sub %>%
                select(entry_name, week) %>%
                group_by(entry_name, week) %>%
                summarise(count = n()), by = 'entry_name') %>%
    select(-entry) %>%
    spread(week, count)

  df_sub_summary_names <- f$league %>%
    filter(entry %in% entries) %>%
    select(entry, entry_name) %>%
    left_join(df_sub %>%
                mutate(transfer = paste0(name_out, ' -> ', name_in)) %>%
                select(entry_name, week, transfer) %>%
                group_by(entry_name, week) %>%
                summarise(transfer = paste0(transfer, collapse = '; ')), by = 'entry_name') %>%
    select(-entry) %>%
    spread(week, transfer)

  if('<NA>' %in% names(df_sub_summary_count)) df_sub_summary_count[['<NA>']] <- NULL
  if('<NA>' %in% names(df_sub_summary_names)) df_sub_summary_names[['<NA>']] <- NULL

  return(list(full = df_sub, summary_count = df_sub_summary_count, summary_names = df_sub_summary_names))
}


#' Substitution Analysis
#'
#' Analyze substitutions - how beneficial were the substitutions?
#' Take the substitutions applied in a game week and project out a number of weeks to determine
#' the points differential between the added player and the dropped player.
#'
#' @param f an fpl object
#' @param start_week Integer.  Week to start the analysis from (default = 2)
#' @param number_weeks Integer.  Number of weeks to run analysus (default = current week).
#' @param managers Vector of teams.  Vector of manager names, manager IDs or team
#'     names.  If empty then include all teams
#'
#' @import dplyr
#' @export
substitution_analysis <- function(f, start_week = 2, number_weeks = 1, managers = c()) {
  if (start_week < 2) stop ('start_week must be at least 2')
  if (number_weeks < 1) stop ('number_weeks must be at least 1')

  weeks <- start_week:(start_week + number_weeks - 1)
  if (weeks[length(weeks)] > f$last_week) warning(paste0('Number of weeks exceeds total weeks.  Calculation will run to week ', f$last_week))

  l.subs <- substitutions(f, weeks = start_week, managers = managers)
  df.subs <- l.subs[[1]] %>%
    mutate(r = row_number())

  ## separate by players in and players out and switch to a long format
  df.subs_l <- bind_rows(df.subs %>% select('r', 'entry_name', 'week', 'pos', name = 'name_out', team = 'team_out') %>% mutate(direction = 'out'),
                         df.subs %>% select('r', 'entry_name', 'week', 'pos', name = 'name_in', team = 'team_in') %>% mutate(direction = 'in')) %>%
    left_join(f$teams %>% select(code, short_name), by = c('team' = 'short_name')) %>%
    left_join(f$players %>% select(id, web_name, team_code), by = c('name' = 'web_name', 'code' = 'team_code'))

  ## calculate score over multiple weeks and join
  df.subs_score <- df.subs_l %>%
    left_join(f$stats %>%
                select(id, week, total_points) %>%
                filter(week %in% weeks) %>%
                group_by(id) %>%
                summarise(points = sum(total_points)),
              by = 'id')

  ## split table by players in / out and rejoin
  df.subs_final <- df.subs_score %>%
    filter(direction == 'in') %>%
    select(r, entry_name, pos, 'name_in' = name, 'team_in' = team, 'points_in' = points) %>%
    left_join(df.subs_score %>%
                filter(direction == 'out') %>%
                select(r, 'name_out' = name, 'team_out' = team, 'points_out' = points),
              by = 'r') %>%
    mutate(points_gained = points_in - points_out) %>%
    select(-r)
  return(df.subs_final)
}

#' Determine points left on bench
#'
#' Calculate number of points left on bench as total and as percentage of weekly score
#'
#' @param f an fpl object
#' @param weeks Vector of weeks.  If empty then include all weeks
#' @param managers Vector of teams.  Vector of manager names, manager IDs or team
#'     names.  If empty then include all teams
#'
#' @return list of two dataframes
#'
#' @import dplyr
#' @importFrom tidyr spread
#' @export
points_on_bench <- function(f, weeks = c(), managers = c()) {
  if (length(weeks) == 0) weeks <- seq(max(f$league_weeks$week))
  entries <- teamIDs(f, managers)

  # calculate points left on bench
  df_pob <- f$league_weeks %>%
    filter(entry %in% entries) %>%
    filter(week %in% weeks) %>%
    group_by(entry, week) %>%
    slice(12:15) %>%
    left_join(f$stats %>% select(id, week, total_points), by = c('element' = 'id', 'week')) %>%
    select(entry, week, total_points) %>%
    summarise(points_on_bench = sum(total_points))

  # calculate weekly points (no captain multiplier or chips)
  df_weekly_points <- f$league_weeks %>%
    filter(entry %in% entries) %>%
    filter(week %in% weeks) %>%
    group_by(entry, week) %>%
    slice(1:11) %>%
    left_join(f$stats %>% select(id, week, total_points), by = c('element' = 'id', 'week')) %>%
    select(entry, week, total_points) %>%
    summarise(points_on_field = sum(total_points)) %>%
    left_join(df_pob, by = c('entry', 'week')) %>%
    left_join(f$league %>% select(entry, entry_name), by = c('entry')) %>%
    mutate(percent = as.integer(100 * points_on_bench / points_on_field)) %>%
    ungroup() %>%
    select(entry_name, week, points_on_field, points_on_bench, percent)

  df_pob_wide <- df_pob %>%
    spread(week, points_on_bench) %>%
    left_join(f$league %>% select(entry, entry_name), by = c('entry')) %>%
    select(-entry) %>%
    select(entry_name, everything())

  return(list(all = df_weekly_points, pob = df_pob_wide))
}


#' Was the best keeper selected?
#'
#' Retrieve a table indicating points differential between the keeper chosen and the
#'     best choice
#'
#' @param f an fpl object
#' @param weeks Vector of weeks.  If empty then include all weeks
#' @param managers Vector of teams.  Vector of manager names, manager IDs or team
#'     names.  If empty then include all teams
#'
#' @return dataframe
#'
#' @import dplyr
#' @importFrom tidyr spread
#' @export
keeperChoice <- function(f, weeks = c(), managers = c()) {
  if (length(weeks) == 0) weeks <- seq(max(f$league_weeks$week))
  entries <- teamIDs(f, managers)

  df_keepers <- f$league_weeks %>%
    filter(entry %in% entries) %>%
    filter(week %in% weeks) %>%
    group_by(entry, week) %>%
    filter(position %in% c(1, 12)) %>%
    left_join(f$stats %>% select(id, week, total_points), by = c('element' = 'id', 'week')) %>%
    select(entry, week, position, element, total_points) %>%
    summarise(delta = total_points[position == 1] - total_points[position == 12]) %>%
    spread(week, delta) %>%
    left_join(f$league %>% select(entry, entry_name), by = c('entry')) %>%
    ungroup() %>%
    select(-entry) %>%
    select(entry_name, everything())


  return(df_keepers)
}
