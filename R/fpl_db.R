#' Add initial data to a MySql database
#'
#' Retrieve fantasy football data from fantasy.premierleague.com site and
#' add to a MySQL database
#'
#' @param db open database object returned from dbConnect
#' @param leagueID Numeric league identifier
#'
#' @import DBI
#' @import dplyr
#' @export
initialize_database <- function(db, leagueID=NULL) {
  if (is.null(leagueID)) stop('No leagueID specified')

  ## grab data
  fpl <- getFPLData()

  ## get league table
  league <- getLeagueTable(leagueID)
  df.league_teams <- league$standings$results %>%
    arrange(player_name) %>%
    select(entry, entry_name, player_name)

  ## get team table
  df.teams <- fpl$teams %>%
    select(code, name, short_name)

  ## get player table
  df.players <- fpl$elements %>%
    select(id, web_name, first_name, second_name, element_type, team_code)

  ## write to database
  out.league <- dbWriteTable(db, 'league', df.league_teams, row.names = FALSE, overwrite = TRUE)
  out.teams <- dbWriteTable(db, 'teams', df.teams, row.names = FALSE, overwrite = TRUE)
  out.players <- dbWriteTable(db, 'players', df.players, row.names = FALSE, overwrite = TRUE)

  return(list(out.league, out.teams, out.players))
}

## if null return 0 otherwise return value
.nullCheck <- function(x) {
  ifelse(is.null(x), 0, x)
}

#' Add selected data to a MySql database
#'
#' Retrieve fantasy football data from fantasy.premierleague.com site and
#' add to a MySQL database
#'
#' @param leagueID Numeric league identifier
#' @param stats Boolean.  If true update stats table
#' @param league Boolean.  If true update league table
#' @param weeks vector of weeks to collect data for
#' @param all Boolean.  If true collect data for all weeks (overrides weeks vector)
#' @param all Boolean.  If true a progress bar is displayed
#'
#' @import DBI
#' @import dplyr
#' @export
add_to_database <- function(db, leagueID=NULL, stats=FALSE, league=FALSE, weeks=c(), all=FALSE, progressbar=FALSE) {
  if (is.null(leagueID)) stop('No leagueID specified')

  ## get range of weeks for all data
  if (all) {
    fpl <- getFPLData()
    currentWeek <- which(fpl$events$is_current)
    weeks <- seq(currentWeek)
  }

  out.stats <- FALSE
  out.league <- FALSE
  out.entry <- FALSE

  if (stats == TRUE) {
    if (progressbar) pb <- txtProgressBar(0, length(weeks), style = 3)
    ## get stats data
    l.stats <- lapply(seq_along(weeks), function(w) {
      if (progressbar) setTxtProgressBar(pb, w)
      Sys.sleep(1)
      week = weeks[w]
      weekly_data <- getEvent(week)
      df <- bind_rows(lapply(weekly_data$elements, function(x) x$stats)) %>%
        mutate(id = as.numeric(names(weekly_data$elements))) %>%
        mutate(week = week) %>%
        select(id, week, everything())
      df$id <- as.numeric(names(weekly_data$elements))
      df
    })
    if (progressbar) close(pb)
    df.stats <- bind_rows(l.stats)

    ## write stats to database
    if (all) {
      out.stats <- dbWriteTable(db, 'stats', df.stats, row.names = FALSE, overwrite = TRUE)
    } else {
      dummy <- dbExecute(db, paste0('DELETE FROM stats WHERE week IN (', paste0(weeks, collapse = ','), ')'))
      out.stats <- dbWriteTable(db, 'stats', df.stats, row.names = FALSE, append = TRUE)
    }
  }

  if (league == TRUE) {
    ## get league entries
    league <- getLeagueTable(leagueID)
    league_entries <- league$standings$results %>%
      select(entry) %>%
      unlist(use.names = FALSE)

    ## get league data
    if (progressbar) pb <- txtProgressBar(0, length(weeks), style = 3)
    l.league_week <- lapply(seq_along(weeks), function(w) {
      if (progressbar) setTxtProgressBar(pb, w)
      week <- weeks[w]
      l.singleWeek <- lapply(league_entries, function(t) {
        Sys.sleep(0.2)  ## wait a little
        teamData <- getTeam(entry = t, wk = week)
        df.teamData <- teamData$picks %>%
          mutate(entry = t) %>%
          mutate(week = week) %>%
          mutate(is_captain = as.numeric(is_captain)) %>%
          mutate(is_vice_captain = as.numeric(is_vice_captain)) %>%
          select(entry, week, everything())
        df.entryHistory <- data.frame(
          entry = t,
          week = week,
          num_transfers = .nullCheck(teamData$entry_history$event_transfers),
          cost_transfers = .nullCheck(teamData$entry_history$event_transfers_cost),
          point = .nullCheck(teamData$entry_history$points),
          total_points = .nullCheck(teamData$entry_history$total_points),
          points_on_bench = .nullCheck(teamData$entry_history$points_on_bench),
          team_value = .nullCheck(teamData$entry_history$value),
          chip = teamData$active_chip,
          stringsAsFactors = FALSE
        )
        list(team = df.teamData, entry = df.entryHistory)
      })
      list(
        team = bind_rows(lapply(l.singleWeek, function(x) {x$team})),
        entry = bind_rows(lapply(l.singleWeek, function(x) {x$entry}))
      )
    })
    if (progressbar) close(pb)
    df.league_week <- bind_rows(lapply(l.league_week, function(x) {x$team}))
    df.entry_week <- bind_rows(lapply(l.league_week, function(x) {x$entry}))

    ## write league to database
    if (all) {
      out.league <- dbWriteTable(db, 'league_weeks', df.league_week, row.names = FALSE, overwrite = TRUE)
      out.entry <- dbWriteTable(db, 'entry_weeks', df.entry_week, row.names = FALSE, overwrite = TRUE)
    } else {
      dummy <- dbExecute(db, paste0('DELETE FROM league_weeks WHERE week IN (', paste0(weeks, collapse = ','), ')'))
      out.league <- dbWriteTable(db, 'league_weeks', df.league_week, row.names = FALSE, append = TRUE)
      dummy <- dbExecute(db, paste0('DELETE FROM entry_weeks WHERE week IN (', paste0(weeks, collapse = ','), ')'))
      out.entry <- dbWriteTable(db, 'entry_weeks', df.entry_week, row.names = FALSE, append = TRUE)
    }
  }
  return(list(out.stats, out.league, out.entry))
}

#' Read data back from a MySQL database
#'
#' Retrieve all tables fpr subsequent analysis
#'
#' @param db open database object returned from dbConnect
#' @return named list of database tables
#'
#' @import DBI
#' @export
read_database <- function(db) {
  df.league <- dbReadTable(db, 'league')
  df.league_weeks <- dbReadTable(db, 'league_weeks')
  df.entry_weeks <- dbReadTable(db, 'entry_weeks')
  df.players <- dbReadTable(db, 'players')
  df.stats <- dbReadTable(db, 'stats')
  df.teams <- dbReadTable(db, 'teams')
  return(list(league = df.league,
              league_weeks = df.league_weeks,
              entry_weeks = df.entry_weeks,
              players = df.players,
              stats = df.stats,
              teams = df.teams))
}
