#' create an fpl class
#'
#' Create an FPL object from a stored database
#'
#' @param db.  A database object
#'
#' @import DBI
#'
#' @export
fpl <- function(db) {

  self <- structure(
    list(
      league = data.frame(),
      league_weeks = data.frame(),
      entry_weeks = data.frame(),
      players = data.frame(),
      stats = data.frame(),
      teams = data.frame(),
      weeks = NULL,
      last_week = NULL
    )
  )

  df.league <- dbReadTable(db, 'league')
  df.league_weeks <- dbReadTable(db, 'league_weeks')
  df.entry_weeks <- dbReadTable(db, 'entry_weeks')
  df.players <- dbReadTable(db, 'players')
  df.stats <- dbReadTable(db, 'stats')
  df.teams <- dbReadTable(db, 'teams')
  weeks <- sort(unique(df.league_weeks$week))
  self <- list(
    league = df.league,
    league_weeks = df.league_weeks,
    entry_weeks = df.entry_weeks,
    players = df.players,
    stats = df.stats,
    teams = df.teams,
    weeks = weeks,
    last_week = weeks[length(weeks)]
  )

  class(self) <- 'fpl'

  return(self)
}


#' summarize fpl object
#'
#' print a summary of an fpl object
#'
#' @return Nothing
#'
#' @export
summary.fpl <- function(object, ...) {
  out <- object
  cat("Number of league entries:", nrow(out$league), "\n")
  cat("Number of weeks:", length(f$weeks), "\n")
  cat("Number of players:", nrow(f$players), "\n")
  return(invisible(NULL))
}
