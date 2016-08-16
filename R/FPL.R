#'getFPLData
#'
#'Get team, player and game data
#'
#' @import jsonlite
getFPLData <- function() {
  url <- 'https://fantasy.premierleague.com/drf/bootstrap-static'
  data <- jsonlite::fromJSON(url)
  return(data)
}

#'getLeagueTable
#'
#'Get a league table
#'
#' @import jsonlite
getLeagueTable <- function(leagueID = NULL, wk = 1) {
  if (is.null(leagueID)) return()
  url <- paste0('https://fantasy.premierleague.com/drf/leagues-classic-standings/', leagueID, '?phase=', wk)
  data <- jsonlite::fromJSON(url)
  return(data)
}

#'getEntry
#'
#'Get an entry info
#'
#' @import jsonlite
getEntry <- function(entry = NULL) {
  if (is.null(entry)) return()
  url <- paste0('https://fantasy.premierleague.com/drf/entry/', entry)
  data <- jsonlite::fromJSON(url)
  return(data)
}

#'getTeam
#'
#'Get team data
#'
#'@import jsonlite
getTeam <- function(entry = NULL, wk = 1) {
  if (is.null(entry)) return()
  url <- paste0('https://fantasy.premierleague.com/drf/entry/', entry, '/event/', wk, '/picks')
  data <- jsonlite::fromJSON(url)
  return(data)
}
