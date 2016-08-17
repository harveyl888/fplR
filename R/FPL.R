#' Get team, player and game data
#'
#' Retrieve fantasy football data from fantasy.premierleague.com site.
#'
#' @return a list containing phases, elements, game-settings, total-players, teams,
#' element_types and events
#'
#' @import jsonlite
getFPLData <- function() {
  url <- 'https://fantasy.premierleague.com/drf/bootstrap-static'
  data <- jsonlite::fromJSON(url)
  return(data)
}

#' Get a league table
#'
#' Retrieve a fantasy football league table for a specific week
#'
#' @param leagueID Numeric league identifier
#' @param wk Week number
#'
#' @import jsonlite
getLeagueTable <- function(leagueID = NULL, wk = 1) {
  if (is.null(leagueID)) return()
  url <- paste0('https://fantasy.premierleague.com/drf/leagues-classic-standings/', leagueID, '?phase=', wk)
  data <- jsonlite::fromJSON(url)
  return(data)
}

#' Get an entry info
#'
#' Return entry details and historical league information
#'
#' @param entry Numeric entry identifier
#'
#' @import jsonlite
getEntry <- function(entry = NULL) {
  if (is.null(entry)) return()
  url <- paste0('https://fantasy.premierleague.com/drf/entry/', entry)
  data <- jsonlite::fromJSON(url)
  return(data)
}

#' Get team data
#'
#' Return team information from a specific week.
#'
#' @param entry Numeric entry identifier
#' @param wk Week number
#'
#'@import jsonlite
getTeam <- function(entry = NULL, wk = 1) {
  if (is.null(entry)) return()
  url <- paste0('https://fantasy.premierleague.com/drf/entry/', entry, '/event/', wk, '/picks')
  data <- jsonlite::fromJSON(url)
  return(data)
}
