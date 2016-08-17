#' Get team, player and game data
#'
#' Retrieve fantasy football data from fantasy.premierleague.com site.
#'
#' @return a list containing phases, elements, game-settings, total-players, teams,
#' element_types and events
#'
#' @import jsonlite
#' @export
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
#' @export
getLeagueTable <- function(leagueID = NULL, wk = 1) {
  if (is.null(leagueID)) return()
  url <- paste0('https://fantasy.premierleague.com/drf/leagues-classic-standings/', leagueID, '?phase=', wk)
  data <- jsonlite::fromJSON(url)
  return(data)
}


#' Get all entries for a particular league
#'
#' Return a list of dataframes each containing an entry
#'
#' @param leagueID Numeric league identifier
#' @param wk Week number
#' @param fplData Output from getFPLData
#'
#' @import dplyr
#' @export
getLeagueEntries <- function(leagueID, wk = 1, fplData = NULL) {
  if (is.null(fpl)) fpl <- getFPLData()
  leagueTableData <- getLeagueTable(leagueID, wk)
  df.leagueTable <- leagueTableData$standings$results
  entries <- df.leagueTable$entry
  l.team <- list()
  for (i in 1:length(entries)) {
    allData <- getTeam(entries[[i]], wk)
    l.team[[i]] <- allData$picks
    Sys.sleep(0.1)
  }
  l.team <- lapply(l.team, function(x) x%>% left_join(fpl$elements %>% select(id, first_name, second_name, element_type, team), c('element' = 'id'))
  )
  return(l.team)
}

#' Get an entry info
#'
#' Return entry details and historical league information
#'
#' @param entry Numeric entry identifier
#'
#' @import jsonlite
#' @export
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
#' @import jsonlite
#' @export
getTeam <- function(entry = NULL, wk = 1) {
  if (is.null(entry)) return()
  url <- paste0('https://fantasy.premierleague.com/drf/entry/', entry, '/event/', wk, '/picks')
  data <- jsonlite::fromJSON(url)
  return(data)
}

# df <- team$picks
# df <- df %>%
#   left_join(fpl$elements %>%
#               select(id, first_name, second_name, element_type, team), c('element' = 'id'))
