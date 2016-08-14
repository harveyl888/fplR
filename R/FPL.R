#'FPLScrape
#'
#'Scrape team, player and game data
#'
#' @import jsonlite
FPLScrape <- function() {
  url <- 'https://fantasy.premierleague.com/drf/bootstrap-static'
  data <- jsonlite::fromJSON(url)
  return(data)
}
