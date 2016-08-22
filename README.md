# fplR
Functions to assist scraping data from Fantasy Premierleague Site.

This repository contains a simple series of functions to aid in scraping data from the new format Fantasy Premierleague site.  

### Functions
*  `getFPLData()` Return a list of 7 components containing all the juicy data (phases, elements, game-settings, total-players, teams, element_types and events).
*  `getEntry(entry)` Return entry details and historical information.
*  `getEvent(week)` Return data relating to a specific week.  Output is a list with two elements (fixtures and elements).
*  `getTeam(entry, wk)` Get team data (week specified).
*  `getLeagueTable(leagueID, wk)` Return a league table for a classic league (week specified).
*  `getLeagueEntries(leagueID, wk, fpl)` Return the teams in a league table for a classic league (week specified).  `fpl` is the output returned by `getFPLData()`.
*  `playerCount(l.teams, fpl)` Return a data frame containing all players and their frequency in a particular league.  `fpl` is the output returned by `getFPLData()`.  `l.teams` is the list of teams returned by `getLeagueEntries(leagueID, wk, fpl)`.  The resulting data frame can be effectively sliced and diced using `dplyr`, for example the following code returns all unselected midfielders ordered by total points:

    ```r
    playerCount(l.teams, fpl) %>% filter(count == 0 & element_type == 'MID') %>% arrange(-total_points)
    ```
*  `pointsFrame(fpl, maxWeek)` Return a data frame containing id, first name, second name points scored for all weeks up to maxWeek.  If maxWeek is NULL then the current game week is assumed.
