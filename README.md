# fplR

## Update

A new series of functions designed to work with stored data for a specific league

Data are pulled back from the Fantasy Premierleague site and stored in a mysql database.  Once available a number of functions can be applied to the data.

### Functions
*  `initialize_database(db, leagueID)` Initialize a mysql database for the league.  `db` is the database object.
*  `add_to_database(db, leagueID, stats=FALSE, league=FALSE, weeks=c(), all=FALSE, progressbar=FALSE)` Add data from Fantasy Premierleague site to the database.  `db` is the database object and leagueID is the league number from the Fantasy Premierleague site.  if `stats` or `league` are true then the stats and league tables are updated.  Data are downloaded from the Fantasy Premierleague site by setting values for the `weeks` variable or setting `all` to true for all weeks.
*  `fpl(db)` Return an fpl object from the database.  The fpl object contains all tables for subsequent functions (assigned to f for the functions that follow)
*  `fpl_league_weekly(f, weeks)` Return weekly scores for weeks `weeks`
*  `fpl_league(f, max_week = 0, out_type = 'total')` Return the league table (cumulative) for up to `max_week`.  The output can be total to `max_week` or cumulative
*  `squad_by_week(f, week, teams)` Return a manager's squad for a specific week.  `teams` is a list of identifiers which can be manager names, manager IDs or team names
*  `captainChoice(f, weeks, managers)` Determine if the best captain was chosen for a number of weeks
*  `playedFormation(f, weeks, managers)` Return the formation played for a number of weeks
*  `bestFormation(f, weeks, managers)`  Return the optimum formation for a number of weeks
*  `use_chip(f, managers)` List chip usage
*  `substitutions(f, weeks, managers)` Return tables containing substitution information
*  `substitution_analysis(f, start_week, number_weeks, managers)` Analyze substitutions - how advantageous were the substitutions?  Take the substitutions applied in week `start_week` and project out `number_weeks` to determine the points differential between the added player and the dropped player.
*  `substitution_analysis_all(f, managers)` Analyze substitutions - how advantageous were the substitutions?  This function assesses points differential from when a player was substituted in to when the same player was dropped (or to the maximum number of weeks if the player was not dropped).
*  `points_on_bench(f, weeks, managers)` Return tables of points left on the bench
*  `keeperChoice(f, weeks, managers)` Assessment of keeper choice

## Original functions

Functions to assist scraping data from Fantasy Premierleague Site.

This repository contains a simple series of functions to aid in scraping data from the new format Fantasy Premierleague site.  

### Functions
*  `getFPLData()` Return a list of 7 components containing all the juicy data (phases, elements, game-settings, total-players, teams, element_types and events).
*  `getEntry(entry)` Return entry details and historical information.
*  `getEvent(week)` Return data relating to a specific week.  Output is a list with two elements (fixtures and elements).
*  `getTeam(entry, wk)` Get team data (week specified).
*  `getLeagueTable(leagueID, wk)` Return a league table for a classic league (week specified).  If week is `NULL` then return the most current table.
*  `getLeagueEntries(leagueID, wk, fpl)` Return the teams in a league table for a classic league (week specified).  `fpl` is the output returned by `getFPLData()`.
*  `playerCount(l.teams, fpl)` Return a data frame containing all players and their frequency in a particular league.  `fpl` is the output returned by `getFPLData()`.  `l.teams` is the list of teams returned by `getLeagueEntries(leagueID, wk, fpl)`.  The resulting data frame can be effectively sliced and diced using `dplyr`, for example the following code returns all unselected midfielders ordered by total points:

    ```r
    playerCount(l.teams, fpl) %>% filter(count == 0 & element_type == 'MID') %>% arrange(-total_points)
    ```
*  `pointsFrame(fpl, maxWeek)` Return a data frame containing id, first name, second name points scored for all weeks up to `maxWeek`.  `fpl` is the output returned by `getFPLData()`.  If `maxWeek` is NULL then the current game week is assumed.
