% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fpl_db_fns.R
\name{squad_by_week}
\alias{squad_by_week}
\title{Return a manager's squad for a particular week}
\usage{
squad_by_week(f, week = 1, teams = c())
}
\arguments{
\item{f}{an fpl object}

\item{week}{Week number}

\item{teams}{Vector of teams.  Vector of manager names, manager IDs or team
names.  If empty then include all teams}
}
\value{
list of data frames containing teams
}
\description{
Return a manager's squad for a particular week
}
