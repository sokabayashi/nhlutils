% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gameutils.R
\name{get_todays_games_df}
\alias{get_todays_games_df}
\title{Get today's games from GameCenter Live site}
\usage{
get_todays_games_df(back = 0, game_date = NULL)
}
\arguments{
\item{back}{Days back from today (Sys.Date()).  0 is today, -1 is yestday.}

\item{game_date}{String in YYYMMDD form.
use game_date if both back and game_date are specified.}
}
\value{
Data frame of season, session_id, game_id4, for game_date games.
}
\description{
Get today's games from GameCenter Live site
}

