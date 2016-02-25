#' nhlutils: A package for working with data scraped from NHL.com
#'
#' The nhlutils package provides functions to handle the
#' problems that frequently emerge when working with NHL data.
#'
#'  baseutils:
#'  These are simple functions to deal with conversions of time
#'  and name formats.
#'
#'  gameutils:
#'  working with NHL-specific game data, involving their standard formats
#'  for season, session, game IDs.
#'
#' @details Details
"_PACKAGE"



.onAttach <- function( libname, pkgname ) {
  packageStartupMessage( "Load Hadleyverse as well as some NHL data specific utilities." )
}
