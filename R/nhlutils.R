#' nhlutils: A package for working with data scraped from NHL.com
#'
#' The nhlutils package provides functions to handle the
#' problems that commonly emerge when working with NHL data.
#'
#'  @section baseutils:
#'  These are simple functions to deal with frequently occuring
#'  conversions of time and name formats.
#'
#'  @docType package
#'  @name nhlutils
NULL

.onAttach <- function( libname, pkgname ) {
  packageStartupMessage( "nhlutils loads Hadleyverse as well as some new NHL-specific utilities.")
}
