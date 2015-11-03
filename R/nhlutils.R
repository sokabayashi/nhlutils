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
#> NULL


.onAttach <- function( libname, pkgname ) {
  packageStartupMessage( "Load Hadleyverse as well as some NHL data specific utilities." )
}
