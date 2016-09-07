
# Time functions ----------------------------------------------------------

#' Convert MM:SS time to MM + SS/60.
#'
#' @param mmss A time in MM:SS format
#' @return Rounded numeric value of decimal time.
#' @export
#' @examples
#' time_mmss_to_decimal( "5:16" )
time_mmss_to_decimal <- function( mmss, digits=3 ) {

  split <- mmss %>% as.character() %>% strsplit( ":" )

  ret_df <- ldply( split, function(x) {
    mm <- ifelse( length(x) >= 1,
                  x[[1]] %>% as.numeric(), NA )
    ss <- ifelse( length(x) > 1,
                  x[[2]] %>% as.numeric(), 0 )
    data_frame( mm=mm, ss=ss )
  } )

  # split_length <- lapply( split, length )
  # mm    <- split %>% pluck(1) %>% as.numeric()
  # ss    <- ifelse( split_length > 1, split %>% pluck(2) %>% as.numeric(), 0 )

  round( ret_df$mm + (ret_df$ss/60), digits )
}

#' Convert x.y decimal time to MM:SS.
#'
#' @param time_decimal A numerical value of time.
#' @return a string of time
#' @export
time_decimal_to_mmss <- function( time_decimal ) {

  mm      <- floor( time_decimal )
  mm_frac <- time_decimal %% 1
  paste0(mm, ":", sprintf("%02d", round(mm_frac * 60, 0)))
}

#' Convert scoreboard (period, clock time) to cumulative time
#'
#' @param period Numeral representing period.
#' @param clocktime String with MM:SS format
#' @param session_id A string, "2" or "3" to indicate regular season or playoffs.
#' Defaults to "2".
#' @return A numeric value denoting cumulative time in min
#' from beginning of game.
#' @export
clock_time_to_start_cum <- function( period, clock_time, session_id="2", round=3 ) {

  period_time_left <- time_mmss_to_decimal( clock_time )
  time_period <- 20
  if( session_id=="2" && period==4 ) {
    time_period <- 5
  }
  period_time_cum <- time_period - period_time_left

  round( (period-1)*20 + period_time_cum, 3 )
}


# Name functions ----------------------------------------------------------

#' Convert 3-ltter team name to our standard 3-letter team_short
#'
#' @param team_short 3-letter team name
#' @return corrected (if necessary) team_short.
#'    Unchanged, unless team is T.B, N.J, S.J, or L.A.
#' @examples
#' clean_team_short( "T.B" )
#' @export
clean_team_short <- function( team_short ) {
  team_short %>%
    gsub( "T.B", "TBL", ., fixed=T ) %>%
    gsub( "N.J", "NJD", ., fixed=T ) %>%
    gsub( "S.J", "SJS", ., fixed=T ) %>%
    gsub( "L.A", "LAK", ., fixed=T )
}

#' Capitalize the first letter of each word (usually names)
#'
#' @param x A name, first and last separated by space
#' @return String with first letter capitalized
#' @examples
#' simple_cap( "ben smith" )
#' @export
simple_cap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
    sep="", collapse=" ")
}

#' Height to inches.
#'
#' @param ht String of height in form 5'11" or 5'11.
#' @param sep String of foot-inches separator, default to "'".
#'
#' @return Integer of height in inches
#' @export
#'
#' @examples height_to_inches( "5'11" )
height_to_inches <- function( ht, sep="'" ) {
  if( is.na( ht) ) return( NA )

  split <- strsplit( ht, sep ) %>% unlist()

  as.numeric(split[1])*12 + parse_number( split[2])
}


