#' nhlutils: A package for working with data scraped from NHL.com
#'
#' The nhlutils package provides functions to handle the annoying little
#' problems that commonly emerge when working with NHL data:
#'  time formats
#'  player and team names
#'
#'  @section Time functions:
#'
#'  @docType package
#'  @name nhlutils
NULL

clean_teamName3 <- function( value ) {
  value %>% gsub( "T.B", "TBL", ., fixed=T ) %>%
    gsub( "N.J", "NJD", ., fixed=T ) %>%
    gsub( "S.J", "SJS", ., fixed=T ) %>%
    gsub( "L.A", "LAK", ., fixed=T )
}


get_game_id10 <- function( season, session_id, game_id4 ) {
  paste0( substr( season, 1,4), "0", session_id, game_id4 )
}



#' Capitalize the first letter of each word (usually names)
#'
#' @param x A name, first and last separated by space
#' @return String with first letter capitalized
#' @examples
#' simple_cap( "ben smith" )
simple_cap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
    sep="", collapse=" ")
}

## http://stackoverflow.com/questions/5186972/how-to-convert-time-mmss-to-decimal-form-in-r
time_convert <- function(x, y, digits=3) {
  x <- as.numeric(x)
  y <- as.numeric(y)

  round( x + (y/60), digits )
}

time_convert_mmss <- function( mmss, digits=3 ) {
  split <- mmss %>% as.character() %>% strsplit( ":" )
  mm    <- split %>% pluck(1) %>% as.numeric()
  ss    <- split %>% pluck(2) %>% as.numeric()

  round( mm + (ss/60), digits )
}

time_convert_back <- function(x) {
  int <- floor(x)
  frac <- x %% 1
  paste0(int, ":", sprintf("%02d", round(frac * 60, 0)))
}

get_ha_number <- function( ha, number ) {
  sprintf( "%s%02.f", ha, as.numeric(number) )
}

njd.parse_game_id10 <- function( game_id10 ) {
  season     <- game_id10 %>% substr( 1, 4 ) %>% as.numeric()
  season     <- paste0( season, season+1 ) %>% as.character()
  session_id <- game_id10 %>% substr( 6, 6 )
  game_id4   <- game_id10 %>% substr( 7, 10) %>% as.character()

  data.frame( season, session_id, game_id4, stringsAsFactors=FALSE )
}

# njd.get_todays_game_df ---------------------------------------------------------

njd.get_todays_games_df <- function(back = 0, game_date=NULL) {
  ## use back = -1 for yesterday, for example

  if( is.null(game_date) ) {
    game_date <- format(Sys.Date() + back, "%Y-%m-%d")
  } else {
    game_date <- as.Date( game_date, format="%Y%m%d" )
  }

  gc_url    <- sprintf( "http://live.nhle.com/GameData/GCScoreboard/%s.jsonp", game_date )
  gc_get    <- gc_url %>% read_html()

  if( is.null(gc_get) ) {
    message( "No data for: ", game_date )
    return( NULL )
  }

  gc_gamelist <- gc_get %>% html_node( "p") %>% html_text()
  gc_gamelist <- gsub( "loadScoreBoard", '', gc_gamelist, fixed=T)
  gc_gamelist <- sub('[^\\{]*', '', gc_gamelist) # remove function name and opening parenthesis
  gc_gamelist <- sub('\\)\n$',  '', gc_gamelist) # remove closing parenthesis and \n
  gc_gamelist <- fromJSON( gc_gamelist )
  gc_gamelist <- gc_gamelist$games

  if( !length( gc_gamelist ) ) {
    message( "No GC games found for date: ", game_date )
    return( NULL )
  }
  print( gc_gamelist %>% select( ata, hta, id ) )

  games_df <- gc_gamelist$id %>% njd.parse_game_id10()
  message( "Today's games, according to GC: ", paste( games_df$game_id4, collapse = " " ) )

  games_df
}
