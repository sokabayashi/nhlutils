# game db names -----------------------------------------------------------

#' Convert (season, session_id, game_id4) triplet into game_id10
#'
#' @param season String of form "20152016"
#' @param session_id String of "2" or "3"
#' @param game_id4 String like "0097"
#' @export
get_game_id10 <- function( season, session_id, game_id4 ) {
  paste0( substr( season, 1,4), "0", session_id, game_id4 )
}


#' Convert game_id10 into (season, session_id, game_id4)
#'
#' @param game_id10 10-character game id, e.g., "2015020037".
#'    Note that the embedded year is the season *start* year.  This
#'    might be unexpected behavior but this is what GC Live does.
#' @return Data frame with season (8-character), session_id, game_id4
#' @export
split_game_id10 <- function( game_id10 ) {
  season     <- game_id10 %>% substr( 1, 4 ) %>% as.numeric()
  season     <- paste0( season, season+1 ) %>% as.character()
  session_id <- game_id10 %>% substr( 6, 6 )
  game_id4   <- game_id10 %>% substr( 7, 10) %>% as.character()

  data_frame( season, session_id, game_id4 )
}


#' Convert (ha, number) pair into ha_number
#'
#' @param ha String "H" or "A"
#' @param number Jersey number
#' @return String of form ha_number, unique to a player in a game.
#' @export
get_ha_number <- function( ha, number ) {
  sprintf( "%s%02.f", ha, as.numeric(number) )
}


#' Get today's games from GameCenter Live site
#'
#' @param back Days back from today (Sys.Date()).  0 is today, -1 is yestday.
#' @param game_date String in YYYMMDD form.
#' use game_date if both back and game_date are specified.
#' @return Data frame of season, session_id, game_id4, for game_date games.
#' @export
get_todays_games_df <- function(back = 0, game_date=NULL) {

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
  gc_gamelist <- gsub( "loadScoreBoard", '', gc_gamelist, fixed=T )
  gc_gamelist <- sub('[^\\{]*', '', gc_gamelist) # remove function name and opening parenthesis
  gc_gamelist <- sub('\\)\n$',  '', gc_gamelist) # remove closing parenthesis and \n
  gc_gamelist <- fromJSON( gc_gamelist )
  gc_gamelist <- gc_gamelist$games

  if( !length( gc_gamelist ) ) {
    message( "No GC games found for date: ", game_date )
    return( NULL )
  }
  print( gc_gamelist %>% select( ata, hta, id ) )

  games_df <- gc_gamelist$id %>% split_game_id10()
  games_df$away_team_short <- gc_gamelist$ata
  games_df$home_team_short <- gc_gamelist$hta

  message( game_date, " games, according to GC: ", paste( games_df$game_id4, collapse = " " ) )

  games_df
}
