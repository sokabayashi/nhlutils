# game db names -----------------------------------------------------------

#' Convert (season, session_id, game_id4) triplet into game_id10
#'
#' @param season String of form "20152016"
#' @param session_id String of "2" or "3"
#' @param game_id4 String like "0097"
#' @return 10-char game ID.  NOTE: uses season *start* ("2015" of "20152016").
#' This is GC convention, not mine.
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


#' internal function. Extract NHL ID and name from GC Roster row.
#'
#' @param gc_row A row from GC roster.
#' @return (NHL ID, last_name, first_name, position)
#' Don't export.
get_gc_name_id <- function( gc_row ) {
  this_nhl_id <- gc_row$id

  if( is.null( this_nhl_id ) ) {
    warning( homeaway, "team ", gc_row$name, " has no NHL ID in gamecenter!!" )
    this_nhl_id <- NA
  }

  c( nhl_id       = this_nhl_id,
    gc_last_name  = iconv( toupper(gc_row$last),  to="ASCII//TRANSLIT" ),
    gc_first_name = iconv( toupper(gc_row$first), to="ASCII//TRANSLIT" ),
    gc_position   = gc_row$pos
  )
}


#' internal function. Get roster from GameCenter Live site
#'
#' @param back Days back from today (Sys.Date()).  0 is today, -1 is yestday.
#' @param game_date String in YYYMMDD form.
#' use game_date if both back and game_date are specified.
#' @return Data frame of season, session_id, game_id4, for game_date games.
#' Don't export.
get_roster_from_list <- function( homeaway, raw_list ) {
  number <- names( raw_list[[ homeaway ]] ) %>% gsub( "sw", "", .) %>% as.integer()
  roster <- ldply( raw_list[[ homeaway ]], .fun = get_gc_name_id )
  # roster$number    <- numbers
  roster$ha  <- if( homeaway=="home" ) {
    "H"
  } else  "A"

  roster$ha_number <- get_ha_number( roster$ha, number )

  return( roster )
}

#' Scrape roster from GameCenter Live site for a specific game
#'
#' @param season String of form "20152016"
#' @param session_id String of session ID, "2" or "3"
#' @param game_id4 String of 4-digit game ID, e.g., "0037"
#' @return Data frame of rosters for both teams
#' @export
get_gc_rosters <- function( season="20152016", session_id="2", game_id4="0001" ) {

  gc_id <- get_game_id10( season, session_id, game_id4 )
  url <- paste0("http://www.nhl.com/gamecenter/en/icetracker?id=", gc_id)

  scrape_roster <- url %>% read_html() %>%
                    html_node( "#pageBody") %>%
                    html_nodes(xpath = "script" ) %>%
                    html_text()

  # Just grab the player map
  scrape_roster <- scrape_roster[ grepl( "gcPlayerMap", scrape_roster )]
  scrape_roster <- scrape_roster %>%   # clean
                    gsub( "\n\tvar gcPlayerMap = ", "", . ) %>%
                    gsub( ";\n", "", . )

  roster_from_JSON <- fromJSON( scrape_roster )

  ha <- c( "home", "away")
  rosters <- lapply( ha, get_roster_from_list, roster_from_JSON )
  gc_rosters <- do.call( "rbind", rosters )
  gc_rosters[ ".id" ] <- NULL

  return( gc_rosters )
}


#' Get Game Summary url
#'
#' @param season String, "20152016"
#' @param session_id String "2" or "3"
#' @param game_id4 String "0001"
#'
#' @return string
#' @export
#'
#' @examples get_gs_url( "20152016", "2", "0001" )
get_gs_url <- function( season, session_id, game_id4 ) {
  sprintf( "http://www.nhl.com/scores/htmlreports/%s/GS0%s%s.HTM", season, session_id, game_id4 )
}


#' Get df of PP results from GS scraped PP vector.
#'
#' @param pp_results_vector Vector scraped from GS of form ( "1-3", "02:12" )
#' @param strength string
#'
#' @return Data frame of PP results from one strength scenario
#' @export
#'
get_pp_results_df <- function( pp_results_vector, strength="5v4" ) {
  if( length(pp_results_vector) <=1 ) {
    return( data_frame() )
  }

  data_frame( strength=strength, conversion=pp_results_vector[1], time=pp_results_vector[2] )
}


#' Bind PP dfs together
#'
#' @param this_team_short String of 3-letter short team name.
#' @param pp_5v4 Vector scraped from GS
#' @param pp_5v3 Vector scraped from GS
#' @param pp_4v3 Vector scraped from GS
#'
#' @return Data frame
#' @export
#'
bind_pp_df <- function( this_team_short, pp_5v4, pp_5v3, pp_4v3 ) {
  pp_df <- data_frame()
  pp_df <- bind_rows( pp_df,
    get_pp_results_df( pp_5v4, "5v4" ),
    get_pp_results_df( pp_5v3, "5v3" ),
    get_pp_results_df( pp_4v3, "4v3" ) )

  pp_df <- cbind( team_short=this_team_short, pp_df, stringsAsFactors = F )

  pp_df
}



#' Get PP results data frame from a single game
#'
#' @param season String for season, e.g., "20152016"
#' @param session_id String for session id, "2" or "3"
#' @param game_id4 String of 4-digit game ID, e.g.,  "0003"
#' @param team_tbl Collected team_tbl
#'
#' @return Data frame with PP results for this game
#' @export
#'
get_pp_results <- function( season, session_id, game_id4, team_tbl ) {
  gs_url <- get_gs_url( season, session_id, game_id4 )
  gs_html <- gs_url %>% read_html()

  pp_5v4 <- gs_html %>% html_nodes( "tr:nth-child(11) .border td:nth-child(1)" ) %>% html_text()
  pp_5v3 <- gs_html %>% html_nodes( "tr:nth-child(11) .border td:nth-child(2)" ) %>% html_text()
  pp_4v3 <- gs_html %>% html_nodes( "tr:nth-child(11) .border td:nth-child(3)" ) %>% html_text()

  # away then home
  teams <- gs_html %>% html_nodes( "#VPenaltySummary .border" ) %>% html_text() %>% str_to_title()
  away_team_short <- team_tbl %>% filter( name==teams[1] ) %>% select( name_short ) %>% unlist(use.names = F)
  home_team_short <- team_tbl %>% filter( name==teams[2] ) %>% select( name_short ) %>% unlist(use.names = F)

  away_5v4 <- pp_5v4[2] %>% str_split( "/") %>% unlist()
  home_5v4 <- pp_5v4[4] %>% str_split( "/") %>% unlist()
  away_5v3 <- pp_5v3[2] %>% str_split( "/") %>% unlist()
  home_5v3 <- pp_5v3[4] %>% str_split( "/") %>% unlist()
  away_4v3 <- pp_4v3[2] %>% str_split( "/") %>% unlist()
  home_4v3 <- pp_4v3[4] %>% str_split( "/") %>% unlist()

  away_pp_df <- bind_pp_df( away_team_short, away_5v4, away_5v3, away_4v3 )
  home_pp_df <- bind_pp_df( home_team_short, home_5v4, home_5v3, home_4v3 )
  game_pp_df <- bind_rows( away_pp_df, home_pp_df )

  game_pp_df <- game_pp_df %>% separate( conversion, c("goals", "ppo" ), sep= "-", convert=T )
  game_pp_df <- game_pp_df %>% mutate( time=time_mmss_to_decimal(time) )

  game_pp_df <- cbind( season=season, session_id=session_id, game_id4=game_id4, game_pp_df )

  game_pp_df
}
