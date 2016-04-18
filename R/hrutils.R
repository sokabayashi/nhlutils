
# Hockey-reference functions ----------------------------------------------


#' Get Season standings from hockey-reference.com
#'
#' @param season String for season, e.g., "20152016"
#' @param team_tbl Team tbl from nhl_db
#'
#' @return data frame of standings.  Note the gf/ga *exclude* shootout wins
#' @export
#'
get_season_standings <- function( season, team_tbl=NULL ) {
  season_end <- get_season_end( this_season )
  season_url     <- sprintf( "http://www.hockey-reference.com/leagues/NHL_%s.html", season_end )
  season_html    <- season_url %>% read_html()
  all_standings  <- season_html %>% html_node( "#teams" ) %>% html_table()
  # east_standings <- season_html %>% html_node( "#EAS_standings" ) %>% html_table()
  # west_standings <- season_html %>% html_node( "#WES_standings" ) %>% html_table()
  #
  # standing_names <- c( "team_long", "gp", "w", "l", "otl", "pts", "pts_pct", "gf", "ga", "srs", "sos", "pts_pct_old" )
  # names( east_standings ) <- standing_names
  # names( west_standings ) <- standing_names
  # season_standings <- bind_rows( east_standings, west_standings ) %>% filter( !str_detect( team_long, "Division" ) )

  standing_names <- c( "rank", "team_long", "avg_age", "gp", "w", "l", "otl", "pts", "pts_pct",
                        "gf", "ga", "srs", "sos", "tg_gm", "gf_pp", "ppo", "pp_pct", "ga_sh", "ppoa", "pk_pct", "sh", "sha", "sog",
                         "sh_pct", "sv", "sv_pct", "pdo" )
  names( all_standings ) <- standing_names

  season_standings <- all_standings %>%
    filter( team_long != "League Average" ) %>%
    mutate(
      made_playoffs = str_detect( team_long, "\\*" ),
      team_long     = gsub( "\\*", "", team_long )
  )

  if( !is.null( team_tbl ) ) {
    season_standings <- season_standings %>%
      left_join( team_tbl %>% select(team_long=name, conference, division, team_short=name_short), by= "team_long" ) %>%
      arrange( desc(pts) )
  }

  season_standings
}
