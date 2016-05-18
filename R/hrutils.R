
# Hockey-reference functions ----------------------------------------------


#' Scrape season standings from hockey-reference.com
#'
#' @param season_str String for season, e.g., "20152016"
#' @param team_tbl Team tbl from nhl_db
#' @param use_current_names Flag for whether or not to use updated team_short everywhere, e.g., WPG in place of ATL.
#' only releveant when team_tbl is provided (won't have team_short otherwise).  Default is TRUE.
#'
#' @return data frame of standings.  Note the gf/ga *exclude* shootout wins
#' @export
#'
scrape_season_standings <- function( season_str, team_tbl=NULL, use_current_names=TRUE ) {
  season_end <- get_season_end( season_str )
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
      team_long     = gsub( "\\*", "", team_long ),
      team_long     = gsub( "Mighty Ducks of Anaheim", "Anaheim Ducks", team_long ),
      g_net         = gf - ga,
      pp_diff       = ppo - ppoa,
      ppg_pct       = round( gf_pp / gf*100, 2 )
  )

  if( !is.null( team_tbl ) ) {
    season_standings <- season_standings %>%
      left_join( team_tbl %>% select(team_long=name, conference, division, team_short=name_short), by= "team_long" ) %>%
      arrange( desc(pts) )

    if( use_current_names ) {
      season_standings <- season_standings %>%
        mutate(
          team_short    = gsub( "PHX", "ARI", team_short ),
          team_short    = gsub( "ATL", "WPG", team_short ),
          team_season   = paste0( team_short, " ", season_end )
        )
    }
  }

  season_standings
}
