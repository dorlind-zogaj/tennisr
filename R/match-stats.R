#' @importFrom
magrittr::`%>%`
magrittr::`%$%`

get_match_stats <- function(year, event_id, match_id) {
  
  Sys.sleep(4)
  
  json <- glue::glue(
    "atptour.com/-/ajax/MatchStats/True/{year}/{event_id}/{match_id}/"
  ) %>% 
    read_json()
  
  data <- json$Match %>%
    unlist() %>% 
    tibble::enframe() %>%
    tidyr::pivot_wider(names_from = name, values_from = value) %>%
    janitor::clean_names()
    
  player_team1_sets_set_score3 <- NA
  player_team1_sets_set_score4 <- NA
  player_team2_sets_set_score3 <- NA
  player_team2_sets_set_score4 <- NA
    
  details <- data %>%
    dplyr::transmute(
      year, event_id, match_id,
      match_id = paste(year, event_id, match_id, sep = "/"),
      match_date = stringr::str_sub(json$Tournament$StartDate, 1, 10) %>% as.character(),
      match_surface = json$Tournament$Court,
      match_round = round_short_name,
      tournament_name = json$Tournament$TournamentName,
      tournament_city = json$Tournament$TournamentCity,
      tournament_draw = json$Tournament$Singles,
      home_name = paste(
        player_team1_player_first_name_full, player_team1_player_last_name
      ),
      away_name = paste(
        player_team2_player_first_name_full, player_team2_player_last_name
      ),
      match_outcome = dplyr::case_when(
        player_team1_player_id == winner ~ "W",
        player_team2_player_id == winner ~ "L"
      ),
      match_score = paste0(
        player_team1_sets_set_score2, "-", player_team2_sets_set_score2, ", ",
        player_team1_sets_set_score3, "-", player_team2_sets_set_score3, ", ",
        player_team1_sets_set_score4, "-", player_team2_sets_set_score4
      ) %>% 
        stringr::str_remove_all(", NA-NA") %>% 
        adjust_score(match_outcome),
      match_time
    )
    
  ifelse(
    details$match_outcome == "W",
    winner <- c("winner", "loser"),
    winner <- c("loser", "winner")
  )
    
  details <- details %>% 
    dplyr::rename_with(~stringr::str_replace(., "home", winner[1])) %>% 
    dplyr::rename_with(~stringr::str_replace(., "away", winner[2])) %>% 
    dplyr::select(-match_outcome)
    
  stats <- json$Match %$%
    dplyr::bind_rows(
      PlayerTeam1$Sets$Stats %>%
        dplyr::select(ServiceStats, ReturnStats, PointStats) %>% 
        dplyr::mutate(
          set_number = json$Match$PlayerTeam1$Sets$SetNumber,
          side = winner[1]
        ),
      PlayerTeam2$Sets$Stats %>%
        dplyr::select(ServiceStats, ReturnStats, PointStats) %>%
        dplyr::mutate(
          set_number = json$Match$PlayerTeam2$Sets$SetNumber,
          side = winner[2]
        ),
    ) %>%
    jsonlite::flatten() %>%
    janitor::clean_names() %>%
    dplyr::transmute(
      side,
      set_number = recode(
        set_number,
        "0" = "all", "1" = "1st", "2" = "2nd", "3" = "3rd"
      ),
      aces = service_stats_aces_number,
      double_faults = service_stats_double_faults_number,
      serve_rating = service_stats_serve_rating_number,
      first_serve = paste0(
        service_stats_first_serve_dividend, "/", service_stats_first_serve_divisor
      ),
      first_serve_points = paste0(
        service_stats_first_serve_points_won_dividend, "/",
        service_stats_first_serve_points_won_divisor
      ),
      second_serve_points = paste0(
        service_stats_second_serve_points_won_dividend, "/",
        service_stats_second_serve_points_won_divisor
      ),
      break_points_saved = paste0(
        service_stats_break_points_saved_dividend, "/",
        service_stats_break_points_saved_divisor
      ),
      service_games_played = service_stats_service_games_played_number,
      return_rating = return_stats_return_rating_number,
      first_serve_return_points = paste0(
        return_stats_first_serve_return_points_won_dividend, "/",
        return_stats_first_serve_return_points_won_divisor
      ),
      second_serve_return_points = paste0(
        return_stats_second_serve_return_points_won_dividend, "/",
        return_stats_second_serve_return_points_won_divisor
      ),
      break_points_converted = paste0(
        return_stats_break_points_converted_dividend, "/",
        return_stats_break_points_converted_divisor
      ),
      total_service_points_won = paste0(
        point_stats_total_service_points_won_dividend, "/",
        point_stats_total_service_points_won_divisor
      ),
      total_return_points_won = paste0(
        point_stats_total_return_points_won_dividend, "/",
        point_stats_total_return_points_won_divisor
      ),
      total_points_won = paste0(
        point_stats_total_points_won_dividend, "/",
        point_stats_total_points_won_divisor
      )
    ) %>%
    tidyr::pivot_wider(
      names_from = c(set_number, side),
      values_from = c(-side, -set_number)
    ) %>% 
    dplyr::rename_with(~paste0("winner_", .), dplyr::ends_with("winner")) %>% 
    dplyr::rename_with(~paste0("loser_", .), dplyr::ends_with("loser")) %>%
    dplyr::rename_with(~stringr::str_remove(., "_[^_]+$"))
      
  dplyr::bind_cols(details, stats) %>% save_match_stats()

}