#' @importFrom
magrittr::`%>%`
magrittr::`%$%`

read_json <- function(url) {
  
  user_agent <- paste(
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15", 
    "(KHTML, like Gecko) Version/16.0 Safari/605.1.15"
  )
  
  url %>%
    httr::GET(httr::user_agent(user_agent)) %>%
    httr::content("text", encoding = "UTF-8") %>%
    jsonlite::fromJSON()
  
}

adjust_score <- function(score, outcome) {
  
  score <- score %>% stringr::str_split(",") %>% unlist() %>% trimws()
  
  if (outcome == "L") score <- stringi::stri_reverse(score)
  
  toString(score)
  
}

find_missing_id <- function(df) {
  
  df %>%
    dplyr::filter(
      !match_id %in% data.table::fread("data/match-stats.csv")$match_id
    )
  
}

save_match_stats <- function(df) {
  
 data.table::fread("data/match-stats.csv") %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>% 
    dplyr::filter(match_id != df$match_id) %>% 
    dplyr::bind_rows(
      df %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
    ) %>%
    dplyr::arrange(dplyr::desc(match_date)) %>% 
    data.table::fwrite("data/match-stats.csv")
  
  df %$% usethis::ui_done("Match saved: {winner_name} - {loser_name} {match_date}")
  
}

load_database <- function() {
  
  data.table::fread("data/match-stats.csv") %>% tibble::view()
  
}
