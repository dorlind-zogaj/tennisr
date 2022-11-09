#' @importFrom
magrittr::`%>%`
magrittr::`%$%`

get_tournament_details <- function(year) {

  page <- glue::glue(
    "https://www.atptour.com/en/scores/results-archive?year={year}"
  ) %>%
    rvest::read_html()

  name <- page %>%
    rvest::html_elements("#scoresResultsArchive .tourney-title") %>%
    rvest::html_text() %>%
    trimws()

  location <- page %>%
    rvest::html_elements("#scoresResultsArchive .tourney-location") %>%
    rvest::html_text() %>%
    trimws()

  date <- page %>%
    rvest::html_elements(".tourney-dates") %>%
    rvest::html_text() %>%
    trimws() %>%
    lubridate::as_date()

  surface <- page %>%
    rvest::html_elements(".tourney-details:nth-child(5) .item-value") %>%
    rvest::html_text() %>%
    trimws()

  prize_money <- page %>%
    rvest::html_elements(".fin-commit") %>%
    rvest::html_text() %>%
    trimws()

  id <- page %>%
    rvest::html_elements("#scoresResultsArchive .tourney-title") %>%
    rvest::html_attr("href") %>%
    stringr::str_extract("[0-9]+")

  dplyr::tibble(date, name, location, surface, prize_money, id)

}

get_tournament_stats <- function(event_id, year) {

  url <- ifelse(
    year == stringr::str_sub(Sys.Date(), 1, 4),
    "https://www.atptour.com/en/scores/current/-",
    "https://www.atptour.com/en/scores/archive/-"
  )

  page <- paste(url, event_id, year, "results", sep = "/") %>% rvest::read_html()

  match_id <- page %>%
    rvest::html_elements(".day-table-score a") %>%
    rvest::html_attr("href") %>%
    stringr::word(6, 8, "/") %>%
    toupper()

  df <- tibble::tibble(match_id) %>%
    find_missing_id() %>%
    tidyr::separate(
      match_id, sep = "/", into = c("year", "event_id", "match_id")
    )

  df %$% usethis::ui_info("{nrow(df)} matches found")
  df %$% purrr::pwalk(list(year, event_id, match_id), get_match_stats)

}
