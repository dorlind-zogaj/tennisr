#' Title
#'
#' @importFrom magrittr %>%
#'
#' @param year
#' @param tour string indicating which tour; "atp" or "ch"
#'
#' @return
#' @export
#'
#' @examples
#'
get_tournament_ids <- function(year = 2022, tour = "atp") {

  page <- glue::glue(
    "https://www.atptour.com/en/scores/results-archive?",
    "year={year}&tournamentType={tour}"
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

  id <- page %>%
    rvest::html_elements("#scoresResultsArchive .tourney-title") %>%
    rvest::html_attr("href") %>%
    stringr::str_extract("[0-9]+")

  dplyr::tibble(date, name, location, surface, id)

}
