#' Title
#'
#' @importFrom magrittr %$%
#'
#' @param event_id
#' @param year
#'
#' @return
#' @export
#'
#' @examples
#'
get_tournament_data <- function(event_id, year = 2022) {

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

  df <- match_id %>%
    find_missing_match_ids() %>%
    tidyr::separate(
      match_id, sep = "/", into = c("year", "event_id", "match_id")
    )

  df %$% usethis::ui_info("{nrow(df)} matches found")
  df %$% purrr::pwalk(., purrr::compose(save_match_data, get_match_data))

}
