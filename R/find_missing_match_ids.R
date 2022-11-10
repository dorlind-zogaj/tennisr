#' Title
#'
#' @param a string
#'
#' @return a data frame with missing match-ids in the database
#' @export
#'
#' @examples
#' find_missing_match_ids("2022/352/MS001")
#'
find_missing_match_ids <- function(match_id) {

  tibble::tibble(match_id) %>%
    dplyr::filter(
      !match_id %in% data.table::fread("data/match-data.csv")$match_id
    )

}
