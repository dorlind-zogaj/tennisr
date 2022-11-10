#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
save_match_data <- function(df) {

  data.table::fread("data/match-data.csv") %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    dplyr::filter(match_id != df$match_id) %>%
    dplyr::bind_rows(
      df %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
    ) %>%
    dplyr::arrange(dplyr::desc(match_date), match_id) %>%
    data.table::fwrite("data/match-data.csv")

  df %$% usethis::ui_done("Match saved: {winner_name} - {loser_name} {match_date}")

}
