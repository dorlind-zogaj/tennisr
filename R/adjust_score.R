#' Title
#'
#' @importFrom magrittr %>%
#'
#' @param score A scoreline
#' @param outcome A character indicating whether the match was won (W) or lost (L)
#'
#' @return a string
#' @export
#'
#' @examples
#' adjust_score("6-2", "6-2", "L")
#'
adjust_score <- function(score, outcome) {

  score <- score %>% stringr::str_split(",") %>% unlist() %>% trimws()

  if (outcome == "L") score <- stringi::stri_reverse(score)

  toString(score)

}
