#' Title
#'
#' @param input A string.
#' @param from A string describing the nature of the input. Currently
#' supported values include "report_name"
#'
#'
#' @return A string identifying a term, e.g. "Fa18"
#' @export
#'
#' @examples
get_term <- function(input, from="report_name"){
  term <- NULL
  if (from == "report_name") {
    season <- stringr::str_sub(stringr::str_extract(input, "(Fall|Spring|Summer)"), 1, 2)
    yr <- stringr::str_sub(stringr::str_split(input,
                                              "(Fall|Spring|Summer)",
                                              simplify = TRUE,
                                              n = 2)[, 2], 3, 4)
    term <- stringr::str_c(season, yr)
    term[stringr::str_detect(input, "Fall_2016")] <- "Fa16"
  }
  if (from == "term_folder") {
    term <- stringr::str_c(stringr::str_sub(input, 8, 9),
                           stringr::str_sub(input, -2))
  }
  term
}
