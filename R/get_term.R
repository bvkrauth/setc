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
    if (stringr::str_detect(input, "Fall_2016")) {
      term <- "Fa16"
    } else {
      if (stringr::str_detect(input, "Fall")) {
        term <- stringr::str_c("Fa",
                               stringr::str_sub(stringr::str_split(input,
                                                                   "Fall", simplify = TRUE, n = 2)[, 2], 3, 4))
      }
      if (stringr::str_detect(input, "Spring")) {
        term <- stringr::str_c("Sp",
                               stringr::str_sub(stringr::str_split(input, "Spring", simplify = TRUE, n = 2)[, 2], 3, 4))
      }
      if (stringr::str_detect(input, "Summer")) {
        term <- stringr::str_c("Su",
                               stringr::str_sub(stringr::str_split(input, "Summer", simplify = TRUE, n = 2)[, 2], 3, 4))
      }
    }
  }
  if (from == "term_folder") {
    term <- stringr::str_c(stringr::str_sub(input,8,9),stringr::str_sub(input,-2))
  }
  term
}
