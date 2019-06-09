#' Title
#'
#' @param report_name,term_folder A vector of strings
#'
#' @return A string identifying a term, e.g. "Fa18"
#'
#' @export
#'
#' @examples
#' get_term(term_folder = "1171 - Spring 2017")
get_term <- function(report_name = NULL, term_folder = NULL){
  term <- NULL
  if (!is.null(report_name)){
    season <- stringr::str_sub(stringr::str_extract(report_name,
                                                    "(Fall|Spring|Summer)"),
                               1, 2)
    yr <- stringr::str_sub(stringr::str_split(report_name,
                                              "(Fall|Spring|Summer)",
                                              simplify = TRUE,
                                              n = 2)[, 2], 3, 4)
    term <- stringr::str_c(season, yr)
    term[stringr::str_detect(report_name, "Fall_2016")] <- "Fa16"
  }
  if (!is.null(term_folder)){
    term <- stringr::str_c(stringr::str_sub(term_folder, 8, 9),
                           stringr::str_sub(term_folder, -2))
  }
  term
}
