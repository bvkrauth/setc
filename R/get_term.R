#' Extract term  from the SETC report filename or term folder name
#'
#' \code{get_term} extracts the term  from the SETC report filename or term folder name
#'
#' @param report_name,term_folder A string or vector
#'   of strings each of which identifies either the
#'   SETC report filename or the term folder.
#'   Either \code{report_name} or \code{term_folder}
#'   must be provided, but not both.
#'
#' @return A string or vector of strings identifying the term, e.g. "Fa18"
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
