#' Extract term_id (e.g. 1171) from either a term or a course_id
#'
#' @param term numeric
#' @param course_id string
#'
#' @return
#' @export
#'
#' @examples
#' get_term_id(term="Fa17")
#' get_term_id(course_id = "ECON381D1Fa17")
get_term_id <- function(term = stringr::str_sub(course_id, -4),
                        course_id = NULL){
  is_term <- term %>%
    stringr::str_detect("^(Fa|Sp|Su)[0-9]{2}$")
  if (!all(is_term)) {
    stop("Invalid term: ", term)
  }
  season <- stringr::str_sub(term, 1, 2)
  yr <- stringr::str_sub(term, 3, 4)
  season_code <- season %>%
    stringr::str_replace("Sp", "1") %>%
    stringr::str_replace("Su", "4") %>%
    stringr::str_replace("Fa", "7")
  as.numeric(stringr::str_c("1", yr, season_code))
}
