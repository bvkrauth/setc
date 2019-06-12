#' Extract term ID from the course ID or term
#'
#' \code{get_term_id} extracts the term ID (e.g. 1171) from either a term
#'   (e.g., "Fa17") or a course ID (e.g., "ECON103D1Fa17").
#'
#' Either \code{term} or \code{course_id} can be provided, though
#' \code{course_id} should be named.
#'
#' @param term A vector of strings that are valid terms,
#'   e.g. "Sp17"
#'
#' @param course_id A vector of strings that are valid course IDs,
#'  e.g., "ECON381D1Sp17"
#'
#' @return A numeric vector of term IDs, e.g 1171.
#'
#' @export
#'
#' @examples
#' get_term_id(term="Fa17")
#' get_term_id(course_id = "ECON381D1Fa17")
get_term_id <- function(term = stringr::str_sub(course_id, -4),
                        course_id = NULL){
  is_term <- term %>%
    stringr::str_detect("^(Fa|Sp|Su)[0-9]{2}$")
  if (!all(is_term) | length(is_term) == 0) {
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
