#' Get course number from a SETC report name or course ID
#'
#' \code{get_course_number} extracts the course number from
#' a SETC report name or course ID.
#'
#' If both \code{report_name} and \code{course_id} are provided,
#' \code{course_id} takes precedence.
#'
#' @param report_name An optional string (vector) giving the
#'   file name of a SETC report
#'
#' @param term An optional string giving the term, e.g. "Fa16"
#'
#' @param course_id An optional vector of course IDs
#'
#' @return A vector of course numbers extracted from the input.
#'
#' @export
#'
#' @examples
#' get_course_number(course_id = "ECON105D1Fa17")
#' get_course_number(report_name = "BrianKrauth -ECON105D100-Fall2018.pdf")
get_course_number <- function(report_name = NULL,
                              term = NULL,
                              course_id = get_course_id(report_name,
                                                        term = term)){
  if (!is_course_id(course_id)) {
    stop("invalid course id: ", course_id)
  }
  course_number <- course_id %>%
    stringr::str_sub(5, 7) %>% # Extract 000 from ECON000D1Fa16
    as.numeric() # convert it from string to number
  course_number
}
