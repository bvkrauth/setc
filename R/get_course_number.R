#' Extract a course number from the course ID or SETC report filename
#'
#' \code{get_course_number} extracts a course number from
#' the course ID or SETC report filename.
#'
#' If both \code{report_name} and \code{course_id} are provided,
#' \code{course_id} takes precedence.
#'
#' @param report_name An optional string (vector) giving the
#'   file name of a SETC report
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
                              course_id = get_course_id(report_name)){
  if (!all(is_course_id(course_id))) {
    stop("Invalid course ID: ", course_id)
  }
  course_number <- course_id %>%
    stringr::str_sub(-9, -7) %>% # Extract 000 from ECON000D1Fa16
    as.numeric() # convert it from string to number
  course_number
}
