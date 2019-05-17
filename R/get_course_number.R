#' Get course number
#'
#' @param report_name string
#' @param term string
#' @param course_id string
#'
#' @return
#' @export
#'
#' @examples
get_course_number <- function(report_name = NULL, term = NULL, course_id = get_course_id(report_name, term = term)){
  if (!is_course_id(course_id)) {
    stop("invalid course id: ", course_id)
  }
  course_number <- course_id %>%
    stringr::str_sub(5, 7) %>% # COURSE_ID takes the form ECON000D1Fa16, so extract the 000 part
    as.numeric() # convert it from string to number
  course_number
}
