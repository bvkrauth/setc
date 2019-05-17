#' Extract course_id from SETC report filename
#'
#' @param report_name string
#' @param term string
#'
#' @return string
#' @importFrom magrittr "%>%"
#' @export
#'
#' @examples
#' get_course_id("BrianKrauth -ECON105D100-Fall2018(SUP)_407d2700-5049-48d3-9ed5-465e7627b5d0en-US.pdf", term="Fa18")
get_course_id <- function(report_name, term=NULL){
  if (is.null(term)) {
    stop("term is required")
  }
  # The document name format was slightly different in Fall 2016
  if (term == "Fa16") {
    clean_report_name <- report_name %>%
      stringr::str_replace_all(" \\(", "") %>%      # Drop inconvenient parentheses
      stringr::str_replace_all("BUEC", "ECON") %>%  # Drop BUEC
      stringr::str_replace_all("WD", "D")     # Drop the W designation
    # The course_id will look like (for example) ECON305D1Fa16
    course_id <- stringr::str_c("ECON",
                                stringr::str_sub( stringr::str_split(clean_report_name, "ECON ",
                      simplify = TRUE, n = 2)[, 2], 1, 5), term)
  } else {
    clean_report_name <- report_name %>%
      stringr::str_replace_all("BUEC", "ECON") %>%
      stringr::str_replace_all("WD", "D")
    # The course_id will look like (for example) ECON302D2Sp17
    course_id <- stringr::str_c(stringr::str_sub(stringr::str_split(clean_report_name, "-", simplify = TRUE, n = 3)[, 2], 1, 9),
                       term)
  }
  if (!is_course_id(course_id)) {
    stop("invalid course id: ", course_id)
  }
  # TODO: Check to make sure it's a valid COURSE_ID and fail elegantly
  # Return the course ID
  course_id
}
