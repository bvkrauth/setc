#' Extract a course ID from the SETC report filename.
#'
#' \code{get_course_id} infers a valid course ID from the file
#' name of a SETC report.
#'
#' A course ID takes the form (for example) "ECON105D1Fa18"
#' where the components are the program code (ECON), course number
#' (105), section number (D1) and term (Fa18).
#'
#' SETC filenames take on a standard format, (for example
#' "BrianKrauth -ECON105D100-Fall2018.pdf") from which the course
#' ID (ECON105D1Fa18) and instructor ID (BrianKrauth) can be
#' extracted.
#'
#' @param report_name A string (vector) giving the filename(s) for
#' a SETC report
#'
#' @return A string (vector) giving the course ID that can be inferred
#'   from each \code{report_name}.
#'
#' @importFrom magrittr "%>%"
#'
#' @seealso \code{\link{is_course_id}}, \code{\link{get_instructor_id}}.
#'
#' @export
#'
#' @examples
#' get_course_id("BrianKrauth -ECON105D100-Fall2018.pdf")
get_course_id <- function(report_name){
  term <- get_term(report_name = report_name)
  clean_report_name <- report_name %>%
    stringr::str_replace_all("BUEC", "ECON") %>%
    stringr::str_replace_all("WD", "D")
  # The course_id will look like (for example) ECON302D2Sp17
  course_id <- stringr::str_c(
    stringr::str_sub(stringr::str_split(clean_report_name,
                                        "-",
                                        simplify = TRUE,
                                        n = 3)[, 2], 1, -3),
    term)
  # The document name format was slightly different in Fall 2016
  if (any(term == "Fa16")) {
    clean_report_name <- report_name %>%
      stringr::str_replace_all("BUEC", "ECON") %>%
      stringr::str_replace_all("W \\(", " \\(")
    # The course_id will look like (for example) ECON305D1Fa16
    course_code <- stringr::str_split(clean_report_name,
                               "\\(",
                               simplify = TRUE,
                               n = 3)[, 2]
    section_code <- stringr::str_sub(
      stringr::str_split(clean_report_name,
                         "\\(",
                         simplify = TRUE,
                         n = 3)[, 3],
      1, 2)
    fa16_course_id <- stringr::str_c(course_code, section_code, term) %>%
        stringr::str_replace_all(" ", "")
    course_id[term == "Fa16"] <- fa16_course_id[term == "Fa16"]
  }
  if (!all(is_course_id(course_id))) {
    stop("Unable to recover valid course id from report name: ", report_name)
  }
  # Return the course ID
  course_id
}
