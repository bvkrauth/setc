#' Extract a course ID from the SETC report file name.
#'
#' \code{get_course_id} infers a valid \code{course_id} from the file
#' name of a SETC report.
#'
#' A \code{course_id} takes the form (for example) "ECON105D1Fa18"
#' where the components are the program code (ECON), course number
#' (105), section number (D1) and term (Fa18).  SETC file names
#' take on a standard format, (for example
#' "BrianKrauth -ECON105D100-Fall2018.pdf") from which the course
#' ID (ECON105D1Fa18) and instructor ID (BrianKrauth) can be
#' extracted.
#'
#' The file name format varies somewhat across terms,
#' so it is a good idea to provide the \code{term} argument.
#'
#' @param report_name A string (vector) giving the filename(s) for
#' a SETC report
#'
#' @param term An optional string giving the term, e.g. "Fa16".  If
#'   not provided, the term will be inferred from \code{report_name}.
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
#' get_course_id("BrianKrauth -ECON105D100-Fall2018.pdf", term="Fa18")
get_course_id <- function(report_name, term=NULL){
  if (is.null(term)) {
    term <- get_term(report_name, from = "report_name")
  }
  clean_report_name <- report_name %>%
    stringr::str_replace_all("BUEC", "ECON") %>%
    stringr::str_replace_all("WD", "D")
  # The course_id will look like (for example) ECON302D2Sp17
  course_id <- stringr::str_c(stringr::str_sub(stringr::str_split(clean_report_name,
                                                                  "-",
                                                                  simplify = TRUE,
                                                                  n = 3)[, 2], 1, 9),
                              term)
  # The document name format was slightly different in Fall 2016
  if (any(term == "Fa16")) {
    clean_report_name <- report_name %>%
      stringr::str_replace_all(" \\(", "") %>%
      stringr::str_replace_all("BUEC", "ECON") %>%
      stringr::str_replace_all("WD", "D")
    # The course_id will look like (for example) ECON305D1Fa16
    fa16_course_id <- stringr::str_c("ECON",
                                stringr::str_sub( stringr::str_split(clean_report_name,
                                                                     "ECON ",
                                                                     simplify = TRUE,
                                                                     n = 2)[, 2], 1, 5),
                                term)
    course_id[term == "Fa16"] <- fa16_course_id[term == "Fa16"]
  }
  if (!is_course_id(course_id)) {
    stop("Unable to recover valid course id from report name: ", report_name)
  }
  # Return the course ID
  course_id
}
