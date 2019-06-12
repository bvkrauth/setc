#' Extract an instructor ID from the SETC report filename.
#'
#' \code{get_instructor_id} extracts  a valid instructor ID from the
#' SETC report filename.
#'
#' An \code{instructor_id} takes the form "FirstnameLastname"
#' without spaces, periods, or special characters.
#' SETC file names take on a standard format, (for example
#' "BrianKrauth -ECON105D100-Fall2018.pdf") from which the course
#' ID (ECON105D1Fa18) and instructor ID (BrianKrauth) can be
#' extracted.
#' The file name format varies somewhat across terms,
#'
#' @param report_name A string (vector) giving the filename(s) for
#' a SETC report
#'
#' @return A string (vector) giving the instructor ID that can be inferred
#'   from each \code{report_name}.
#'
#' @export
#'
#' @seealso \code{\link{is_instructor_id}}, \code{\link{get_course_id}}.
#'
#' @examples
#' get_instructor_id("BrianKrauth -ECON105D100-Fall2018.pdf")
get_instructor_id <- function(report_name){
  term <- get_term(report_name = report_name)
  # The document name format was slightly different in Fall 2016
  # The document name follows a consistent format in all other terms
  # Clean up the report_name
  clean_report_name <- report_name %>%
    stringr::str_replace_all("BUEC", "ECON") %>%
    stringr::str_replace_all("WD", "D")
  # Construct the (preliminary) instructor_id
  # It will take the form FirstnameLastname
  instructor_id <- stringr::str_split(clean_report_name, " -",
                                      simplify = TRUE,
                                      n = 2)[, 1] %>%
    stringr::str_replace_all("[[:space:] .]", "")
  if (any(term == "Fa16")) {
    # Clean up the report_name
    # Construct the (preliminary) instructor_id
    # It will take the form FirstnameLastname
    fa16_instructor_id <- stringr::str_split(report_name,
                                        "\\(",
                                        simplify = TRUE,
                                        n = 2)[, 1] %>%
      stringr::str_replace_all("[[:space:] .]", "")
    instructor_id[term == "Fa16"] <- fa16_instructor_id[term == "Fa16"]
  }
  if (!is_instructor_id(instructor_id)) {
    stop("invalid instructor id: ", instructor_id)
  }
  # Return the instructor ID
  instructor_id
}
