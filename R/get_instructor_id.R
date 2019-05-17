#' Extract instructor_id from SETC report filename
#'
#' @param report_name string
#' @param term string
#'
#' @return
#' @export
#'
#' @examples
get_instructor_id <- function(report_name, term=NULL){
  if (is.null(term)) {
    stop("term is required")
  }
  # The document name format was slightly different in Fall 2016
  if (term == "Fa16") {
    # Clean up the report_name
    clean_report_name <- report_name %>%
      stringr::str_replace_all(" \\(", "") %>%      # Take some inconvenient parentheses out
      stringr::str_replace_all("BUEC", "ECON") %>%  # Get rid of the BUEC designation
      stringr::str_replace_all("WD", "D")     # Get rid of the W designation    # Take some inconvenient parentheses out
    # Construct the (preliminary) instructor_id  It will take the form FirstnameLastname
    instructor_id <- stringr::str_split(clean_report_name, "ECON ", simplify = TRUE, n = 2)[, 1] %>%
      stringr::str_replace_all("[[:space:] .]", "")
  } else {
    # The document name follows a consistent format in all other terms
    # Clean up the report_name
    clean_report_name <- report_name %>%
      stringr::str_replace_all("BUEC", "ECON") %>%
      stringr::str_replace_all("WD", "D")
    # Construct the (preliminary) instructor_id  It will take the form FirstnameLastname
    instructor_id <- stringr::str_split(clean_report_name, " -", simplify = TRUE, n = 2)[, 1] %>%
      stringr::str_replace_all("[[:space:] .]", "")
    # TODO: to make this a true ID, we need to construct a master instructor list, and match to that
  }
  if (!is_instructor_id(instructor_id)) {
    stop("invalid instructor id: ", instructor_id)
  }
  # Return the instructor ID
  instructor_id
}
