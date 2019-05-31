#' Checks for a valid instructor ID
#'
#' \code{is_instructor_id} checks to see if its input
#' is a valid instructor IDs.  Instructor IDs should
#' be alphabetic strings.
#'
#' @param instructor_id A vector of strings
#'
#' @param strict A logical scalar (currently ignored).
#'
#' @return A logical vector, each element of which is TRUE
#'  if the corresponding element of \code{instructor_id} is
#'  a valid instructor ID.
#'
#' @export
#'
#' @examples
#' is_instructor_id("JoeSmith")
is_instructor_id <- function(instructor_id, strict = FALSE) {
  all(stringr::str_detect(instructor_id, "[:alpha:]"))
}
