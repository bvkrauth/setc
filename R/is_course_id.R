#' Check whether input is a valid course ID
#'
#' \code{is_course_id} checks whether its input is a valid course ID.
#'
#' @param course_id A vector of strings
#'
#' @param strict Optional logical scalar.  If \code{strict = TRUE},
#'   the function is case-sensitive and only allows known
#'   course section types (currently C,D,E and G).  If
#'   \code{strict = FALSE} (the default), the function is
#'   case sensitive and allows any course section type.
#'
#' @return A logical vector equalling \code{TRUE} if the
#'   corresponding element in \code{course_id} is a valid
#'   course ID, and \code{FALSE} otherwise.
#'
#' @export
#'
#' @examples
#' is_course_id("ECON381D1Fa19")
#' is_course_id("ECON381D1Fa19", strict = TRUE)
#' is_course_id("NOT A VALID COURSE ID")
is_course_id <- function(course_id, strict=FALSE){
  if (strict){
    stringr::str_detect(
      course_id,
      "^[:upper:]{2,4}[0-9]{3}[CDEG][0-9](Fa|Sp|Su)[0-9]{2}$")
  } else {
    stringr::str_detect(
      course_id,
      "^(?i)[:alpha:]{2,4}[0-9]{3}[:alpha:][0-9](Fa|Sp|Su)[0-9]{2}(?-i)$")
  }
}
