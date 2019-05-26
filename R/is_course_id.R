#' Check whether input is a valid course_id
#'
#' @param course_id string
#' @param strict logical
#'
#' @return logical
#' @export
#'
#' @examples
#' is_course_id("ECON381D1Fa19")
is_course_id <- function(course_id, strict=FALSE){
  if (strict){
    all(stringr::str_detect(course_id,
                        "^[:upper:]{2,4}[0-9]{3}[CDEG][0-9](Fa|Sp|Su)[0-9]{2}$"))
  } else {
    all(stringr::str_detect(course_id,
                      "^(?i)[:alpha:]{2,4}[0-9]{3}[:alpha:][0-9](Fa|Sp|Su)[0-9]{2}(?-i)$"))
  }
}
