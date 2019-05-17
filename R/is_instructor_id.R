#' Checks for a valid instructor_id (to be added)
#'
#' @param instructor_id string
#' @param strict logical
#'
#' @return
#' @export
#'
#' @examples
is_instructor_id <- function(instructor_id, strict = FALSE) {
  all(stringr::str_detect(instructor_id, "[:alpha:]"))
}
