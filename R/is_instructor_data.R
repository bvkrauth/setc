#' Check for valid instructor data
#'
#' \code{is_instructor_data} checks its input to see if it is
#' a tibble (or data frame) in which each row contains a
#' unique value of \code{dat$instructor_id}.
#'
#' @param dat A tibble (data frame) that supposedly contains
#'   valid instructor data
#'
#' @return \code{TRUE} if \code{dat} is a valid instructor data set,
#'   \code{FALSE} if not.
#'
#' @export
#'
is_instructor_data <- function(dat){
  ans <- FALSE
  if (is.data.frame(dat)) {
    if (any(stringr::str_detect(names(dat), "^instructor_id$"))){
      ans <- (anyDuplicated(dat$instructor_id) == 0)
   }
  }
  ans
}
