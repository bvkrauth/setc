#' Check for valid question data
#'
#' \code{is_question_data} checks its input to see if it is
#' a tibble (or data frame) in which each row contains a
#' unique value of \code{dat$question_id}.
#'
#' @param dat A tibble (data frame) that supposedly contains
#'   valid question data
#'
#' @return \code{TRUE} if \code{dat} is a valid question data set,
#'   \code{FALSE} if not.
#'
#' @export
#'
is_question_data <- function(dat){
  ans <- FALSE
  if (is.data.frame(dat)) {
    if (any(stringr::str_detect(names(dat), "^question_id$")) &
        any(stringr::str_detect(names(dat), "^question_text$")) &
        any(stringr::str_detect(names(dat), "^table_type$"))){
      ans <- (anyDuplicated(dat$question_id) == 0)
    }
  }
  ans
}
