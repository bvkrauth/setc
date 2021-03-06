#' Spread question_and_course level data out to course level data
#'
#' \code{setc_spread} spreads question_and_course=level data out to course-level data
#'
#' This function is used by \code{\link{get_setc_data}}.
#'
#' @param dat A tibble with course-and-question level SETC data
#' @param stat A string indicating the statistic.  Valid values are
#' "n", "mean", "median", and "sd"
#'
#' @return A tibble
#'
#' @export
#'
setc_spread <- function(dat, stat="mean"){
  tmpdat <- dat %>%
    dplyr::select("course_id",
                  "question_id",
                  stringr::str_c("score", stat, sep = "_")) %>%
    tidyr::spread(key = "question_id",
                  value = stringr::str_c("score", stat, sep = "_"))
  names(tmpdat)[-1] <- stringr::str_c(names(tmpdat)[-1], stat, sep = "_")
  tmpdat
}
