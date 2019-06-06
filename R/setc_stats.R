#' Calculate SETC statistics from frequency distribution.
#'
#' These functions calculate the number of respondents, mean
#' response, median response, and standard deviation of responses
#' to a SETC question based on the frequency distribution of
#' responses.
#'
#' @param dat A tibble or data.frame containing the numeric
#' variables \code{score_1}, \code{score_2}, \code{score_3},
#' \code{score_4}, and \code{score_5}.  The variable
#' \code{score_1} is interpreted as the number of respondents
#' who chose response = 1, etc.
#'
#' @return A numeric vector corresponding to the requested
#' statistic.
#'
#'
#' @name setc_stats
NULL

#' @rdname setc_stats
#' @export
setc_n <- function(dat){
  ans <- dat$score_1 + dat$score_2 + dat$score_3 + dat$score_4 + dat$score_5
  ans
}

#' @rdname setc_stats
#' @export
setc_mean <- function(dat){
  ans <- (1 * dat$score_1 +
            2 * dat$score_2 +
            3 * dat$score_3 +
            4 * dat$score_4 +
            5 * dat$score_5) / setc_n(dat)
  ans
}

#' @rdname setc_stats
#' @export
setc_sd <- function(dat){
  xbar <- setc_mean(dat)
  n <- setc_n(dat)
  tmp_var <- (dat$score_1 * (1 - xbar) ^ 2 +
                dat$score_2 * (2 - xbar) ^ 2 +
                dat$score_3 * (3 - xbar) ^ 2 +
                dat$score_4 * (4 - xbar) ^ 2 +
                dat$score_5 * (5 - xbar) ^ 2) / (n - 1)
  sqrt(tmp_var)
}

#' @rdname setc_stats
#' @export
setc_median <- function(dat){
  tmp_median <- NA
  halfway <- setc_n(dat) / 2
  tmp_median <- 1 +
    (dat$score_1 < halfway) +
    (dat$score_1 + dat$score_2 < halfway) +
    (dat$score_1 + dat$score_2 + dat$score_3 < halfway) +
    (dat$score_1 + dat$score_2 + dat$score_3 + dat$score_4 < halfway)
  tmp_median[halfway == 0] <- NA
  tmp_median
}
