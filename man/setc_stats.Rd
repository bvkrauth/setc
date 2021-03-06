% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setc_stats.R
\name{setc_stats}
\alias{setc_stats}
\alias{setc_n}
\alias{setc_mean}
\alias{setc_sd}
\alias{setc_median}
\title{Calculate SETC statistics from the frequency distribution.}
\usage{
setc_n(dat)

setc_mean(dat)

setc_sd(dat)

setc_median(dat)
}
\arguments{
\item{dat}{A tibble or data.frame containing the numeric
variables \code{score_1}, \code{score_2}, \code{score_3},
\code{score_4}, and \code{score_5}.  The variable
\code{score_1} is interpreted as the number of respondents
who chose response = 1, etc.}
}
\value{
A numeric vector corresponding to the requested
statistic.
}
\description{
These functions calculate the number of respondents, mean
response, median response, and standard deviation of responses
to a SETC question based on the frequency distribution of
responses.
}
