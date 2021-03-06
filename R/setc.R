#' setc : a package for processing SETC reports
#'
#' This package is a set of tools for processing
#' course-level reports generated by Simon Fraser University's
#' Student Evaluation of Teaching and Courses (SETC) system.
#' The course-level SETC reports are PDF files containing
#' SETC results, formatting, graphics, etc.  The tools in
#' this package can be used to extract the key data from these
#' reports into data files that are usable for statistical
#' analysis.
#'
#' The main function for processing SETC reports is
#' \code{\link{get_setc_data}}. Given a list of SETC report files,
#' or the name of a folder containing those files, \code{\link{get_setc_data}}
#' will create a data object containing both quantitative survey
#' results and comments.
#'
#' SETC results created with \code{\link{get_setc_data}} can
#' then be analyzed in R or exported to a CSV file
#' using \code{\link{setc_save}}.
#'
#' The package also includes various support functions that
#' are either called by \code{\link{get_setc_data}} or support
#' the particular implementation used by the Economics
#' Department.
#'
#' @docType package
#'
#' @name setc
#'
NULL
