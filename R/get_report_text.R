#' Read in report text from a SETC report
#'
#' \code{get_report_text} reads in the text of an SETC report from
#' the PDF file.
#'
#' @param report_path A string giving the full path to
#'   a PDF file containing a course-lvel SETC report
#'
#' @return A vector of strings, the text of the report.
#'
#' @export
#'
get_report_text <- function(report_path) {
  if (!file.exists(report_path)){
    stop("File does not exist: ", report_path)
  }
  report_text <- report_path %>%
      pdftools::pdf_text() %>% # Read in PDF file as a single string
      stringr::str_split("\n") %>% # Split text at each newline character,
      unlist() %>% # Convert list of lines into vector of lines
      stringr::str_replace_all("\r", "") # Remove any left-over carriage returns
  invisible(report_text)
}
