#' Get report text
#'
#' @param report_path string
#'
#' @return
#' @export
#'
#' @examples
get_report_text <- function(report_path) {
  if (!file.exists(report_path)){
    stop("File does not exist: ", report_path)
  }
  report_text <- report_path %>%
      pdftools::pdf_text() %>% # Read in PDF file as a single string
      stringr::str_split("\n") %>% # Convert to list of lines by splitting text at each newline character,
      unlist() %>% # Convert list of lines into vector of lines
      stringr::str_replace_all("\r", "") # Remove any left-over carriage returns
  invisible(report_text)
}
