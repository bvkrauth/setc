#' Finds response rate information in SETC report
#'
#' \code{get_response_rate} finds SETC response rate information from
#' the PDF text.
#' If row_name = "Responded", it finds the number who responded to the survey
#' if row_name =  "Invited", it finds the number who were invited to the survey
#'
#' @param report_text The text of an SETC report, normally
#'   obtained with \code{\link{get_report_text}}.
#'
#' @param row_name A string, either "Responded" or "Invited"
#'
#' @return A number, either the number of students invited
#'   to the SETC survey, or the number who responded.
#'
#' @export
#'
get_response_rate <- function(report_text, row_name = "Responded"){
  report_text[stringr::str_detect(report_text,
                                  row_name)] %>% # Find the line in the text
    stringr::str_replace_all(row_name, "") %>% # Get rid of the word "Responded"
    stringr::str_replace_all(" ", "") %>% # Drop white space
    as.numeric() %>% # Convert to a number
    dplyr::first() # Take first matching line
}
