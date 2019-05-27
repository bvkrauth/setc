#' Finds response rate information in SETC report
#'
#' @param report_text SETC report text
#' @param row_name string
#'
#' @return
#' @export
#'
#' @examples
get_response_rate <- function(report_text, row_name = "Responded"){
  report_text[stringr::str_detect(report_text,
                                  row_name)] %>% # Find the line in the text
    stringr::str_replace_all(row_name, "") %>% # Get rid of the word "Responded"
    stringr::str_replace_all(" ", "") %>% # Drop white space
    as.numeric() %>% # Convert to a number
    dplyr::first() # Take first matching line
}
# GET_RESPONSE_RATE finds SETC response rate information from the PDF text
# if ROW_NAME="Responded", it finds the number who responded to the survey
# if ROW_NAME="Invited", it finds the number who were invited to the survey
