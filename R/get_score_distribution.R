#' Find and extract score distribution from SETC report
#'
#' @param report_text string
#' @param question tibble
#'
#' @return
#' @export
#'
#' @examples
get_score_distribution <- function(report_text, question){
  question_appearance <- report_text %>%
    stringr::str_which(question$question_text[1])
  score_distribution <- rep(NA, 5)
  if (length(question_appearance) > 1) {
    # Get the text starting with the second appearance of the question
#    tmptext <- report_text[question_appearance[2]:length(report_text)]
    # The table is in the five lines after the row with the heading "Options"
    tmp <- stringr::str_which(report_text, "Options")
    tablestart <- dplyr::first(tmp[tmp >= question_appearance[2]]) + 1
#    tablestart <- stringr::str_which(tmptext, "Options")[1] + 1
    # Convert the table text into a matrix
#    score <- tmptext[tablestart:(tablestart + 4)] %>% # Get the next 5 lines of text
    score <- report_text[tablestart:(tablestart + 4)] %>% # Get the next 5 lines of text
      stringr::str_split("\\s{2,}") %>% # split eacn line up by white space
      unlist() %>% # convert from list to vector
      matrix(nrow = 5, byrow = T) # reshape into matrix
    # TODO: There is a fragile piece of code here.  score may or may not have
    #       a blank initial column, depending on the term, so we select columns
    #       from the right.  Not a robust solution, need to do this better.
    nc <- ncol(score)
    # Extract the counts from the matrix
    score_distribution <- c(as.numeric(score[score[, nc - 2] == "1", nc - 1]),
                            as.numeric(score[score[, nc - 2] == "2", nc - 1]),
                            as.numeric(score[score[, nc - 2] == "3", nc - 1]),
                            as.numeric(score[score[, nc - 2] == "4", nc - 1]),
                            as.numeric(score[score[, nc - 2] == "5", nc - 1]))
  }
  score_distribution
}
