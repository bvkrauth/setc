#' Title
#'
#' @param question_file a file name
#' @param output_file a file name
#'
#' @return
#' @export
#'
#' @examples
get_questions <- function(question_file, output_file = NULL){
  question <- readr::read_csv(question_file)
  question$score_legend <- stringr::str_c(question$label_1, question$label_2, question$label_3, question$label_4, question$label_5, sep = " | ")
  if (!is.null(output_file)){
    question %>%
      readr::write_excel_csv(output_file)
  }
  question
}
