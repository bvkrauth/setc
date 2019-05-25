#' Combine a particular data set across multiple terms
#'
#' @param data_structure string
#' @param data_folder string
#'
#' @return
#' @export
#'
#' @examples
combine_across_terms <- function(data_structure, data_folder){
  file_names <- dir(data_folder,
                    pattern = stringr::str_c("(Fa|Sp|Su)[0-9]{2}_", data_structure, ".csv"),
                    full.names = TRUE)
  dat <- NULL
  if (length(file_names) > 0){
    for (i in 1:length(file_names)) {
      current_term <- readr::read_csv(file_names[i])
      dat <- rbind(dat, current_term)
    }
  }
  invisible(dat)
}
