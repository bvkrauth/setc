#' Read in the master instructor list as a tibble
#'
#' @param instructor_file filename
#' @param output_file filename
#'
#' @return
#' @export
#'
#' @examples
get_instructors <- function(instructor_file, output_file = NULL){
  instructor <- readr::read_csv(instructor_file)
  if (!is.null(instructor_file)) {
    instructor %>%
      readr::write_excel_csv(instructor_file)
  }
  instructor
}
