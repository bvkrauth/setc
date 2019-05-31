#' Save SETC data in a convenient form.
#'
#' @param dat A tibble or data frame
#'
#' @param as A string indicating the type of data.
#'
#' @param output_folder The folder into which the data should be saved
#'
#' @return Returns \code{dat} invisibly.
#'
#' @export
#'
setc_save <- function(dat, as, output_folder){
  dat %>%
    readr::write_excel_csv(stringr::str_c(output_folder, as, ".csv")) %>%
    save(file = stringr::str_c(output_folder, as, ".RData"))
  invisible(dat)
}
