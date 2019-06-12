#' Save SETC data in a convenient form.
#'
#' \code{setc_save} saves an SETC data object in both
#' CSV and Rdata formats.
#'
#' If \code{dat} is a data frame (or tibble), \code{setc_save}
#' will save \code{dat} in the files \code{[as]}.csv and
#' \code{[as]}.Rdata, where \code{[as]} is the value of the
#' \code{as = } argument.
#'
#' If \code{dat} is a list of data frames (or tibbles),
#' \code{setc_save} will save \code{dat} in the file
#' \code{[as]}.Rdata and each data frame in dat
#' as the file \code{[as]}_\code{[data frame name]}.csv.
#' .
#'
#' @param dat A tibble, data frame, or list of them.
#'
#' @param as A string indicating the type of data.
#'
#' @param output_folder A string indicating the folder into which the data should be saved
#'
#' @param save_rdata,save_csv An optional logical scalar.  If
#'   \code{FALSE}, a file of the given format will not be
#'   saved.
#'
#' @return Returns \code{dat} invisibly.
#'
#' @export
#'
setc_save <- function(dat,
                      as,
                      output_folder,
                      save_rdata = TRUE,
                      save_csv = TRUE){
  if (save_csv){
    if (is.data.frame(dat)) {
      dat %>%
        readr::write_excel_csv(stringr::str_c(output_folder, as, ".csv"))
    } else {
      datnames <- names(dat)
      for (i in 1:length(datnames)){
        dat[[datnames[i]]] %>%
          readr::write_excel_csv(stringr::str_c(output_folder,
                                                as,
                                                "_",
                                                datnames[i],
                                                ".csv"))
      }
    }
  }
  if (save_rdata){
    dat %>%
      save(file = stringr::str_c(output_folder, as, ".RData"))
  }
  invisible(dat)
}
