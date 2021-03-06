#' Pool multiple terms of SETC data into a single data set
#'
#' \code{pool_setc_data} combines multiple terms of SETC
#' data into a single pooled data set.
#'
#' This function is designed to be called after
#' \code{\link{get_setc_data}}, which extracts SETC
#' data from the PDF files, and \code{\link{setc_save}},
#' which saves the data to CSV files. The term-by-term
#' CSV files should be named according to the convention
#' \code{[term]_[data_structure].csv}, for example
#' \code{Fa18_comments.csv}.
#'
#' Once the term-by-term CSV files have been generated,
#' which saves the data to CSV files. Once the term-by-term
#' CSV files have been generated,
#' use this function to pool them into a single data file.
#'
#' @param data_structure A string describing an SETC data structure
#'        Typical values are "course" or "comments".
#'
#' @param data_folder A string, the folder in which the data can be found.
#'
#' @return A tibble (data frame) constructed by \code{rbind}ing
#' every CSV file in \code{data_folder} with a filename of the
#' form \code{[term]_[data_structure].csv} where \code{[term]}
#' is a valid term (e.g. "Fa16") and \code{[data_structure]}
#' is the value of the \code{data_structure} argument
#'
#' @export
#' @seealso \code{\link{get_setc_data}}, \code{\link{setc_save}}
#'
#' @examples
#' \dontrun{
#' pool_setc_data("course", "../data/")
#' }
pool_setc_data <- function(data_structure,
                           data_folder){
  file_names <- dir(
    data_folder,
    pattern = stringr::str_c("(Fa|Sp|Su)[0-9]{2}_",
                             data_structure,
                             ".csv"),
    full.names = TRUE)
  dat <- NULL
  if (length(file_names) > 0){
    for (i in 1:length(file_names)) {
      current_term <- readr::read_csv(file_names[i])
      dat <- rbind(dat, current_term)
    }
  } else {
    stop("No CSV ", data_structure, " files found in folder ", data_folder)
  }
  invisible(dat)
}
