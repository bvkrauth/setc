#' Read in additional instructor information as a tibble
#'
#' \code{get_instructors} reads additional instructor information
#' from \code{instructor_file}, if it is provided.
#'
#' If provided, \code{instructor_file} should be a CSV file in which the top
#' row gives variable names, and each additional row corresponds
#' to an instructor. Variables should include \code{instructor_id}
#' so that this data can be linked to other SETC data. To avoid name
#' conflicts, it is advisable for all variables to have the prefix
#' \code{instructor_}.
#'
#' @param instructor_file An optional string giving the location of the
#'   CSV file with additional instructor information.
#'
#' @return If instructor_file is provided, a tibble (data frame)
#' containing instructor information.
#'
#' If \code{instructor_file}
#' is not provided, \code{get_instructors} returns NULL.
#'
#' @export
#'
#' @examples
#' get_instructors()
#' \dontrun{
#' get_instructors("..\data\master\master intructor list.csv")
#' }
get_instructors <- function(instructor_file = NULL){
  if (is.null(instructor_file)){
    instructor <- tibble::tibble(instructor_id = character(0))
  } else {
    if (!file.exists(instructor_file)){
      stop("File does not exist: ", instructor_file)
    }
    instructor <- readr::read_csv(instructor_file)
    if (!is_instructor_data(instructor)){
      stop("Invalid instructor data in ", instructor_file)
    }
  }
  instructor
}
