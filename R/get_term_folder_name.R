#' Extract term folder name from the term
#'
#' \code{get_term_folder_name} extracts a term folder name
#' (e.g., "1171 - Spring 2017") from the term (e.g., "Sp17")
#'
#' @param term A string or vector of strings agiving the term (e.g. "Sp17")
#'
#' @return A string or vector of strings giving the associated term
#'   folder (e.g., "1171 - Spring 2017")
#'
#' @export
#'
#' @examples
#' get_term_folder_name("Sp17")
get_term_folder_name <- function(term){
  is_term <- term %>%
    stringr::str_detect("^(Fa|Sp|Su)[0-9]{2}$")
  if (!all(is_term)) {
    stop("Invalid term: ", term)
  }
  season <- stringr::str_sub(term, 1, 2)
  yr <- stringr::str_sub(term, 3, 4)
  season_long <- stringr::str_replace_all(season,
                                          c("Fa" = "Fall",
                                            "Sp" = "Spring",
                                            "Su" = "Summer"))
  season_code <- stringr::str_replace_all(season,
                                          c("Fa" = "7",
                                            "Sp" = "1",
                                            "Su" = "4"))
  stringr::str_c("1",
                 yr,
                 season_code,
                 " - ",
                 season_long,
                 " ",
                 "20",
                 yr)
}
