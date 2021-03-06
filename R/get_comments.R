#' Find and extract comments from the text of an SETC report
#'
#' \code{get_comments} finds and extracts the student comments
#' from the text of an SETC report.
#'
#' \code{get_comments} is normally called by \code{\link{get_setc_data}}
#' but can in principle be called on its own.
#'
#' \code{get_comments} will look in \code{report_text} for the text
#' "Do you have any further comments?"  It will then treat (almost)
#' every line after that as a student comment.
#'
#' If \code{report_text} does not include the text
#' "Do you have any further comments?", \code{get_comments}
#' will return NULL.
#'
#' @param report_text A vector of strings, usually
#' generated by \code{\link{get_report_text}}.
#'
#' @param course_id An optional string that will be
#' treated as the course ID.
#'
#' @return A tibble (data frame) in which each row corresponds to a single
#' line of commentary, and the variables include:
#'   \describe{
#'   \item{\code{course_id}}{The course ID.}
#'   \item{\code{comment_numbers}}{Rows are numbered sequentially.}
#'   \item{\code{comment_text}}{The text of the comment.}
#'   \item{\code{instructor_id}, \code{term_id}, \code{course_number}}{Additional information about the course and instructor.}
#'   }
#'
#' @export
#'
#' @seealso \code{\link{get_setc_data}}, \code{\link{get_report_text}}
#'
get_comments <- function(report_text, course_id = ""){
  comments <- NULL
  comment_appearance <- stringr::str_which(report_text,
                                           "Do you have any further comments?")
  if (length(comment_appearance) > 0) {
    comment_start <- comment_appearance[1] + 2
    if (comment_start <= length(report_text)) {
      tmptext <- report_text[comment_start:length(report_text)]
      tmptext <- tmptext[stringr::str_detect(tmptext, "^\\s")]
      tmptext <- stringr::str_replace(tmptext, "^\\s+", "")
      tmptext <- tmptext[tmptext != "Comments"]
      if (length(tmptext) > 0) {
        comments <- tibble::tibble(course_id = course_id,
                                   comment_number = seq_along(tmptext),
                                   comment_text = tmptext)
      }
    }
  }
  comments
}
