#' Extract SETC data from a collection of SETC reports.
#'
#' \code{get_setc_data} extracts course-level SETC results and
#' comments from a set of PDF files containing SETC reports.
#'
#' SETC reports are PDF files that give various information
#' about student responses to various questions, as well as
#' more open-ended comments. The files are named according
#' to a naming convention that allows us to determine which
#' course they are associated with, and have a pretetermined
#' format that allows us to extract the data from the PDF
#' files.  Both the file naming convention and the file
#' format can vary somewhat across terms, so the \code{term}
#' argument is mandatory and all of the files in
#' \code{term_folder} must be from that term.  Use
#' \code{\link{pool_setc_data}} to pool across multiple terms.
#'
#' The output has two components.  The course-level data set
#' (\code{course}) includes student responses to the
#' multiple choice questions. The multiple choice questions
#' to be included can be provided by the user in \code{question_file}.
#' If a user-supplied question file is not provided, a default
#' question file based on SFU and FASS-level will be used.
#' Basic instructor information is extracted from the file,
#' but you can also merge in additional instructor information
#' in \code{instructor_file}.
#'
#' The second data set \code{comments} provides a list of student
#' comments, along with basic information on the course each
#' comment is from.
#'
#' @param term A 4-character string indicating the term, e.g. "Sp17"

#' @param term_folder A string giving the folder containing SETC reports

#' @param output_folder Optional string, the folder in which you want output to be written.
#'   If not provided, no output will be written

#' @param instructor_file Optional string, the location of a file providing
#'   additional instructor information to be passed on to \code{\link{get_instructors}}.

#' @param question_file Optional string, the location of a file containing
#'   additional question information to be passed on to \code{\link{get_questions}}
#'
#' @return A list with two components:
#'
#'   \code{course}, a tibble (data frame) in which each row is a course
#'   identified by the variable \code{course_id}. Other variables include
#'   course information, instructor information
#'   (including \code{instructor_id}), and student responses to SETC
#'   multiple choice questions.
#'
#'   \code{comments}, a tibble (data frame) in which each row is
#'   a student comment.  In addition to the comment text, additional
#'   variables provide basic information on the course
#'   (including \code{course_id}) and instructor
#'   (including \code{instructor_id})
#'
#' @export
#'
#' @seealso \code{\link{get_instructors}}, \code{\link{get_questions}},
#'   \code{\link{pool_setc_data}}.
#'
#' @examples
#' \dontrun{
#' get_setc_data("Sp17", "../SETC reports/1171 - Spring 2017")
#' }
get_setc_data <- function(term,
                              term_folder,
                              output_folder = NULL,
                              question_file = NULL,
                              instructor_file = NULL) {
  # Indicate where course-level SETC reports are
  report_file <- dir(path = term_folder, pattern = "*.pdf", full.names = TRUE)
  report_name <- basename(report_file)
  # Read from master instructor list
  instructor <- get_instructors(instructor_file = instructor_file)
  # Read from master question list
  question <- get_questions(question_file = question_file)
  number_of_questions <- nrow(question)
  # Create course-level tibble
  # Each course has a PDF file in report_name, so that is how we build the list
  course <- tibble::tibble(course_id     = get_course_id(report_name, term = term),
                    instructor_id = get_instructor_id(report_name, term = term),
                    term_id       = get_term_id(term),
                    course_number = get_course_number(report_name, term = term),
                    report_name   = report_name,
                    invited       = NA,
                    responded     = NA)
  number_of_courses <- nrow(course)
  # Create course-and-question-level tibble
  # Make list of all course-and-question combinations
  question_and_course <- tibble::tibble( course_id     = rep(course$course_id, each = number_of_questions),
                                 question_id   = rep(question$question_id, times = number_of_courses),
                                 question_text = rep(question$question_text, times = number_of_courses),
                                 score_mean    = NA,
                                 score_sd      = NA,
                                 score_median  = NA,
                                 score_n       = NA,
                                 score_1       = NA,
                                 score_2       = NA,
                                 score_3       = NA,
                                 score_4       = NA,
                                 score_5       = NA)
  comments <- tibble::tibble(course_id = character(0),
                      comment_row = integer(0),
                      comment_text = character(0))
  # Loop over all of the courses (files)
  for (i in 1:number_of_courses) {
    # Get full text from PDF file
    report_text <- report_file[i] %>%
      get_report_text()
    #Get comments, if they exist
    current_comments <- get_comments(report_text, course_id = course$course_id[i])
    comments <- rbind(comments, current_comments)
    # Find response rate information in file, if it exists
    course$responded[i] <- report_text %>%
      get_response_rate("Responded")
    course$invited[i] <- report_text %>%
      get_response_rate("Invited")
    # Loop over all of the questions
    for (q in 1:number_of_questions) {
      iq <- number_of_questions * (i - 1) + q
      # Find all lines in which the question appears in the text
      question_appearance <- report_text %>%
        stringr::str_which(question$question_text[q])
      # Each question appears one to three times
      # FIRST APPEARANCE: summary statistics (rounded mean if < 5 responses, mean/sd/n otherwise)
      # We will only need these if the full distribution is suppressed (< 5 responses)
      if (is.na(course$responded[i])) {
        question_and_course$score_mean[iq] <- get_score_mean(report_text, question = question[q, ])
      }
      # SECOND APPEARANCE: the full distribution of scores (only if >=5 responses)
      # We will get this distribution
      if (length(question_appearance) > 1) {
        score_distribution <- get_score_distribution(report_text, question[q, ])
        question_and_course$score_1[iq] <- score_distribution[1]
        question_and_course$score_2[iq] <- score_distribution[2]
        question_and_course$score_3[iq] <- score_distribution[3]
        question_and_course$score_4[iq] <- score_distribution[4]
        question_and_course$score_5[iq] <- score_distribution[5]
      }
      # THIRD APPEARANCE (for some questions): comparison scores from dept/faculty/SFU
      # We won't use these as we have this information elsewhere
    }
    # Print out some information about what we've found
    cat(course$course_id[i], course$course_number[i], course$instructor_id[i], course$invited[i], course$responded[i], "\n")
  }
  # Extract summary statistics from the detailed distribution
  # score_n is total number of observations
  question_and_course$score_n <- setc_n(question_and_course)
  # score_mean is average score
  tmpmean <- setc_mean(question_and_course)
  # The calculated mean is more precise than the reported mean, so replace as long as it is not NA
  question_and_course$score_mean[!is.na(tmpmean)] <- tmpmean[!is.na(tmpmean)]
  # score_sd is standard deviation of scores
  question_and_course$score_sd <- setc_sd(question_and_course)
  # score_median is median of scores
  question_and_course$score_median <- setc_median(question_and_course)
  question_and_course <- question_and_course %>%
    dplyr::mutate(term_id = get_term_id(course_id = course_id)) %>%
    dplyr::left_join(dplyr::select(question, question_id, score_legend), by = "question_id")
  course <- course %>%
    dplyr::left_join(instructor, by = "instructor_id") %>%
    dplyr::left_join(setc_spread(question_and_course, "mean"), by = "course_id") %>%
    dplyr::left_join(setc_spread(question_and_course, "n"), by = "course_id") %>%
    dplyr::left_join(setc_spread(question_and_course, "legend"), by = "course_id") %>%
    dplyr::mutate(course_section = stringr::str_sub(course_id, 8, 9)) %>%
    dplyr::mutate(season = stringr::str_sub(course_id, 10, 11)) %>%
    dplyr::mutate(year = 2000 + as.numeric(stringr::str_sub(course_id, 12, 13))) %>%
    dplyr::mutate(academic_year = year - as.numeric(season != "Fa"))
  comments <- comments %>%
    dplyr::left_join(dplyr::select(course, course_id, instructor_id, term_id, course_number), by = "course_id") %>%
    dplyr::mutate(course_number = get_course_number(course_id = course_id))
  # Export the data to CSV files
  if (!is.null(output_folder)) {
    course %>%
      readr::write_excel_csv(stringr::str_c(output_folder, term, "_course.csv"))
    comments %>%
      readr::write_excel_csv(stringr::str_c(output_folder, term, "_comments.csv"))
  }
  invisible(list(course, comments))
}
