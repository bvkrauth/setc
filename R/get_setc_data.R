#' Title
#'
#' @param term A string, either "Sp", "Su", or "Fa"
#' @param term_folder The folder containing SETC reports
#' @param output_folder The folder in which you want output to be written
#' @param instructor_file filename
#' @param question_file Optional question file
#'
#' @return
#' @export
#'
#' @examples
get_setc_data <- function(term,
                              term_folder = NULL,
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
  ## CREATE COURSE-LEVEL TIBBLE
  # Each course has a PDF file in report_name, so that is how we build the list
  course <- tibble::tibble(course_id     = get_course_id(report_name, term = term),
                    instructor_id = get_instructor_id(report_name, term = term),
                    term_id       = get_term_id(term),
                    course_number = get_course_number(report_name, term = term),
                    report_name   = report_name,
                    invited       = NA,
                    responded     = NA)
  number_of_courses <- nrow(course)
  ###### CREATE COURSE-AND-QUESTION-LEVEL TIBBLE
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
      # APPEARANCE #1: summary statistics (rounded mean if < 5 responses, mean/sd/n otherwise)
      # We will only need these if the full distribution is suppressed (< 5 responses)
      if (is.na(course$responded[i])) {
        question_and_course$score_mean[iq] <- get_score_mean(report_text, question = question[q, ])
      }
      # APPEARANCE #2: the full distribution of scores (only if >=5 responses)
      # We will get this distribution
      if (length(question_appearance) > 1) {
        score_distribution <- get_score_distribution(report_text, question[q, ])
        question_and_course$score_1[iq] <- score_distribution[1]
        question_and_course$score_2[iq] <- score_distribution[2]
        question_and_course$score_3[iq] <- score_distribution[3]
        question_and_course$score_4[iq] <- score_distribution[4]
        question_and_course$score_5[iq] <- score_distribution[5]
      }
      # APPEARANCE #3 (for some questions): comparison scores from dept/faculty/SFU
      # We won't use these as we have this information elsewhere
    }
    # Print out some information about what we've found
    cat(course$course_id[i], course$course_number[i], course$instructor_id[i],course$invited[i],course$responded[i], "\n")
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
