#' Read in additional question information as a tibble
#'
#' \code{get_questions} assembles a tibble containing information on
#' SETC multiple choice questions for use by \code{\link{get_setc_data}}.
#'
#' If called with no arguments, \code{get_questions} will assemble
#' a default list of questions that includes basic information on
#' all SFU and FASS-level SETC multiple choice questions used
#' through the Spring 2019 term.
#'
#' The \code{question_file} option can be used to include additional
#' department-level or faculty-level (if not FASS) questions,
#' or additional information on questions.  The file should
#' be a CSV file in which the first row gives variable names
#' and each additional row corresponds to a question.  Questions
#' are to be identified by the variable \code{question_id};
#' additional required variables are:
#'
#' \code{question_text} The text of the question as written in SETC.
#' This will be used to find the question in the SETC document.
#'
#' \code{table_type} Either "grouped" or "separate" depending on how
#' answers to this question are reported.  "grouped" means they
#' are reported in a table with other questions, "separate"
#' means they are reported separately.
#'
#' \code{label_1}, \code{label_2},...\code{label_5} The labels
#' associated with each level of the answer, e.g. "Strongly disagree"
#'
#' @param question_file Optional string, the name of the file containing
#'   additional question information.  If not provided, a default
#'   list of questions will be provided.
#'
#' @return A tibble (data frame) in which each row corresponds to
#'  a SETC multiple-choice question. Questions are identified by
#'  the variable \code{question_id}.
#'
#' @export
#'
#' @seealso \code{\link{get_instructors}}, \code{\link{get_setc_data}}.
#'
#' @examples
#' get_questions()
#' \dontrun{
#' get_questions("..\data\master\master question list.csv")
#' }
get_questions <- function(question_file = NULL){
  if (is.null(question_file)) {
    question <- get_default_questions()
  } else {
    question <- readr::read_csv(question_file)
  }
  if (!is_question_data(question)){
    stop("Invalid question data in ", question_file)
  }
  question$score_legend <- stringr::str_c(question$label_1, question$label_2, question$label_3, question$label_4, question$label_5, sep = " | ")
  question
}

get_default_questions <- function(){
  structure(list(question_id = c("ACTI", "APPR", "ASES", "ATND",
                                 "CONC", "CONN", "CRIT", "EASY", "ENVI", "EXPT", "GDCT", "MORE",
                                 "MTRL", "QUAL", "THME"), question_text = c("Course activities",
                                                                            "The course instructor was approachable when students asked for guidance",
                                                                              "The assessments in this course", "I attended class", "The course instructor explained course concepts clearly",
                                                                              "The different course parts/activities", "My experience in this course has encouraged me to think critically",
                                                                              "How easy was this course", "The course instructor created a respectful learning environment",
                                                                              "The course instructor made it clear what students were expected to learn",
                                                                              "The course instructor explained grading criteria clearly", "My experience in this course has motivated me to learn more about the subject",
                                                                              "Course materials", "Overall, the quality of my learning experience in this course was",
                                                                              "When explaining course concepts, the instructor connected the concepts to the major themes"
                                   ), question_level = c("SFU", "SFU", "SFU", "SFU", "SFU", "SFU",
                                                         "FASS", "SFU", "SFU", "FASS", "SFU", "FASS", "SFU", "SFU", "FASS"
                                   ), table_type = c("grouped", "grouped", "grouped", "separate",
                                                     "grouped", "grouped", "separate", "separate", "grouped", "separate",
                                                     "grouped", "separate", "grouped", "separate", "separate"), fass_mean = c(3.85,
                                                                                                                              4.31, 3.98, 4.5, 4.08, 4.16, 4, 2.75, 4.41, 4.14, 4.07, 3.66,
                                                                                                                              4.09, 3.92, 4.27), sfu_mean = c(3.92, 4.34, 3.98, 4.57, 4.07,
                                                                                                                                                              4.18, NA, 2.69, 4.41, NA, 4.08, NA, 4.04, 3.93, NA), label_1 = c("Strongly Disagree",
                                                                                                                                                                                                                               "Strongly Disagree", "Strongly Disagree", "Never", "Strongly Disagree",
                                                                                                                                                                                                                               "Strongly Disagree", "Strongly Disagree", "Very Hard", "Strongly Disagree",
                                                                                                                                                                                                                               "Strongly Disagree", "Strongly Disagree", "Strongly Disagree",
                                                                                                                                                                                                                               "Strongly Disagree", "Very Poor", "Strongly Disagree"), label_2 = c("Disagree",
                                                                                                                                                                                                                                                                                                   "Disagree", "Disagree", "Rarely", "Disagree", "Disagree", "Disagree",
                                                                                                                                                                                                                                                                                                   "Hard", "Disagree", "Disagree", "Disagree", "Disagree", "Disagree",
                                                                                                                                                                                                                                                                                                   "Poor", "Disagree"), label_3 = c("No Opinion", "No Opinion",
                                                                                                                                                                                                                                                                                                                                    "No Opinion", "About half of the time", "No Opinion", "No Opinion",
                                                                                                                                                                                                                                                                                                                                    "No Opinion", "Medium", "No Opinion", "No Opinion", "No Opinion",
                                                                                                                                                                                                                                                                                                                                    "No Opinion", "No Opinion", "Fair", "No Opinion"), label_4 = c("Agree",
                                                                                                                                                                                                                                                                                                                                                                                                   "Agree", "Agree", "Most of the time", "Agree", "Agree", "Agree",
                                                                                                                                                                                                                                                                                                                                                                                                   "Easy", "Agree", "Agree", "Agree", "Agree", "Agree", "Good",
                                                                                                                                                                                                                                                                                                                                                                                                   "Agree"), label_5 = c("Strongly Agree", "Strongly Agree", "Strongly Agree",
                                                                                                                                                                                                                                                                                                                                                                                                                         "All of the time", "Strongly Agree", "Strongly Agree", "Strongly Agree",
                                                                                                                                                                                                                                                                                                                                                                                                                         "Very Easy", "Strongly Agree", "Strongly Agree", "Strongly Agree",
                                                                                                                                                                                                                                                                                                                                                                                                                         "Strongly Agree", "Strongly Agree", "Very Good", "Strongly Agree"
                                                                                                                                                                                                                                                                                                                                                                                                   ), question_note = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                                                                                                                                                                                                                                                                                                                                                                                                        NA, NA, NA, "Test note for questions", NA), score_legend = c("Strongly Disagree | Disagree | No Opinion | Agree | Strongly Agree",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "Strongly Disagree | Disagree | No Opinion | Agree | Strongly Agree",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "Strongly Disagree | Disagree | No Opinion | Agree | Strongly Agree",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "Never | Rarely | About half of the time | Most of the time | All of the time",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "Strongly Disagree | Disagree | No Opinion | Agree | Strongly Agree",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "Strongly Disagree | Disagree | No Opinion | Agree | Strongly Agree",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "Strongly Disagree | Disagree | No Opinion | Agree | Strongly Agree",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "Very Hard | Hard | Medium | Easy | Very Easy", "Strongly Disagree | Disagree | No Opinion | Agree | Strongly Agree",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "Strongly Disagree | Disagree | No Opinion | Agree | Strongly Agree",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "Strongly Disagree | Disagree | No Opinion | Agree | Strongly Agree",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "Strongly Disagree | Disagree | No Opinion | Agree | Strongly Agree",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "Strongly Disagree | Disagree | No Opinion | Agree | Strongly Agree",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "Very Poor | Poor | Fair | Good | Very Good", "Strongly Disagree | Disagree | No Opinion | Agree | Strongly Agree"
                                                                                                                                                                                                                                                                                                                                                                                                                        )), row.names = c(NA, -15L), class = c("tbl_df", "tbl", "data.frame"
                                                                                                                                                                                                                                                                                                                                                                                                                        ))


}
